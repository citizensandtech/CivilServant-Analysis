from datetime import datetime, timedelta
import csv
import json
import logging
import math
import os
import pytz
import time
from tqdm import tqdm

### LOAD SQLALCHEMY
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy import text, and_, or_
import sqlalchemy.orm.session

### LOAD CIVILSERVANT
try:
    from app.models import Comment, FrontPage, ModAction, Post, Subreddit
except ImportError:
    pass

### LOAD PRAW
import configparser
import time
import pickle
import praw
try:
    from praw.errors import NotFound
except ModuleNotFoundError:
    from prawcore.exceptions import NotFound

utc = pytz.UTC

class DataSource:
    """Base class for data sources used to extract and validate DataFiles.""" 
    def label(self):
        raise NotImplementedError
    
    def column_types(self):
        raise NotImplementedError

    def extract(self):
        raise NotImplementedError
        
class CivilServantSource(DataSource):
    def __init__(self,
                 subreddit_id,
                 start_time,
                 end_time,
                 prefix=None,
                 database_config=None,
                 ignore=None
                ):
        self.subreddit_id = subreddit_id
        self._subreddit_name = None
        self.start_time = start_time
        self.end_time = end_time
        self.database_config = database_config
        self.prefix = prefix
        if ignore is None:
            self.ignore = []
        else:
            self.ignore = ignore
        self._db = None
        self.logger = logging.getLogger('CivilServant-Analysis')

    def db(self):
        if self._db is None:
            db_engine = create_engine("mysql://{user}:{password}@{host}/{database}".format(
                host = self.database_config['host'],
                user = self.database_config['user'],
                password = self.database_config['password'],
                database = self.database_config['database']))
            DBSession = sessionmaker(bind=db_engine)
            self._db = DBSession()
        return self._db


class CivilServantPostSource(CivilServantSource):
    def label(self):
        parts = []
        parts.append(self.subreddit_id)
        if self.prefix is not None:
            parts.append("prefix")
        parts.append("posts")
        return "-".join(parts)
    
    def columns(self):
        return [
            field for field in [
                "id",
                "fullname",
                "created.utc",
                "created.at",
                "subreddit.id",
                "author",
                "author.fullname",
                "link.flair.text",
                "selftext"
            ]
            if field not in self.ignore
        ]

    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ("id", str),
                ("fullname", str),
                ("created.utc", float),
                ("created.at", float),
                ("subreddit.id", str),
                ("author", str),
                ("author.fullname", str),
                ("link.flair.text", str),
                ("selftext", str)
            ]
            if field not in self.ignore
        )

    def extract(self):
        self.logger.info("Extracting posts")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        # created_at is indexed, so query on that rather than created_utc
        # one day is added to end_time to catch cases where created_utc is
        # within the time period but created_at is after
        filter = and_(
            Post.subreddit_id == self.subreddit_id,
            Post.created_at >= self.start_time,
            Post.created_at < self.end_time + timedelta(days=1))

        total_posts = self.db().query(Post).filter(filter).count()
        self.logger.info("  Querying {} records".format(total_posts))
        
        next_start_time = self.start_time
        total_hours = int(math.ceil((self.end_time - self.start_time).total_seconds() / 3600))

        count = 0
        skipped = 0
        progress_start = time.time()
        progress = tqdm(total=total_hours)
        last_log = None
        while next_start_time < self.end_time + timedelta(days=1):
            start_time = next_start_time
            next_start_time = start_time + timedelta(hours=1)

            posts = self.db().query(Post).filter(and_(
                Post.subreddit_id == self.subreddit_id,
                Post.created_at >= start_time,
                Post.created_at < next_start_time))

            for post in posts:
                post_data = json.loads(post.post_data)
                created_utc = utc.localize(datetime.utcfromtimestamp(post_data['created_utc']))
                if created_utc > self.end_time:
                    # Becasue an extra day is included at the end of the query,
                    # some records need to be filtered out if created_utc is outside
                    # the observation period
                    skipped += 1
                    continue
                datum = {}
                if 'id' not in self.ignore:
                    datum['id'] = post.id
                if 'fullname' not in self.ignore:
                    datum['fullname'] = post_data['name']
                if 'created.utc' not in self.ignore:
                    datum['created.utc'] = post_data['created_utc']
                if 'created.at' not in self.ignore:
                    datum['created.at'] = utc.localize(post.created_at).timestamp()
                if 'subreddit.id' not in self.ignore:
                    datum['subreddit.id'] = post.subreddit_id
                if 'author' not in self.ignore:
                    datum['author'] = post_data['author']
                if 'author.fullname' not in self.ignore:
                    datum['author.fullname'] = post_data.get('author_fullname', '')
                if 'link.flair.text' not in self.ignore:
                    datum['link.flair.text'] = post_data.get('link_flair_text', '')
                if 'selftext' not in self.ignore:
                    datum['selftext'] = post_data['selftext']

                count += 1
                yield datum

            progress.update()
            progress_time = time.time() - progress_start
            if (last_log is None
                or progress_time - last_log > 300
                or next_start_time >= self.end_time + timedelta(days=1)
            ):
                self.logger.debug("    {} complete in {} seconds".format(count, progress_time))
                last_log = progress_time
                progress.refresh()
                
        progress.close()
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        self.logger.info("  Skipped {} outside observation period".format(skipped))

class CivilServantCommentSource(CivilServantSource):
    
    def label(self):
        parts = []
        parts.append(self.subreddit_id)
        if self.prefix is not None:
            parts.append("prefix")
        parts.append("comments")
        return "-".join(parts)

    def columns(self):
        return [
            field for field in [
                'id',
                'fullname',
                'created.utc',
                'created.at',
                'link.id',
                'parent.id',
                'subreddit.id',
                'author',
                'author.fullname',
                'is.submitter',
                'body'
            ]
            if field not in self.ignore
        ]
    
    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ('id', str),
                ('fullname', str),
                ('created.utc', float),
                ('created.at', float),
                ('link.id', str),
                ('parent.id', str),
                ('subreddit.id', str),
                ('author', str),
                ('author.fullname', str),
                ('is.submitter', bool),
                ('body', str)
            ]
            if field not in self.ignore
        )
    
    def extract(self):
        self.logger.info("Extracting comments")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))
            
        # created_at is indexed, so query on that rather than created_utc
        # one day is added to end_time to catch cases where created_utc is
        # within the time period but created_at is after
        filter = and_(
                Comment.subreddit_id == self.subreddit_id,
                Comment.created_at >= self.start_time,
                Comment.created_at < self.end_time + timedelta(days=1))

        total_comments = self.db().query(Comment).filter(filter).count()
        self.logger.info("  Querying {} records".format(total_comments))
        
        next_start_time = self.start_time
        total_hours = int(math.ceil((self.end_time - self.start_time).total_seconds() / 3600))

        count = 0
        skipped = 0
        progress_start = time.time()
        progress = tqdm(total=total_hours)
        last_log = None
        while next_start_time < self.end_time + timedelta(days=1):
            start_time = next_start_time
            next_start_time = start_time + timedelta(hours=1)

            comments = self.db().query(Comment).filter(and_(
                Comment.subreddit_id == self.subreddit_id,
                Comment.created_at >= start_time,
                Comment.created_at < next_start_time))
            
            for comment in comments:
                comment_data = json.loads(comment.comment_data)
                created_utc = utc.localize(datetime.utcfromtimestamp(comment_data['created_utc']))
                if created_utc > self.end_time:
                    # Becasue an extra day is included at the end of the query,
                    # some records need to be filtered out if created_utc is outside
                    # the observation period
                    skipped += 1
                    continue
                datum = {}
                if 'id' not in self.ignore:
                    datum['id'] = comment.id
                if 'fullname' not in self.ignore:
                    datum['fullname'] = comment_data['name']
                if 'created.utc' not in self.ignore:
                    datum['created.utc'] = utc.localize(comment.created_utc).timestamp()
                if 'created.at' not in self.ignore:
                    datum['created.at'] = utc.localize(comment.created_at).timestamp()
                if 'link.id' not in self.ignore:
                    datum['link.id'] = comment_data['link_id']
                if 'parent.id' not in self.ignore:
                    datum['parent.id'] = comment_data['parent_id']
                if 'subreddit.id' not in self.ignore:
                    datum['subreddit.id'] = comment.subreddit_id
                if 'author' not in self.ignore:
                    datum['author'] = comment_data['author']
                if 'author.fullname' not in self.ignore:
                    datum['author.fullname'] = comment_data.get('author_fullname')
                if 'is.submitter' not in self.ignore:
                    datum['is.submitter'] = comment_data['is_submitter']
                if 'body' not in self.ignore:
                    datum['body'] = comment_data['body']
                    
                yield datum
                count += 1

            progress.update()
            progress_time = time.time() - progress_start
            if (last_log is None
                or progress_time - last_log > 300
                or next_start_time >= self.end_time + timedelta(days=1)
            ):
                self.logger.debug("    {} complete in {} seconds".format(count, progress_time))
                last_log = progress_time
                progress.refresh()
                
        progress.close()
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        self.logger.info("  Skipped {} outside observation period".format(skipped))

class CivilServantModActionSource(CivilServantSource):
    def __init__(self, *args, **kwargs):
        self.actions = [
            "removelink",
            "spamlink",
            "approvelink",
            "removecomment",
            "spamcomment",
            "approvecomment",
            "banuser",
            "unbanuser"
        ]
        self.resume_by_action = None
        super().__init__(*args, **kwargs)
        
    def label(self):
        return "{}-modactions".format(self.subreddit_id)

    def columns(self):
        return [
            field for field in [
                'id',
                'created.utc',
                'created.at',
                'subreddit.id',
                'action',
                'mod',
                'target.author',
                'target.fullname',
                'details',
                'description'
            ]
            if field not in self.ignore
        ]

    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ('id', str),
                ('created.utc', float),
                ('created.at', float),
                ('subreddit.id', str),
                ('action', str),
                ('mod', str),
                ('target.author', str),
                ('target.fullname', str),
                ('details', str),
                ('description', str)
            ]
            if field not in self.ignore
        )

    def set_actions(self, actions):
        self.actions = actions
        return self

    def resume(self, loaded_rows):
        # Record which actions and ids exist in the data set being resumed
        completed_actions = []
        self.resume_by_action = dict()
        self.completed_ids = set()
        for row in loaded_rows:
            self.completed_ids.add(row['id'])
            action = row['action']
            if action not in self.resume_by_action:
                self.resume_by_action[action] = []
            self.resume_by_action[action].append(row)
        self.resume_action = loaded_rows[-1]['action']
    
    def extract(self):
        self.logger.info("Extracting mod actions")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        for action in self.actions:

            self.logger.info("  Extracting {} actions".format(action))
            resumed_count = 0
            last_resumed_time = None
            
            # Check whether we are resuming a dataset that extracted any of this action
            if self.resume_by_action is not None and action in self.resume_by_action:
                for row in self.resume_by_action[action]:
                    resumed_count += 1
                    last_resumed_time = row['created.utc']
                    yield row
                self.logger.info("    Used {} records from resumed dataset".format(resumed_count))
                if action != self.resume_action:
                    # This action was fully extracted in the resumed dataset
                    continue
                else:
                    # This was the last action extracted in the resumed dataset
                    # Extraction may not have been complete
                    pass

            next_start_time = self.start_time
            total_hours = int(math.ceil((self.end_time - self.start_time).total_seconds() / 3600))

            count = 0
            duplicate_count = 0
            last_log = None
            progress_start = time.time()
            progress = tqdm(total=total_hours)
            while next_start_time < self.end_time:
                start_time = next_start_time
                next_start_time = start_time + timedelta(hours=1)

                # Check whether we are resuming and this time period has already been extracted
                if last_resumed_time is not None:
                    if next_start_time.timestamp() < last_resumed_time:
                        progress.update()
                        continue
                
                # Unlike posts and comments, created_utc is indexed so can just query on that
                filter = and_(
                    ModAction.subreddit_id == self.subreddit_id,
                    ModAction.created_utc >= start_time,
                    ModAction.created_utc < min(next_start_time, self.end_time),
                    ModAction.action == action)                
                modactions = self.db().query(ModAction).filter(filter)
                for modaction in modactions:
                    action_data = json.loads(modaction.action_data)
                    datum = {}
                    if 'id' not in self.ignore:
                        datum['id'] = modaction.id
                    if 'created.utc' not in self.ignore:
                        datum['created.utc'] = utc.localize(modaction.created_utc).timestamp()
                    if 'created.at' not in self.ignore:
                        datum['created.at'] = utc.localize(modaction.created_at).timestamp()
                    if 'subreddit.id' not in self.ignore:
                        datum['subreddit.id'] = modaction.subreddit_id
                    if 'action' not in self.ignore:
                        datum['action'] = modaction.action
                    if 'mod' not in self.ignore:
                        datum['mod'] = action_data['mod']
                    if 'target.author' not in self.ignore:
                        datum['target.author'] = modaction.target_author
                    if 'target.fullname' not in self.ignore:
                        datum['target.fullname'] = action_data['target_fullname']
                    if 'details' not in self.ignore:
                        datum['details'] = action_data['details']
                    if 'description' not in self.ignore:
                        datum['description'] = action_data['description']

                    count += 1
                    if self.resume_by_action is None or datum['id'] not in self.completed_ids:
                        yield datum
                    else:
                        duplicate_count += 1
                    
                progress.update()
                progress_time = time.time() - progress_start
                if (last_log is None
                    or progress_time - last_log > 300
                    or next_start_time >= self.end_time + timedelta(days=1)
                ):
                    self.logger.debug("    {} complete in {} seconds".format(count, progress_time))
                    last_log = progress_time
                    
            progress.close()
            self.logger.info("    Extracted {} actions".format(count))
            if self.resume_by_action is not None:
                self.logger.info("    Used {} records from resumed dataset".format(resumed_count))
                self.logger.info("    Ignored {} re-extracted duplicates".format(duplicate_count))
                
        self.logger.info("  Done")

class CivilServantFrontPageSource(CivilServantSource):
    def label(self):
        return "{}-front_pages".format(self.subreddit_id)
    
    def columns(self):
        return [
            field for field in [
                "id",
                "post.id",
                "created.at",
                "subreddit",
                "page.type",
                "post.rank",
            ]
            if field not in self.ignore
        ]

    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ("id", str),
                ("post.id", str),
                ("created.at", float),
                ("subreddit", str),
                ("page.type", int),
                ("post.rank", int)
            ]
            if field not in self.ignore
        )

    def extract(self):
        self.logger.info("Extracting front page posts")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        filter = and_(
            FrontPage.created_at >= self.start_time,
            FrontPage.created_at < self.end_time)

        total = self.db().query(FrontPage).filter(filter).count()
        self.logger.info("  Querying {} records".format(total))
        
        next_start_time = self.start_time
        total_hours = int(math.ceil((self.end_time - self.start_time).total_seconds() / 3600))

        count = 0
        skipped = 0
        progress_start = time.time()
        progress = tqdm(total=total_hours)
        last_log = None
        while next_start_time < self.end_time:
            start_time = next_start_time
            next_start_time = min(start_time + timedelta(hours=1), self.end_time)

            pages = self.db().query(FrontPage).filter(and_(
                FrontPage.created_at >= start_time,
                FrontPage.created_at < next_start_time))

            for page in pages:
                page_data = json.loads(page.page_data)
                for rank, page_post in enumerate(page_data):
                    post = self.db().query(Post).filter(Post.id == page_post['id']).first()
                    if post is None or post.subreddit_id != self.subreddit_id:
                        continue
                    datum = {}
                    if 'id' not in self.ignore:
                        datum['id'] = page.id
                    if 'post.id' not in self.ignore:
                        datum['post.id'] = page_post['id']
                    if 'created.at' not in self.ignore:
                        datum['created.at'] = utc.localize(page.created_at).timestamp()
                    if 'subreddit' not in self.ignore:
                        datum['subreddit'] = post.subreddit_id
                    if 'post.rank' not in self.ignore:
                        datum['post.rank'] = rank
                    if 'page.type' not in self.ignore:
                        datum['page.type'] = page.page_type
                    count += 1
                    yield datum

            progress.update()
            progress_time = time.time() - progress_start
            if (last_log is None
                or progress_time - last_log > 300
                or next_start_time >= self.end_time + timedelta(days=1)
            ):
                self.logger.debug("    {} complete in {} seconds".format(count, progress_time))
                last_log = progress_time
                progress.refresh()
                
        progress.close()
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        self.logger.info("  Skipped {} outside observation period".format(skipped))

class PRAWPostSource(DataSource):
    def __init__(self, subreddit_id, start_time, end_time, reddit=None, things=[], delay_s=2, ignore=None):
        self.start_time = start_time
        self.end_time = end_time
        self.subreddit_id = subreddit_id
        self.reddit = reddit
        self.things = list(things)
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.delay_s = delay_s
        if ignore is None:
            self.ignore = []
        else:
            self.ignore = ignore
        
    def label(self):
        return "{}-praw_posts".format(self.subreddit_id)
    
    def columns(self):
        return [
            field for field in [
                "id",
                "fullname",
                "created.utc",
                "created.at",
                "subreddit.id",
                "author",
                "author.fullname",
                "selftext"
            ]
            if field not in self.ignore
        ]
        
    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ("id", str),
                ("fullname", str),
                ("created.utc", float),
                ("created.at", float),
                ("subreddit.id", str),
                ("author", str),
                ("author.fullname", str),
                ("selftext", str)
            ]
            if field not in self.ignore
        )
        
    def extract(self):
        self.logger.info("Extracting Reddit PRAW posts")

        count = 0
        skipped = 0
        skipped_subreddit = 0
        pages = math.ceil(len(self.things) / 100)
        progress = tqdm(total=len(self.things))
        for page in range(pages):
            page_ids = self.things[page*100:(page+1)*100]
            response = self.reddit.get_info(thing_id=page_ids)
            for thing in response:
                created_utc = utc.localize(datetime.utcfromtimestamp(thing.created_utc))
                if created_utc < self.start_time or created_utc >= self.end_time:
                    skipped += 1
                    continue
                if thing.subreddit_id.replace('t5_', '') != self.subreddit_id:
                    skipped_subreddit += 1
                    continue
                result = {}
                if 'id' not in self.ignore:
                    result['id'] = thing.id
                if 'fullname' not in self.ignore:
                    result['fullname'] = thing.name
                if 'created.utc' not in self.ignore:
                    result['created.utc'] = thing.created_utc
                if 'created.at' not in self.ignore:
                    result['created.at'] = None,
                if 'subreddit.id' not in self.ignore:
                    result['subreddit.id'] = thing.subreddit_id
                if 'selftext' not in self.ignore:
                    result['selftext'] = thing.selftext
                if 'author' not in self.ignore:
                    try:
                        result['author'] = thing.author.name
                    except AttributeError:
                        result['author'] = None
                if 'author.fullname' not in self.ignore:
                    try:
                        result['author.fullname'] = thing.author.fullname
                    except (AttributeError, NotFound):
                        result['author.fullname'] = None
                yield result
                count += 1
                progress.update()
                time.sleep(self.delay_s)
            
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        self.logger.info("  Skipped {} rows out of date range".format(skipped))
        self.logger.info("  Skipped {} rows not matching subreddit_id".format(skipped_subreddit))

class PRAWCommentSource(DataSource):
    def __init__(self, subreddit_id, start_time, end_time, reddit=None, things=[], delay_s=2, ignore=None):
        self.start_time = start_time
        self.end_time = end_time
        self.subreddit_id = subreddit_id
        self.reddit = reddit
        self.things = list(things)
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.delay_s = delay_s
        self.resume_by_id = None
        if ignore is None:
            self.ignore = []
        else:
            self.ignore = ignore
        
    def label(self):
        return "{}-praw_comments".format(self.subreddit_id)
    
    def columns(self):
        return [
            field for field in [
                'id',
                'fullname',
                'created.utc',
                'created.at',
                'link.id',
                'subreddit.id',
                'author',
                'author.fullname',
                'is.submitter',
                'body'
            ]
            if field not in self.ignore
        ]
        
    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ('id', str),
                ('fullname', str),
                ('created.utc', float),
                ('created.at', float),
                ('link.id', str),
                ('subreddit.id', str),
                ('author', str),
                ('author.fullname', str),
                ('is.submitter', bool),
                ('body', str)
            ]
            if field not in self.ignore
        )

    def resume(self, rows):
        self.resume_by_id = dict([(row['id'], row) for row in rows])
        
    def extract(self):
        self.logger.info("Extracting Reddit PRAW comments")

        query_fullnames = []
        if self.resume_by_id is None:
            query_fullnames = self.things
        else:
            for thing in self.things:
                thing_id = thing.replace('t1_', '')
                if thing_id not in self.resume_by_id:
                    query_fullnames.append(thing)
            self.logger.info("  Resuming and skipping {} previous rows".format(len(self.things) - len(query_fullnames)))
        
        count = 0
        skipped = 0
        skipped_subreddit = 0
        resumed = 0
        page_fullnames = []
        progress = tqdm(total=len(self.things))
        for i, thing in enumerate(self.things):

            if thing in query_fullnames:
                page_fullnames.append(thing)
            else:
                thing_id = thing.replace('t1_', '')
                resumed += 1
                progress.update()
                yield self.resume_by_id[thing_id]

            # If the query page is full or we're out of items, query the API
            if len(page_fullnames) == 100 or i == len(self.things) - 1:
                response = self.reddit.get_info(thing_id=page_fullnames)
                page_fullnames = []
                for thing in response:
                    created_utc = utc.localize(datetime.utcfromtimestamp(thing.created_utc))
                    if created_utc < self.start_time or created_utc >= self.end_time:
                        skipped += 1
                        progress.update()
                        continue
                    if thing.subreddit_id.replace('t5_', '') != self.subreddit_id:
                        skipped_subreddit += 1
                        progress.update()
                        continue
                    result = {}
                    if 'id' not in self.ignore:
                        result['id'] = thing.id
                    if 'fullname' not in self.ignore:
                        result['fullname'] = thing.name
                    if 'created.utc' not in self.ignore:
                        result['created.utc'] = thing.created_utc
                    if 'created.at' not in self.ignore:
                        result['created.at'] = None
                    if 'link.id' not in self.ignore:
                        result['link.id'] = thing.link_id
                    if 'subreddit.id' not in self.ignore:
                        result['subreddit.id'] = thing.subreddit_id
                    if 'is.submitter' not in self.ignore:
                        result['is.submitter'] = thing.is_submitter
                    if 'body' not in self.ignore:
                        result['body'] = thing.body
                    if 'author' not in self.ignore:
                        try:
                            result['author'] = thing.author.name
                        except AttributeError:
                            result['author'] = None
                    if 'author.fullname' not in self.ignore:
                        try:
                            result['author.fullname'] = thing.author.fullname
                        except (AttributeError, NotFound):
                            result['author.fullname'] = None
                    yield result
                    count += 1
                    progress.update()
                    time.sleep(self.delay_s)

        progress.refresh()
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        if self.resume_by_id is not None:
            self.logger.info("  Re-used {} rows from resumed dataset".format(resumed))
        self.logger.info("  Skipped {} rows out of date range".format(skipped))
        self.logger.info("  Skipped {} rows not matching subreddit_id".format(skipped_subreddit))
        