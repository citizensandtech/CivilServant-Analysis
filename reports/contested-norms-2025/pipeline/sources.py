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
    from app.models import Post, Comment, ModAction, Subreddit
except ImportError:
    pass

### LOAD PRAW
import configparser
import time
import pickle
import praw
from praw.errors import NotFound

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
                 database_config=None,
                 ignore=None
                ):
        self.subreddit_id = subreddit_id
        self._subreddit_name = None
        self.start_time = start_time
        self.end_time = end_time
        self.database_config = database_config
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
        return "{}-posts".format(self.subreddit_id)
    
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
        return "{}-comments".format(self.subreddit_id)

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
        super().__init__(*args, **kwargs)
        
    def label(self):
        return "{}-modactions".format(self.subreddit_id)

    def columns(self):
        return [
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

    def column_types(self):
        return {
            'id': str,
            'created.utc': float,
            'created.at': float,
            'subreddit.id': str,
            'action': str,
            'mod': str,
            'target.author': str,
            'target.fullname': str,
            'details': str,
            'description': str
        }

    def set_actions(self, actions):
        self.actions = actions
        return self
    
    def extract(self):
        self.logger.info("Extracting mod actions")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        for action in self.actions:
            self.logger.info("  Extracting {} actions".format(action))

            next_start_time = self.start_time
            total_hours = int(math.ceil((self.end_time - self.start_time).total_seconds() / 3600))

            count = 0
            last_log = None
            progress_start = time.time()
            progress = tqdm(total=total_hours)
            while next_start_time < self.end_time:
                start_time = next_start_time
                next_start_time = start_time + timedelta(hours=1)

                # Unlike posts and comments, created_utc is indexed so can just query on that
                filter = and_(
                    ModAction.subreddit_id == self.subreddit_id,
                    ModAction.created_utc >= start_time,
                    ModAction.created_utc < min(next_start_time, self.end_time),
                    ModAction.action == action)                
                modactions = self.db().query(ModAction).filter(filter)
                for modaction in modactions:
                    action_data = json.loads(modaction.action_data)
                    datum = {
                        'id': modaction.id,
                        'created.utc': utc.localize(modaction.created_utc).timestamp(),
                        'created.at': utc.localize(modaction.created_at).timestamp(),
                        'subreddit.id': modaction.subreddit_id,
                        'action': modaction.action,
                        'mod': action_data['mod'],
                        'target.author': modaction.target_author,
                        'target.fullname': action_data['target_fullname'],
                        'details': action_data['details'],
                        'description': action_data['description']
                    }
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
                    
            progress.close()
            self.logger.info("    Extracted {} actions".format(count))
                
        self.logger.info("  Done")

class PRAWPostSource(DataSource):
    def __init__(self, subreddit_id, start_time, end_time, reddit=None, things=[], delay_s=2):
        self.start_time = start_time
        self.end_time = end_time
        self.subreddit_id = subreddit_id
        self.reddit = reddit
        self.things = list(things)
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.delay_s = delay_s
        
    def label(self):
        return "{}-praw_posts".format(self.subreddit_id)
    
    def columns(self):
        return [
            "id",
            "fullname",
            "created.utc",
            "created.at",
            "subreddit.id",
            "author",
            "author.fullname",
            "selftext"
        ]
        
    def column_types(self):
        return {
            "id": str,
            "fullname": str,
            "created.utc": float,
            "created.at": float,
            "subreddit.id": str,
            "author": str,
            "author.fullname": str,
            "selftext": str
        }
        
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
                result = {
                    'id': thing.id,
                    'fullname': thing.name,
                    'created.utc': thing.created_utc,
                    'created.at': None,
                    'subreddit.id': thing.subreddit_id,
                    'selftext': thing.selftext
                }
                try:
                    result['author'] = thing.author.name
                except AttributeError:
                    result['author'] = None
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
    def __init__(self, subreddit_id, start_time, end_time, reddit=None, things=[], delay_s=2):
        self.start_time = start_time
        self.end_time = end_time
        self.subreddit_id = subreddit_id
        self.reddit = reddit
        self.things = list(things)
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.delay_s = delay_s
        self.resume_by_id = None
        
    def label(self):
        return "{}-praw_comments".format(self.subreddit_id)
    
    def columns(self):
        return [
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
        
    def column_types(self):
        return {
            'id': str,
            'fullname': str,
            'created.utc': float,
            'created.at': float,
            'link.id': str,
            'subreddit.id': str,
            'author': str,
            'author.fullname': str,
            'is.submitter': bool,
            'body': str
        }

    def resume(self, rows):
        self.resume_by_id = dict([(row['id'], row) for row in rows])
        
    def extract(self):
        self.logger.info("Extracting Reddit PRAW comments")

        query_ids = []
        if self.resume_by_id is None:
            query_fullnames = self.things
        else:
            for thing in self.things:
                thing_id = thing.replace('t1_', '')
                if selfthing_id not in self.resume_by_id:
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
                        continue
                    if thing.subreddit_id.replace('t5_', '') != self.subreddit_id:
                        skipped_subreddit += 1
                        continue
                    result = {
                        'id': thing.id,
                        'fullname': thing.name,
                        'created.utc': thing.created_utc,
                        'created.at': None,
                        'link.id': thing.link_id,
                        'subreddit.id': thing.subreddit_id,
                        'is.submitter': thing.is_submitter,
                        'body': thing.body
                    }
                    try:
                        result['author'] = thing.author.name
                    except AttributeError:
                        result['author'] = None
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
        if self.resume_by_id is not None:
            self.logger.info("  Re-used {} rows from resumed dataset".format(resumed))
        self.logger.info("  Skipped {} rows out of date range".format(skipped))
        self.logger.info("  Skipped {} rows not matching subreddit_id".format(skipped_subreddit))
        