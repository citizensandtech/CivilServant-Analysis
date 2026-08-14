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
    from app.models import Comment, FrontPage, ModAction, Post, Subreddit, ExperimentThing
except ImportError:
    pass

from . import DataSource

utc = pytz.UTC

class CivilServantSource(DataSource):
    def connect(self,
                 subreddit_id,
                 start_time,
                 end_time,
                 prefix=None,
                 database_config=None,
                 exclude_columns=None
                ):
        self.subreddit_id = subreddit_id
        self._subreddit_name = None
        self.start_time = start_time
        self.end_time = end_time
        self.database_config = database_config
        self.prefix = prefix
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.db = None
        self._db_connect()
        self.super().__init__(self, exclude_columns)
        
    def _db_connect(self):
        if self.db is not None:
            self.logger.warning("  Already connected to databse, skipping")
            return
        
        self.logger.info("  Connecting to database: {}".format(self.database_config['database']))
        db_engine = create_engine("mysql://{user}:{password}@{host}/{database}".format(
            host = self.database_config['host'],
            user = self.database_config['user'],
            password = self.database_config['password'],
            database = self.database_config['database']))
        DBSession = sessionmaker(bind=db_engine)
        self.db = DBSession()

    def columns(self) :
        return [
            column for column, column_type
            in self.column_types().items()
            if column not in self.exclude_columns]
        
class CivilServantPostSource(CivilServantSource):
    def label(self):
        parts = []
        parts.append(self.subreddit_id)
        if self.prefix is not None:
            parts.append("prefix")
        parts.append("posts")
        return "-".join(parts)
    
    def column_types(self):
        return {
            'id': str,
            'fullname': str,
            'created.utc': float,
            'created.at': float,
            'subreddit.id': str,
            'author': str,
            'author.fullname': str,
            'link.flair.text': str,
            'selftext': str
        }

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

        total_posts = self.db.query(Post).filter(filter).count()
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

            posts = self.db.query(Post).filter(and_(
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
                datum = self.extract_row(post)
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

    def extract_row(self, post):
        post_data = json.loads(post.post_data)
        created_utc = utc.localize(datetime.utcfromtimestamp(post_data['created_utc']))
        datum = {}
        if 'id' not in self.exclude_columns:
            datum['id'] = post.id
        if 'fullname' not in self.exclude_columns:
            datum['fullname'] = post_data['name']
        if 'created.utc' not in self.exclude_columns:
            datum['created.utc'] = post_data['created_utc']
        if 'created.at' not in self.exclude_columns:
            datum['created.at'] = utc.localize(post.created_at).timestamp()
        if 'subreddit.id' not in self.exclude_columns:
            datum['subreddit.id'] = post.subreddit_id
        if 'author' not in self.exclude_columns:
            datum['author'] = post_data['author']
        if 'author.fullname' not in self.exclude_columns:
            datum['author.fullname'] = post_data.get('author_fullname', '')
        if 'link.flair.text' not in self.exclude_columns:
            datum['link.flair.text'] = post_data.get('link_flair_text', '')
        if 'selftext' not in self.exclude_columns:
            datum['selftext'] = post_data['selftext']
        return datum

class CivilServantCommentSource(CivilServantSource):
    
    def label(self):
        parts = []
        parts.append(self.subreddit_id)
        if self.prefix is not None:
            parts.append("prefix")
        parts.append("comments")
        return "-".join(parts)
    
    def column_types(self):
        return {
            'id': str,
            'fullname': str,
            'created.utc': float,
            'created.at': float,
            'link.id': str,
            'parent.id': str,
            'subreddit.id': str,
            'author': str,
            'author.fullname': str,
            'is.submitter': bool,
            'body': str
        }
    
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

        total_comments = self.db.query(Comment).filter(filter).count()
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

            comments = self.db.query(Comment).filter(and_(
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
                datum = self.extract_row(comment)
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

    def extract_row(self, comment):
        comment_data = json.loads(comment.comment_data)
        created_utc = utc.localize(datetime.utcfromtimestamp(comment_data['created_utc']))
        datum = {}
        if 'id' not in self.exclude_columns:
            datum['id'] = comment.id
        if 'fullname' not in self.exclude_columns:
            datum['fullname'] = comment_data['name']
        if 'created.utc' not in self.exclude_columns:
            datum['created.utc'] = utc.localize(comment.created_utc).timestamp()
        if 'created.at' not in self.exclude_columns:
            datum['created.at'] = utc.localize(comment.created_at).timestamp()
        if 'link.id' not in self.exclude_columns:
            datum['link.id'] = comment_data['link_id']
        if 'parent.id' not in self.exclude_columns:
            datum['parent.id'] = comment_data['parent_id']
        if 'subreddit.id' not in self.exclude_columns:
            datum['subreddit.id'] = comment.subreddit_id
        if 'author' not in self.exclude_columns:
            datum['author'] = comment_data['author']
        if 'author.fullname' not in self.exclude_columns:
            datum['author.fullname'] = comment_data.get('author_fullname')
        if 'is.submitter' not in self.exclude_columns:
            datum['is.submitter'] = comment_data['is_submitter']
        if 'body' not in self.exclude_columns:
            datum['body'] = comment_data['body']
        return datum
        
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
        parts = []
        parts.append(self.subreddit_id)
        if self.prefix is not None:
            parts.append("prefix")
        parts.append("modactions")
        return "-".join(parts)

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
        # resume_action is useful if actions are extracted separately
        self.resume_action = loaded_rows[-1]['action']
    
    def extract(self):
        self.logger.info("Extracting mod actions")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        self.logger.info("  Extracting actions:")
        for action in self.actions:
            self.logger.info("    {}".format(action))
            
        resumed_count = 0
        last_resumed_time = None
        
        # Check whether we are resuming a dataset that extracted any of this action
        if self.resume_by_action is not None:
            for action in self.actions:
                for row in self.resume_by_action[action]:
                    resumed_count += 1
                    last_resumed_time = max(row['created.utc'], last_resumed_time)
                    yield row
                self.logger.info("    Used {} {} records from resumed dataset".format(resumed_count, action))
            next_start_time = last_resumed_time
        else:
            next_start_time = self.start_time            

        total_hours = int(math.ceil((self.end_time - self.start_time).total_seconds() / 3600))
        self.logger.info("    Querying {} 1 hour time periods".format(total_hours))

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
            # Filtering subreddit and action in code because otherwise queries are too slow
            filter = and_(
                ModAction.created_utc >= start_time,
                ModAction.created_utc < min(next_start_time, self.end_time))                
            modactions = self.db.query(ModAction).filter(filter)
            for modaction in modactions:
                if modaction.subreddit_id != self.subreddit_id:
                    continue
                if modaction.action not in self.actions:
                    continue
                datum = self.extract_row(modaction)
                count += 1
                if self.resume_by_action is None or datum['id'] not in self.completed_ids:
                    yield datum
                else:
                    duplicate_count += 1
                
            progress.update()
            progress.refresh()
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

    def extract_row(self, modaction):
        action_data = json.loads(modaction.action_data)
        datum = {}
        if 'id' not in self.exclude_columns:
            datum['id'] = modaction.id
        if 'created.utc' not in self.exclude_columns:
            datum['created.utc'] = utc.localize(modaction.created_utc).timestamp()
        if 'created.at' not in self.exclude_columns:
            datum['created.at'] = utc.localize(modaction.created_at).timestamp()
        if 'subreddit.id' not in self.exclude_columns:
            datum['subreddit.id'] = modaction.subreddit_id
        if 'action' not in self.exclude_columns:
            datum['action'] = modaction.action
        if 'mod' not in self.exclude_columns:
            datum['mod'] = action_data['mod']
        if 'target.author' not in self.exclude_columns:
            datum['target.author'] = modaction.target_author
        if 'target.fullname' not in self.exclude_columns:
            datum['target.fullname'] = action_data['target_fullname']
        if 'details' not in self.exclude_columns:
            datum['details'] = action_data['details']
        if 'description' not in self.exclude_columns:
            datum['description'] = action_data['description']
        return datum
                        
class CivilServantExperimentThingSource(CivilServantSource):
    def connect(
        self,
        subreddit_id,
        start_time,
        end_time,
        experiment_id,
        database_config=None,
        exclude_columns=None
    ):
        self.experiment_id = experiment_id
        super().connect(
            subreddit_id,
            start_time,
            end_time,
            database_config=database_config,
            exclude_columns=exclude_columns
        )
        
    def label(self):
        return "{}-{}-experiment_things".format(self.subreddit_id, self.experiment_id)
        
    def column_types(self):
        return {
            'id': str,
            'created.at': float,
            'object.created': float,
            'object.type': int,
            'thing.id': str,
            'query.index': str,
            'experiment.id': int,
            'metadata.json': str
        }
    
    def extract(self):
        self.logger.info("Extracting experiment_things")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  experiment_id: {}".format(self.experiment_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        # created_at is indexed, so query on that rather than created_utc
        # one day is added to end_time to catch cases where created_utc is
        # within the time period but created_at is after
        filter = and_(
            ExperimentThing.experiment_id == self.experiment_id,
            ExperimentThing.created_at >= self.start_time,
            ExperimentThing.created_at < self.end_time)

        total_experiment_things = self.db.query(ExperimentThing).filter(filter).count()
        self.logger.info("  Querying {} records".format(total_experiment_things))
        
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

            experiment_things = self.db.query(ExperimentThing).filter(and_(
                ExperimentThing.experiment_id == self.experiment_id,
                ExperimentThing.created_at >= start_time,
                ExperimentThing.created_at < next_start_time))

            for experiment_thing in experiment_things:
                datum = self.extract_row(experiment_thing)
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

    def extract_row(self, experiment_thing):
        created_at = utc.localize(experiment_thing.created_at)
        datum = {}
        if 'id' not in self.exclude_columns:
            datum['id'] = experiment_thing.id
        if 'created.at' not in self.exclude_columns:
            datum['created.at'] = utc.localize(experiment_thing.created_at).timestamp()
        if 'object.created' not in self.exclude_columns and experiment_thing.object_created is not None:
            datum['object.created'] = utc.localize(experiment_thing.object_created).timestamp()
        if 'object.type' not in self.exclude_columns:
            datum['object.type'] = experiment_thing.object_type
        if 'thing.id' not in self.exclude_columns:
            datum['thing.id'] = experiment_thing.thing_id
        if 'query.index' not in self.exclude_columns:
            datum['query.index'] = experiment_thing.query_index
        if 'experiment.id' not in self.exclude_columns:
            datum['experiment.id'] = experiment_thing.experiment_id
        if 'metadata.json' not in self.exclude_columns:
            datum['metadata.json'] = experiment_thing.metadata_json
        return datum

### CODE BELOW THIS LINE HAS NOT BEEN PORTED TO V2 CONVENTIONS ###

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

class CivilServantExperimentThingSnapshotSource(CivilServantSource):
    def connect(self,
                 subreddit_id,
                 start_time,
                 end_time,
                 experiment_id,
                 database_config=None,
                 ignore=None
                ):
        self.experiment_id = experiment_id
        super().__init__(subreddit_id, start_time, end_time, database_config=database_config, ignore=ignore)
        
    def label(self):
        return "{}-{}-experiment_thing_snapshots".format(self.subreddit_id, self.experiment_id)
    
    def columns(self):
        return [
            field for field in [
                "id",
                "created.at",
                "experiment.thing.id",
                "experiment.id",
                "metadata.json"
            ]
            if field not in self.ignore
        ]

    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ("id", str),
                ("created.at", float),
                ("experiment.thing.id", str),
                ("experiment.id", int),
                ("metadata.json", str)
            ]
            if field not in self.ignore
        )

    def extract(self):
        self.logger.info("Extracting experiment_thing_snapshots")
        self.logger.info("  subreddit_id: {}".format(self.subreddit_id))
        self.logger.info("  experiment_id: {}".format(self.experiment_id))
        self.logger.info("  starting: {}".format(self.start_time))
        self.logger.info("  ending: {}".format(self.end_time))

        # created_at is indexed, so query on that rather than created_utc
        # one day is added to end_time to catch cases where created_utc is
        # within the time period but created_at is after
        filter = and_(
            ExperimentThingSnapshot.experiment_id == self.subreddit_id,
            ExperimentThingSnapshot.created_at >= self.start_time,
            ExperimentThingSnapshot.created_at < self.end_time)

        total_experiment_things = self.db().query(ExperimentThingSnapshot).filter(filter).count()
        self.logger.info("  Querying {} records".format(total_experiment_things))
        
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

            experiment_things = self.db().query(ExperimentThingSnapshot).filter(and_(
                ExperimentThingSnapshot.experiment_id == self.experiment_id,
                ExperimentThingSnapshot.created_at >= start_time,
                ExperimentThingSnapshot.created_at < next_start_time))

            for experiment_thing in experiment_things:
                created_at = utc.localize(experiment_thing.created_at)
                datum = {}
                if 'id' not in self.ignore:
                    datum['id'] = experiment_thing.id
                if 'created.at' not in self.ignore:
                    datum['created.at'] = utc.localize(experiment_thing.created_at).timestamp()
                if 'experiment.thing.id' not in self.ignore:
                    datum['experiment.thing.id'] = experiment_thing.experiment_thing_id
                if 'experiment.id' not in self.ignore:
                    datum['experiment.id'] = experiment_thing.experiment_id
                if 'metadata.json' not in self.ignore:
                    datum['metadata.json'] = experiment_thing.metadata_json

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