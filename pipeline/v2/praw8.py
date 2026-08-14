from datetime import datetime, timedelta
import csv
import json
import logging
import math
import os
import pytz
import time
from tqdm import tqdm

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
            response = self.reddit.info(fullnames=page_ids)
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
                if 'subreddit.id' not in self.ignore:
                    result['subreddit.id'] = thing.subreddit_id
                if 'selftext' not in self.ignore:
                    if thing.selftext == "[removed]":
                        result['selftext'] = None
                    else:
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
            progress.refresh()
            time.sleep(self.delay_s)

        progress.refresh()
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
            self.logger.info("  Beginning new extraction of {} rows".format(len(query_fullnames)))
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
                response = self.reddit.info(fullnames=page_fullnames)
                time.sleep(self.delay_s)
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
                        if thing.body == "[removed]":
                            result['body'] = None
                        else:
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
                    count += 1
                    progress.update()
                    progress.refresh()
                    yield result

        progress.refresh()
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        if self.resume_by_id is not None:
            self.logger.info("  Re-used {} rows from resumed dataset".format(resumed))
        self.logger.info("  Skipped {} rows out of date range".format(skipped))
        self.logger.info("  Skipped {} rows not matching subreddit_id".format(skipped_subreddit))

class PRAWRedditorSource(DataSource):
    def __init__(self, subreddit_id, start_time, end_time, reddit=None, redditors=[], delay_s=2, ignore=None):
        self.start_time = start_time
        self.end_time = end_time
        self.subreddit_id = subreddit_id
        self.reddit = reddit
        self.redditors = [redditor for redditor in redditors if redditor != '']
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.delay_s = delay_s
        if ignore is None:
            self.ignore = []
        else:
            self.ignore = ignore
        
    def label(self):
        return "{}-praw_redditors".format(self.subreddit_id)
    
    def columns(self):
        return [
            field for field in [
                "fullname",
                "name",
                "created.utc"
            ]
            if field not in self.ignore
        ]
        
    def column_types(self):
        return dict(
            (field, field_type) for field, field_type in [
                ("fullname", str),
                ("name", str),
                ("created.utc", float)
            ]
            if field not in self.ignore
        )
        
    def extract(self):
        self.logger.info("Extracting Reddit PRAW Redditors")

        count = 0
        pages = math.ceil(len(self.redditors) / 100)
        progress = tqdm(total=len(self.redditors))
        for page in range(pages):
            remaining = set(self.redditors[page*100:(page+1)*100])
            while len(remaining) > 0:
                response = self.reddit.redditors.partial_redditors(list(remaining))
                requested_count = len(remaining)
                page_thing_count = 0
                redditors_fetched = set()
                for thing in response:
                    result = {}
                    if 'fullname' not in self.ignore:
                        result['fullname'] = thing.fullname
                    if 'name' not in self.ignore:
                        result['name'] = thing.name
                    if 'created.utc' not in self.ignore:
                        result['created.utc'] = thing.created_utc
                    remaining.remove(thing.fullname)
                    count += 1
                    page_thing_count += 1
                    progress.update()
                    yield result
                self.logger.info("  Requested {} records, received {}".format(requested_count, page_thing_count))
                progress.refresh()
                time.sleep(self.delay_s)
            
        self.logger.info("  Done")
        self.logger.info("  Extracted {} rows".format(count))
        
