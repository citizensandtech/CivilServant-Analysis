"""

removing: 
max rank file creation


adding to curve data file:
- score
- difference between snapshot and when that post was submitted (age)
- median elapsed time of other posts on that page (median age)
then differences:
- time difference from last observation (seconds)
- difference in rank position
- difference in score
- difference in the median elapsed time of other posts on that page (difference in median age)



python query_link_ranking_curve_v2.py foo.csv 2016-10-26 2016-11-11

"""

import inspect, os, sys, copy, pytz, re, glob
import simplejson as json
import pandas as pd
from dateutil import parser
import datetime
import numpy as np
from sqlalchemy import and_, or_
import pytz
from tzlocal import get_localzone
import statistics

BASE_DIR = os.environ['CS_PATH']
sys.path.append(BASE_DIR)

from app.models import Base, SubredditPage, FrontPage, Subreddit, Post, ModAction
from utils.common import PageType

#import pybloom
utc=pytz.UTC


CURVE_OUTPUTFILE = sys.argv[1]
#subreddit = sys.argv[2]
opening_date = sys.argv[2]
closing_date = sys.argv[3]

ENV = "analysis"
#os.environ['CS_ENV'] = 'production'

with open(os.path.join(BASE_DIR, "config") + "/{env}.json".format(env=ENV), "r") as config:
  DBCONFIG = json.loads(config.read())

### LOAD SQLALCHEMY
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy import text

db_engine = create_engine("mysql://{user}:{password}@{host}/{database}".format(
    host = DBCONFIG['host'],
    user = DBCONFIG['user'],
    password = DBCONFIG['password'],
    database = DBCONFIG['database']))
DBSession = sessionmaker(bind=db_engine)

### LOAD PRAW
#import reddit.connection
#conn = reddit.connection.Connect(base_dir=BASE_DIR, env=ENV) #env="jupyter"

### FILTER OUT DEPRECATION WARNINGS ASSOCIATED WITH DECORATORS
# https://github.com/ipython/ipython/issues/9242
import warnings
warnings.filterwarnings('ignore', category=DeprecationWarning, message='.*use @default decorator instead.*')

Base.metadata.bind = db_engine
DBSession = sessionmaker(bind=db_engine)
db_session = DBSession()


local_tz = get_localzone()

#####################==================================
#####################==================================


print("CONSTRUCTING xRANK_VECTORS")

def construct_rank_vectors(is_subpage):
    rank_vectors = {}   # rank_vectors[subid][pt][pid][page.created_at] = {page_id, rank, score, age}
    max_rank_vectors = {} # [pid][subid][pt] = i
    page_ages = {} # [page_id][pid] = age

    for pt in PageType:
        print(pt)
        if is_subpage:
            pages = db_session.query(SubredditPage).filter(and_(SubredditPage.page_type == pt.value,SubredditPage.created_at >= parser.parse(opening_date), SubredditPage.created_at <= parser.parse(closing_date)))
        else:
            pages = db_session.query(FrontPage).filter(FrontPage.page_type == pt.value)
        for page in pages:
            subid = "FRONT PAGE"
            if is_subpage:
                subid = page.subreddit_id
            posts = json.loads(page.page_data)
            
            page_ages[page.id] = []

            for i,post in enumerate(posts):
                pid = post['id']

                if subid not in rank_vectors:
                    rank_vectors[subid] = {}
                if pt not in rank_vectors[subid]:
                    rank_vectors[subid][pt] = {}
                if pid not in rank_vectors[subid][pt]:                
                    rank_vectors[subid][pt][pid] = {}

                created_at_local = local_tz.localize(page.created_at)
                created_at_utc = created_at_local.astimezone(pytz.utc)

                if created_at_utc not in rank_vectors[subid][pt][pid]:
                    rank_vectors[subid][pt][pid][created_at_utc] = {}
                rank_vectors[subid][pt][pid][created_at_utc]["rank"] = i
                rank_vectors[subid][pt][pid][created_at_utc]["score"] = post["score"]

                if "created_utc" not in post.keys():
                    age = None
                else: 
                    age = created_at_utc.timestamp() - post["created_utc"] # time (in seconds) between post creation and snapshot 
                rank_vectors[subid][pt][pid][created_at_utc]["age_sec"] = age
                page_ages[page.id].append(age)

                rank_vectors[subid][pt][pid][created_at_utc]["page_id"] = page.id

    return rank_vectors, max_rank_vectors, page_ages

srank_vectors, smax_rank_vectors, spage_ages = construct_rank_vectors(True)
#frank_vectors, fmax_rank_vectors, fpage_ages = construct_rank_vectors(False)



#####################==================================
#####################==================================

"""

trajectory/longitudinal/curve study:
subreddit/frontpage | page type | post id | timestamp | rank

"""

print("WRITING TO CURVE_OUTPUTFILE")
rows = []
labels = ["subreddit/frontpage", "page type", "post id", "timestamp", "rank", "score", 
    "age_sec", "other_median_age", "time_diff", "rank_diff", "score_diff", "median_age_diff"]
rows.append(",".join(labels))

for (vectors, page_ages) in [(srank_vectors, spage_ages)]: # (frank_vectors, fpage_ages),    # subreddit or front page  
    for subid in vectors:   # subreddit id
        for pt in vectors[subid]:   # page type
            for pid in vectors[subid][pt]:  # post id

                prev_timestamp = None
                prev_rank = None
                prev_score = None
                prev_median_age = None

                for timestamp in sorted(vectors[subid][pt][pid].keys()):   #   timestamp
                    rank = vectors[subid][pt][pid][timestamp]["rank"]
                    score = vectors[subid][pt][pid][timestamp]["score"]
                    age_sec = vectors[subid][pt][pid][timestamp]["age_sec"]

                    page_id = vectors[subid][pt][pid][timestamp]["page_id"]

                    age_index = page_ages[page_id].index(age_sec)
                    other_median_age = statistics.median(page_ages[page_id][:age_index] + page_ages[page_id][age_index:])  
                    time_diff = timestamp - prev_timestamp if prev_timestamp is not None else None
                    rank_diff = rank - prev_rank if prev_rank is not None else None
                    score_diff = score - prev_score if prev_score is not None else None
                    median_age_diff = other_median_age - prev_median_age if prev_median_age is not None else None

                    row = [subid, pt.name, pid, str(timestamp), str(rank), str(score), str(age_sec), str(other_median_age), 
                        str(time_diff), str(rank_diff), str(score_diff), str(median_age_diff)]                    
                    row = ",".join(row)
                    rows.append(row)

                    prev_timestamp = timestamp
                    prev_rank = rank
                    prev_score = score
                    prev_median_age = other_median_age                    

            print(rows[-1])

with open(CURVE_OUTPUTFILE, 'w') as f:
    f.write("\n".join(rows))
