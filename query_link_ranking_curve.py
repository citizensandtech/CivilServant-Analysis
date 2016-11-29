import inspect, os, sys, copy, pytz, re, glob
import simplejson as json
import pandas as pd
from dateutil import parser
import datetime
import numpy as np
from sqlalchemy import and_, or_

BASE_DIR = os.environ['CS_PATH']
sys.path.append(BASE_DIR)

from app.models import Base, SubredditPage, FrontPage, Subreddit, Post, ModAction
from utils.common import PageType

#import pybloom
utc=pytz.UTC

CURVE_OUTPUTFILE = sys.argv[1]
MAX_OUTPUTFILE = sys.argv[2]
#subreddit = sys.argv[2]
opening_date = sys.argv[3]
closing_date = sys.argv[4]

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

#####################==================================
#####################==================================


print("CONSTRUCTING xRANK_VECTORS")

def construct_rank_vectors(is_subpage):
    rank_vectors = {}   # rank_vectors[subid][pt][pid][page.created_at] = i
    max_rank_vectors = {} # [pid][subid][pt] = i

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
            for i,post in enumerate(posts):
                pid = post['id']

                #CURVE WORK
                if subid not in rank_vectors:
                    rank_vectors[subid] = {}
                if pt not in rank_vectors[subid]:
                    rank_vectors[subid][pt] = {}
                if pid not in rank_vectors[subid][pt]:                
                    rank_vectors[subid][pt][pid] = {}
                rank_vectors[subid][pt][pid][page.created_at] = i

                #MAX RANK WORK
                if pid not in max_rank_vectors:
                    max_rank_vectors[pid] = {}
                if subid not in max_rank_vectors[pid]:
                    max_rank_vectors[pid][subid] = {}
                if (pt not in max_rank_vectors[pid][subid]) or (i < max_rank_vectors[pid][subid][pt]):
                    # max rank = smallest number placement
                    max_rank_vectors[pid][subid][pt] = i

    return rank_vectors, max_rank_vectors

srank_vectors, smax_rank_vectors = construct_rank_vectors(True)
frank_vectors, fmax_rank_vectors = construct_rank_vectors(False)



#####################==================================
#####################==================================

"""

trajectory/longitudinal/curve study:
subreddit/frontpage | page type | post id | timestamp | rank

"""    



print("WRITING TO CURVE_OUTPUTFILE")
rows = []
labels = ["subreddit/frontpage", "page type", "post id", "timestamp", "rank"]
rows.append(",".join(labels))

for vectors in [frank_vectors, srank_vectors]: 
    for subid in vectors:
        for pt in vectors[subid]:
            for pid in vectors[subid][pt]:
                for timestamp in vectors[subid][pt][pid]:
                    rank = vectors[subid][pt][pid][timestamp]
                    row = [subid, pt.name, pid, str(timestamp), str(rank)]                    
                    row = ",".join(row)
                    rows.append(row)
            print(rows[-1])

with open(CURVE_OUTPUTFILE, 'w') as f:
    f.write("\n".join(rows))



"""

max ranking study:
post id | sub id | max rank for top ...

"""

print("WRITING TO MAX_OUTPUTFILE")
rows = []
labels = ["post_id", "sub_id"]
for table in ["FrontPage", "SubredditPage"]:
    for pt in PageType:
        labels.append("{0} {1}".format(table, pt.name))
rows.append(",".join(labels))


for vectors in [fmax_rank_vectors, smax_rank_vectors]:
    for pid in vectors:
        row = [pid]
        p_sub_id = ""
        page_ranks = {"SubredditPage":["","","",""], "FrontPage":["","","",""]}

        for subid in vectors[pid]:
            pranks = [str(vectors[pid][subid][pt]) if pt in vectors[pid][subid] else "" for pt in PageType]
            if subid == "FRONT PAGE":
                page_ranks["FrontPage"] = pranks
            else:
                page_ranks["SubredditPage"] = pranks
                p_sub_id = subid
        row.append(p_sub_id)
        row += page_ranks["FrontPage"]
        row += page_ranks["SubredditPage"]

        rows.append(", ".join(row))
        
        print(rows[-1])


with open(MAX_OUTPUTFILE, 'w') as f:
    f.write("\n".join(rows))