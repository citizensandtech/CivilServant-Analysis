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

OUTPUTFILE = sys.argv[1]
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

#####################==================================
#####################==================================


print("CONSTRUCTING xRANK_VECTORS")

def construct_rank_vectors(is_subpage):
    rank_vectors = {}
    for pt in PageType:
        page_rank_vectors = {}
        
        if is_subpage:
            pages = db_session.query(SubredditPage).filter(and_(SubredditPage.page_type == pt.value,SubredditPage.created_at >= parser.parse(opening_date), SubredditPage.created_at <= parser.parse(closing_date)))
        else:
            pages = db_session.query(FrontPage).filter(FrontPage.page_type == pt.value)
        for page in pages:
            posts = json.loads(page.page_data)
            for i,post in enumerate(posts):
                pid = post['id']
                if pid not in page_rank_vectors:
                    page_rank_vectors[pid] = {}
                page_rank_vectors[pid][page.created_at] = i
        rank_vectors[pt] = page_rank_vectors
    return rank_vectors

srank_vectors = construct_rank_vectors(True)
frank_vectors = construct_rank_vectors(False)

#####################==================================
#####################==================================


print("CONSTRUCTING ALL_DELTAS")

#construct all_deltas

def construct_all_deltas(rank_vectors):
    all_deltas = {}
    for pt in PageType:
        all_deltas[pt] = {}
        for pid in rank_vectors[pt]:
            k = sorted(list(rank_vectors[pt][pid].keys())) # sorted timestamps
            deltas = [(k[i+1]-k[i], rank_vectors[pt][pid][k[i+1]]-rank_vectors[pt][pid][k[i]]) for i in range(len(k)-1)] 
            all_deltas[pt][pid] = deltas
    return all_deltas


all_sdeltas = construct_all_deltas(srank_vectors)
all_fdeltas = construct_all_deltas(frank_vectors)




#####################==================================
#####################==================================

print("CONSTRUCTING AVE")


def construct_ave(all_deltas):
    gap = {} # in sec
    for pt in PageType:
        num = 0
        total = 0
        for pid in all_deltas[pt]:
            total += sum([delta[0].total_seconds() for delta in all_deltas[pt][pid]])
            num += len(all_deltas[pt][pid])
        ave = total/num if num!=0 else 0 # no posts for this pt
        gap[pt] = datetime.timedelta(0, 2*ave).total_seconds()
    return gap

f_gap = construct_ave(all_fdeltas)
s_gap = construct_ave(all_sdeltas)

#####################==================================
#####################==================================


"""
print("CONSTRUCTING POST_TO_TIME")


post_to_time = {} # {pt: {pid: {rank_limit: {"total_time": 0, "outliers": [(time_delta, (previous_rank, next_rank))] }}}}

rank_limits = [4, 5, 10, 15, 20, 25]


rank_vectors = frank_vectors


for pt in PageType:
    #print(pt.value)

    max_time_diff = datetime.timedelta(0, ave[pt])
    post_to_time[pt] = {}

    for pid in srank_vectors[pt]:
        post_to_time[pt][pid] = {}
        for rank_limit in rank_limits:
            post_to_time[pt][pid][rank_limit] = {"total_time":0, "outliers":[]}
            total_time = 0 # post_to_time[pt][pid][rank_limit]["total_time"] = 0
            outliers = []
            previous_time = None
            previous_rank = None
            for time in sorted(srank_vectors[pt][pid].keys()):
                current_rank = srank_vectors[pt][pid][time]
                if previous_time:
                    if current_rank <= rank_limit:
                        prev_time_delta = time - previous_time
                        if prev_time_delta < max_time_diff:
                            total_time += prev_time_delta.total_seconds()
                            #print("total: {0}".format(total_time))
                        else:
                            outliers.append((prev_time_delta, (previous_rank, current_rank)))
                            #print("outliers: {0}".format(outliers))
                    else:
                        pass
                        # do nothing if current_rank is too low
                previous_time = time
                previous_rank = current_rank
                    
            post_to_time[pt][pid][rank_limit]["total_time"] = total_time
            post_to_time[pt][pid][rank_limit]["outliers"] = outliers
"""

#####################==================================
#####################==================================


print("CONSTRUCTING TRANSFORMED POST_TO_TIME")

def construct_xformed_post_to_time(rank_vectors, gap):
    post_to_time = {}  # {pid: {pt: {rank_limit: {"total_time": , "max_time_diff": , "num_gaps": , "sum_gaps": } }}}
    rank_limits = [4, 5, 10, 15, 20, 25]

    for pt in PageType:

        for pid in rank_vectors[pt]:
            if pid not in post_to_time:
                post_to_time[pid] = {}
            post_to_time[pid][pt] = {}

            for rank_limit in rank_limits:
                post_to_time[pid][pt][rank_limit] = {"total_time": 0, "max_time_diff":gap[pt], "num_gaps": 0, "sum_gaps": 0}
                total_time = 0
                num_gaps = 0
                sum_gaps = 0
                previous_time = None
                previous_rank = None
                for time in sorted(rank_vectors[pt][pid].keys()):
                    current_rank = rank_vectors[pt][pid][time]
                    if previous_time:
                        if current_rank <= rank_limit:
                            time_delta = (time - previous_time).total_seconds()
                            if time_delta < gap[pt]:
                                total_time += time_delta
                                #print("total: {0}".format(total_time))
                            else:
                                num_gaps += 1
                                sum_gaps += time_delta
                        else:
                            pass
                            # do nothing if current_rank is too low
                    previous_time = time
                    previous_rank = current_rank
                        
                post_to_time[pid][pt][rank_limit]["total_time"] = total_time
                post_to_time[pid][pt][rank_limit]["num_gaps"] = num_gaps
                post_to_time[pid][pt][rank_limit]["sum_gaps"] = sum_gaps
    return post_to_time

s_post_to_time = construct_xformed_post_to_time(srank_vectors, s_gap)
f_post_to_time = construct_xformed_post_to_time(frank_vectors, f_gap)

#####################==================================
#####################==================================



OUTPUTPATH = OUTPUTFILE

rank_limits = [4, 5, 10, 15, 20, 25]

rows = []

labels = ["post_id"]
for table in ["SubredditPage", "FrontPage"]:
    for pt in PageType:
        for rank in rank_limits:
            for value in ["time", "gap_cutoff", "num_gaps", "sum_gaps"]:
                labels.append("{0} in {1} {2} {3}".format(value, table, pt.name, rank))
rows.append(",".join(labels))


#janky joining of s_post_to_time and f_post_to_time

seen_pids = set([])

for pid in s_post_to_time:
    row = [pid]
    seen_pids.add(pid)
    for table in ["SubredditPage", "FrontPage"]:
        post_to_time = s_post_to_time if table=="SubredditPage" else f_post_to_time

        for pt in PageType:
            for rank in rank_limits:
                for value in ["total_time", "max_time_diff", "num_gaps", "sum_gaps"]:
                    cell = str(0)
                    if value=="max_time_diff":
                        cell = str(s_gap[pt]) if table=="SubredditPage" else str(f_gap[pt])
                    if pid in post_to_time and pt in post_to_time[pid]:
                        cell = str(post_to_time[pid][pt][rank][value])
                    row.append(cell)
    rows.append(",".join(row))


for pid in f_post_to_time:
    if pid not in seen_pids:
        row = [pid]
        seen_pids.add(pid)        
        for table in ["SubredditPage", "FrontPage"]:
            post_to_time = s_post_to_time if table=="SubredditPage" else f_post_to_time

            for pt in PageType:
                for rank in rank_limits:
                    for value in ["total_time", "max_time_diff", "num_gaps", "sum_gaps"]:
                        cell = str(0)
                        if value=="max_time_diff":
                            cell = str(s_gap[pt]) if table=="SubredditPage" else str(f_gap[pt])
                        if pid in post_to_time and pt in post_to_time[pid]:
                            cell = str(post_to_time[pid][pt][rank][value])
                        row.append(cell)
    rows.append(",".join(row))
    

with open(OUTPUTPATH, 'w') as f:
    f.write("\n".join(rows))
