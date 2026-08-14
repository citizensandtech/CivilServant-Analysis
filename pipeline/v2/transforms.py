from datetime import datetime
import logging
import pytz
import re
from tqdm import tqdm

from . import _bool

utc = pytz.UTC

re_temporary = re.compile(r'^(\d+) day')
re_permanent = re.compile(r'^permanent$')
re_changed_temporary = re.compile(r'^(?:Ban )?changed to (\d+) day')
re_changed_permanent = re.compile(r'^changed to permanent')

class CivilServantTransformToAccounts:
    """Creates an account dataset from comment, post, and mod action data.

    This class is passed to `DataFile` as the first argument when creating or opening
    an account `DataFile`.
    
    PARAMETERS
    ----------
    `start_time` : The beginning of the data collection time window (inclusive)
    
    `end_time` : The end of the data collection time window (exclusive)
    
    `subreddit_id` : The id of the subreddit being observed
    
    `posts` : DataFile of posts extracted by CivilServantPostSource
    
    `comments` : DataFile of comments extracted by CivilServantCommentSource
    
    `modactions` : DataFile of mod actions extracted by CivilServantModAction source
    
    `praw_posts` : DataFile of posts from PRAW extracted by PRAWPostSource
    
    `praw_comments` : DataFile of comments from PRAW extracted by PRAWComment
    """
    def __init__(
        self, 
        start_time, 
        end_time, 
        subreddit_id, 
        posts=None, 
        comments=None, 
        modactions=None, 
        praw_posts=None, 
        praw_comments=None
    ):
        self.start_time = start_time
        self.end_time = end_time
        self.subreddit_id = subreddit_id
        if comments is not None:
            self.comments = sorted(
                list(comments.rows()) + list(praw_comments.rows()),
                key=lambda row: float(row['created.utc']))
        if posts is not None:
            self.posts = sorted(
                list(posts.rows()) + list(praw_posts.rows()),
                key=lambda row: float(row['created.utc']))
        if modactions is not None:
            self.modactions = sorted(modactions.rows(), key=lambda row: float(row['created.utc']))
        if praw_posts is not None:
            self.praw_posts = sorted(praw_posts.rows(), key=lambda row: float(row['created.utc']))
        if praw_comments is not None:
            self.praw_comments = sorted(praw_comments.rows(), key=lambda row: float(row['created.utc']))
        self.data = {}
        self.logger = logging.getLogger("CivilServant-Analysis")
        
    def label(self):
        """Returns the label used to construct filenames."""
        return "{}-accounts".format(self.subreddit_id)
        
    def columns(self):
        """Returns a list of column names in the order of column index."""
        return self.column_types().keys()
        
    def column_types(self):
        """Returns a list of column names in the order of column index."""
        return {
            'username': str,
            'num.comments': int,
            'num.posts': int,
            'num.comments.removed': int,
            'num.posts.removed': int,
            'num.comments.spam': int,
            'num.posts.spam': int,
            'num.temporary.suspensions': int,
            'num.bans': int,
            'elapsed.days.since.first.post.or.comment': float,
            'elapsed.days.since.last.post.or.comment': float,
            'is.currently.suspended': _bool
        }
        
    def transform(self):
        """Transforms post, comment, and mod action data to create a dataset of accounts.

        Columns are generated one-at-a-time. Once all columns have been created, results are
        returned as a generator, for compatibility with DataFile.

        RETURNS
        -------
        A generator yielding dicts of the form `{ <column_name>: <column_value>, ... }`.
        """
        
        self.init_author_map()
        self.init_usernames()
        self.init_post_fullnames()
        self.init_comment_fullnames()
        self.init_user_bans_suspensions()
        
        for column in self.columns():
            make = getattr(self, "make_" + column.replace('.', '_'))
            self.data[column] = make()

        for username in self.usernames:
            row = dict(
                (column, self.data[column][username])
                for column in self.columns())
            yield row
    
    def init_author_map(self):
        """ Initializes post/comment author mapping from mod actions.

        Sets the instance varaibles:
        
        `author_by_thing_id` : authors of mod action targets,
        of the form `{ <thing_fullname>: <username>, ... }`

        `unknown_author_thing_id` : set of fullnames of mod action targets with no known author
        """
        self.logger.info("Initializing username/thing_id map from mod actions")

        self.author_by_thing_id = {}
        self.unknown_author_thing_id = set()
        for modaction in self.modactions:
            if modaction['action'] in ['removelink', 'removecomment', 'spamlink', 'spamcomment']:
                author = modaction['target.author']
                if author in ['', '[removed]', '[deleted]']:
                    self.unknown_author_thing_id.add(modaction['target.fullname'])
                    continue
                self.author_by_thing_id[modaction['target.fullname']] = modaction['target.author']
        
        self.logger.info("  Found {} author-thing pairs".format(len(self.author_by_thing_id)))
        self.logger.info("  Skipped {} mod actions with unknown authors".format(len(self.unknown_author_thing_id)))
        
    def init_usernames(self):
        """Initializes a set of all usernames in the dataset.

        Creates instance varaibles:
        `usernames` : sorted list of all usernames in the dataset.
        """
        self.logger.info("Initializing usernames")
        usernames = set()

        # Removed posts/comments fetched from PRAW do not have author info
        # so it must be taken from the modaction
        self.unknown_author_posts = set()
        for post in self.posts:
            if post['author'] in ['', '[removed]', '[deleted]']:
                try:
                    author = self.author_by_thing_id[post['fullname']]
                    post['author'] = author
                    usernames.add(author)
                except KeyError:
                    self.unknown_author_posts.add(post['fullname'])
            else:
                usernames.add(post['author'])
                self.author_by_thing_id[post['fullname']] = post['author']

        self.unknown_author_comments = set()
        for comment in self.comments:
            if comment['author'] in ['', '[removed]', '[deleted]']:
                try:
                    author = self.author_by_thing_id[comment['fullname']]
                    comment['author'] = author
                    usernames.add(author)
                except KeyError:
                    self.unknown_author_comments.add(comment['fullname'])
            else:
                usernames.add(comment['author'])
                self.author_by_thing_id[comment['fullname']] = comment['author']

        # Add users who didn't post/comment but were banned during observation window
        for modaction in self.modactions:
            if modaction['target.author'] in ['', '[removed]', '[deleted]']:
                continue
            if modaction['action'] == 'banuser':
                usernames.add(modaction['target.author'])

            
        self.usernames = sorted(usernames)
        self.logger.info("  Identified {} usernames in posts, comments, and modlog".format(len(usernames)))
        self.logger.info("  {} posts with unknown author".format(len(self.unknown_author_posts)))
        self.logger.info("  {} comments with unknown author".format(len(self.unknown_author_comments)))

    def init_post_fullnames(self):
        """Initializes set of post fullnames to be counted in the account dataset.

        Creates instance variable:
        `post_fullnames` : set of post fullnames, excluding those with unknown author.
        """
        self.logger.info("Initializing post fullnames")
        self.post_fullnames = (
            set([post['fullname'] for post in self.posts])
            - self.unknown_author_posts)
        self.logger.info("  Found {}".format(len(self.post_fullnames)))
        self.logger.info("  Excluded {} with unknown author".format(len(self.unknown_author_posts)))

    def init_comment_fullnames(self):
        """Initializes set of comment fullnames to be counted in the account dataset.

        Creates instance variable:
        `comment_fullnames` : set of comment fullnames, excluding those with unknown author.
        """
        self.logger.info("Initializing comment fullnames")
        self.comment_fullnames = (
            set(comment['fullname'] for comment in self.comments)
            - self.unknown_author_comments)
        self.logger.info("  Found {}".format(len(self.comment_fullnames)))
        self.logger.info("  Excluded {} with unknown author".format(len(self.unknown_author_comments)))

    def init_user_bans_suspensions(self):
        """Initialize list of bans and suspensions for each username.

        Creates the instance variable:
        `bans_by_user` : dict mapping usernames to a list with an element for each ban
        or suspension.
        """
        self.logger.info("Initializing bans and suspensions for each account")
        self.bans_by_user = {}

        for modaction in self.modactions:
            details = modaction['details']
            author = modaction['target.author']
            created_utc = float(modaction['created.utc'])
            if author not in self.bans_by_user:
                self.bans_by_user[author] = []
            if modaction['action'] == 'banuser':
                if details in ['permanent']:
                    self.bans_by_user[author].append('permanent')
                elif re.match(re_temporary, details):
                    self.bans_by_user[author].append('temporary')
                elif re.match(re_changed_permanent, details):
                    # This is a change, the most recent ban is updated rather than adding a new one
                    try:
                        if self.bans_by_user[author][-1] == 'temporary':
                            self.bans_by_user[-1] = 'permanent'
                    except IndexError:
                        self.logger.warning("Ban type changed to permanent, but not seen previously: {} {}".format(modaction['target.author'], str(modaction['created.utc'])))
                        self.bans_by_user[author].append('permanent')
                elif re.match(re_changed_temporary, details):
                    # This is a change, the most recent ban is updated rather than adding a new one
                    try:
                        if self.bans_by_user[author][-1] == 'permanent':
                            self.bans_by_user[-1] = 'temporary'
                    except IndexError:
                        self.logger.warning("Ban type changed to temporary, but not seen previously: {}".format(str(modaction['created.utc'])))
                        self.bans_by_user[author].append('temporary')
                else:
                    raise Exception("Unknown details in banuser: {}".format(details))

    def make_username(self):
        self.logger.info ("Creating username")
        data = dict((username, username) for username in self.usernames)
        self.logger.info("  Done")
        return data
        
    def make_num_posts(self):
        self.logger.info("Creating num.posts")
        self.logger.info("  Processing {} posts".format(len(self.posts)))

        data = dict((username, 0) for username in self.usernames)
        skipped = 0
        skipped_deleted = 0
        for post in self.posts:
            author = post['author']
            if author == '':
                try:
                    author = self.author_by_thing_id[post['fullname']]
                except KeyError:
                    #self.logger.warning("Unkown author for post: {}".format(post['fullname']))
                    skipped += 1
                    continue
            if author == '[deleted]':
                try:
                    author = self.author_by_thing_id[post['fullname']]
                except KeyError:
                    #self.logger.warning("Unkown author for post: {}".format(post['fullname']))
                    skipped_deleted += 1
                    continue                
            data[author] += 1
            
        self.logger.info("  Done")
        self.logger.info("  Skipped {} removed posts with unknown authors".format(skipped))
        self.logger.info("  Skipped {} deleted posts with unknown authors".format(skipped_deleted))
        return data

    def make_num_comments(self):
        self.logger.info("Creating num.comments")
        self.logger.info("  Processing {} comments".format(len(self.comments)))

        data = dict((username, 0) for username in self.usernames)
        skipped = 0
        skipped_deleted = 0
        for comment in self.comments:
            author = comment['author']
            if author == '':
                try:
                    author = self.author_by_thing_id[comment['fullname']]
                except KeyError:
                    #self.logger.warning(" Unknown author for comment {}".format(comment['fullname']))
                    skipped += 1
                    continue
            if author == '[deleted]':
                try:
                    author = self.author_by_thing_id[comment['fullname']]
                except KeyError:
                    #self.logger.warning(" Unknown author for comment {}".format(comment['fullname']))
                    skipped_deleted += 1
                    continue
            data[author] += 1
            
        self.logger.info("  Done")
        self.logger.info("  Skipped {} removed comments with unknown author".format(skipped))
        self.logger.info("  Skipped {} deleted comments with unknown author".format(skipped_deleted))
        return data

    def make_num_posts_removed(self):
        self.logger.info("Creating num.posts.removed")
        self.logger.info("  Processing {} modactions".format(len(self.modactions)))

        data = dict((username, 0) for username in self.usernames)
        removed_posts = set()
        for modaction in tqdm(self.modactions):
            target_fullname = modaction['target.fullname']
            if modaction['action'] == 'removelink':
                removed_posts.add(target_fullname)
            elif modaction['action'] == 'approvelink':
                if target_fullname in removed_posts:
                    removed_posts.remove(target_fullname)
                    
        skipped_posts = set()
        for post_fullname in tqdm(list(removed_posts)):
            if post_fullname not in self.post_fullnames:
                skipped_posts.add(post_fullname)
                continue
            author = self.author_by_thing_id[post_fullname]
            data[author] += 1

        num_counted = len(removed_posts - skipped_posts)
        
        self.logger.info("  Done")
        self.logger.info("  Attributed {} posts to authors".format(num_counted))
        self.logger.info("  Skipped {} unrecognized posts".format(len(skipped_posts)))

        return data
        
    def make_num_comments_removed(self):
        self.logger.info("Creating num.comments.removed")
        self.logger.info("  Processing {} modactions".format(len(self.modactions)))

        data = dict((username, 0) for username in self.usernames)
        removed_comments = set()
        for modaction in tqdm(self.modactions):
            target_fullname = modaction['target.fullname']
            if modaction['action'] == 'removecomment':
                removed_comments.add(target_fullname)
            elif modaction['action'] == 'approvecomment':
                if target_fullname in removed_comments:
                    removed_comments.remove(target_fullname)
                    
        self.skipped_comments = set()
        
        for comment_fullname in tqdm(list(removed_comments)):
            if comment_fullname not in self.comment_fullnames:
                self.skipped_comments.add(comment_fullname)
                continue
            author = self.author_by_thing_id[comment_fullname]
            data[author] += 1

        num_counted = len(removed_comments - self.skipped_comments)
        
        self.logger.info("  Done")
        self.logger.info("  Attributed {} comments to authors".format(num_counted))
        self.logger.info("  Skipped {} unrecognized comments".format(len(self.skipped_comments)))

        return data

    def make_num_posts_spam(self):
        self.logger.info("Creating num.posts.spam")
        self.logger.info("  Processing {} modactions".format(len(self.modactions)))

        data = dict((username, 0) for username in self.usernames)
        spam_posts = set()
        for modaction in self.modactions:
            target_fullname = modaction['target.fullname']
            if modaction['action'] == 'spamlink':
                spam_posts.add(target_fullname)
            elif modaction['action'] == 'approvelink':
                if target_fullname in spam_posts:
                    spam_posts.remove(target_fullname)
                    
        skipped_posts = set()
        for post_fullname in list(spam_posts):
            if post_fullname not in self.post_fullnames:
                skipped_posts.add(post_fullname)
                continue
            author = self.author_by_thing_id[post_fullname]
            data[author] += 1
            
        self.logger.info("  Done")
        self.logger.info("  Skipped {} unrecognized posts".format(len(skipped_posts)))

        return data
        
    def make_num_comments_spam(self):
        self.logger.info("Creating num.comments.spam")
        self.logger.info("  Processing {} modactions".format(len(self.modactions)))

        data = dict((username, 0) for username in self.usernames)
        spam_comments = set()
        for modaction in self.modactions:
            target_fullname = modaction['target.fullname']
            if modaction['action'] == 'spamcomment':
                spam_comments.add(target_fullname)
            elif modaction['action'] == 'approvecomment':
                if target_fullname in spam_comments:
                    spam_comments.remove(target_fullname)
                    
        skipped_comments = set()
        for comment_fullname in list(spam_comments):
            if comment_fullname not in self.comment_fullnames:
                skipped_comments.add(comment_fullname)
                continue
            author = self.author_by_thing_id[comment_fullname]
            data[author] += 1
            
        self.logger.info("  Done")
        self.logger.info("  Skipped {} unrecognized comments".format(len(skipped_comments)))

        return data

    def make_num_temporary_suspensions(self):
        self.logger.info("Creating num.temporary.suspensions")
        
        data = dict((username, 0) for username in self.usernames)
        for user, bans in self.bans_by_user.items():
            temporary = [ban for ban in bans if ban == "temporary"]
            data[user] = len(temporary)

        self.logger.info("  Done")
        return data

    def make_num_bans(self):
        self.logger.info("Creating num.bans")
        
        data = dict((username, 0) for username in self.usernames)
        for user, bans in self.bans_by_user.items():
            permanent = [ban for ban in bans if ban == 'permanent']
            data[user] = len(permanent)

        self.logger.info("  Done")
        return data

    def make_elapsed_days_since_first_post_or_comment(self):
        self.logger.info("Creating elapsed.days.since.first.post.or.comment")

        first_activity = dict((username, None) for username in self.usernames)
        for post in self.posts:
            if post['fullname'] in self.unknown_author_posts:
                continue
            author = post['author']
            if author in ['', '[deleted]']:
                author = self.author_by_thing_id[post['fullname']]
            created_utc = float(post['created.utc'])
            if first_activity[author] is None or first_activity[author] > created_utc:
                first_activity[author] = created_utc
        for comment in self.comments:
            if comment['fullname'] in self.unknown_author_comments:
                continue
            author = comment['author']
            if author in ['', '[deleted]']:
                author = self.author_by_thing_id[comment['fullname']]
            created_utc = float(comment['created.utc'])
            if first_activity[author] is None or first_activity[author] > created_utc:
                first_activity[author] = created_utc

        data = {}
        for username, created_utc in first_activity.items():
            if created_utc is None:
                data[username] = None
                continue
            since = (self.end_time - utc.localize(datetime.utcfromtimestamp(created_utc)))
            days = since.total_seconds() / 60 / 60 / 24
            data[username] = days
        
        self.logger.info("  Done")
        return data
        
    def make_elapsed_days_since_last_post_or_comment(self):
        self.logger.info("Creating elapsed.days.since.last.post.or.comment")

        first_activity = dict((username, None) for username in self.usernames)
        for post in self.posts:
            if post['fullname'] in self.unknown_author_posts:
                continue
            author = post['author']
            if author in ['', '[deleted]']:
                author = self.author_by_thing_id[post['fullname']]
            created_utc = float(post['created.utc'])
            if first_activity[author] is None or first_activity[author] < created_utc:
                first_activity[author] = created_utc
        for comment in self.comments:
            if comment['fullname'] in self.unknown_author_comments:
                continue
            author = comment['author']
            if author in ['', '[deleted]']:
                author = self.author_by_thing_id[comment['fullname']]
            created_utc = float(comment['created.utc'])
            if first_activity[author] is None or first_activity[author] < created_utc:
                first_activity[author] = created_utc

        data = {}
        for username, created_utc in first_activity.items():
            if created_utc is None:
                data[username] = None
                continue
            since = (self.end_time - utc.localize(datetime.utcfromtimestamp(created_utc)))
            days = since.total_seconds() / 60 / 60 / 24
            data[username] = days
        
        self.logger.info("  Done")
        return data

    def make_is_currently_suspended(self):
        self.logger.info("Creating is.currently.suspended")

        # Permanent suspensions
        banned = set()

        # End datetime of temporary suspsensions
        suspension_ends = {}

        # Find the end datetime of the most recent ban for each account
        for modaction in self.modactions:
            details = modaction['details']
            author = modaction['target.author']
            created_utc = float(modaction['created.utc'])
            if modaction['action'] == 'banuser':
                if details in ['permanent', 'changed to permanent']:
                    banned.add(author)
                    if author in suspension_ends:
                        del suspension_ends[author]
                elif re.match(re_temporary, details):
                    days = re.match(re_temporary, details).groups()[0]
                    ends = created_utc + 60*60*24*float(days)
                    suspension_ends[author] = ends
                elif re.match(re_changed_temporary, details):
                    days = re.match(re_changed_temporary, details).groups()[0]
                    if author in banned:
                        banned.remove(author)
                    elif author not in suspension_ends:
                        self.logger.warning(
                            "Ban changed to {} days with no previous ban: {} {}".format(
                                days,
                                modaction['target.author'],
                                modaction['created.utc']))
                    ends = created_utc + 60*60*24*float(days)
                    suspension_ends[author] = ends
                else:
                    raise Exception("Unknown details in banuser: {}".format(details))
            elif modaction['action'] == 'unbanuser':
                if author in banned:
                    banned.remove(author)
                if author in suspension_ends:
                    del suspension_ends[author]

        end_utc = self.end_time.timestamp()
        data = dict(
            (author, (
                author in banned
                or (author in suspension_ends and suspension_ends[author] > end_utc)))
            for author in self.usernames)
        
        self.logger.info("  Done")
        return data
