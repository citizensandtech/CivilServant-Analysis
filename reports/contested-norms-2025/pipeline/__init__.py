from datetime import datetime, timedelta
import csv
import json
import logging
import math
import os
import pytz
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

utc = pytz.UTC

def _bool(s):
    """Parse python's string representations of True/False values."""
    if s == 'True':
        return True
    elif s == 'False':
        return False
    elif s == '':
        return None
    else:
        raise ValueError

class DataFile:
    """Extracts, writes, and reads tabular data files.

    This class can be used to extract data from a `DataSource` and automatically write it
    to a tabular data file. It can also be used to validate and read tabular data, either
    one record at a time, or entirely.

    The `extract()` method is used to extract data from a `DataSource`. Data is extrated
    one record at a time and automatically written to a file. If `load` is `True`,
    the rows are cached in memory as well.

    The `transform()` method works similarly, but takes data from previously extracted
    `DataFiles` and produces a new `DataFile` with new columns derived from previous data.

    Once a `DataFile` has been extracted, transformed, or specified using `filename`, the
    data can be read using `rows()`, which returns an iterator over the records. When a
    file is specified using `filename`, it is not automatically loaded into memory unless
    `load` is `True`.

    PARAMETERS
    ----------
    source: The `DataSource` used to extract, transform, or validate the data.
    
    directory: The directory to store the output (default `extracted/`).
    
    filename: Records will be read from the specified filename (default `None`).
    
    load: Whether to automatically cache records in memory (default `False`).
    
    exclude: When reading a file, records will only be included if `exclude(row)`
        is False (default: `None`).
    """
    def __init__(self, source, directory=None, filename=None, load=False, exclude=None):
        self.source = source
        self.filename = filename
        self.logger = logging.getLogger('CivilServant-Analysis')
        if directory is not None:
            self.directory = directory
        else:
            self.directory = "extracted" 
        if filename is None:
            self.script_date = datetime.now().strftime('%Y-%m-%d')
        self.loaded_rows = None
        self.loaded = False
        self.exclude = exclude
        if filename is not None and load:
            self.loaded_rows = list(self._load())

    def extract(self, load=False):
        if self.loaded:
            raise RuntimeError("DataFile already loaded, cannot be extracted")

        # Open a new file and write each row
        self.filename = "{}-{}.tsv".format(self.script_date, self.source.label())
        self.logger.info("Extracting dataset")
        self.logger.info("  Opening for writing: {}".format(self.filename))
        if load:
            self.loaded_rows = []
        with open(os.path.join("extracted", self.filename), 'w', encoding='utf-8') as f:
            writer = csv.DictWriter(
                f, self.source.columns(), delimiter='\t', quoting=csv.QUOTE_MINIMAL)
            writer.writeheader()
            for datum in self.source.extract():
                writer.writerow(datum)
                if load:
                    self.loaded_rows.append(datum)
        if load:
            self.loaded = True
        self.logger.info("Closed file: {}".format(self.filename))

    def transform(self, load=False):
        if self.loaded:
            raise RuntimeError("DataFile already loaded, cannot be transformed")

        # Open a new file and write each row
        self.filename = "{}-{}.tsv".format(self.script_date, self.source.label())
        self.logger.info("Transforming datasets")
        self.logger.info("  Opening for writing: {}".format(self.filename))
        if load:
            self.loaded_rows = []
        with open(os.path.join("transformed", self.filename), 'w', encoding='utf-8') as f:
            writer = csv.DictWriter(
                f, self.source.columns(), delimiter='\t', quoting=csv.QUOTE_MINIMAL)
            writer.writeheader()
            for datum in self.source.transform():
                writer.writerow(datum)
                if load:
                    self.loaded_rows.append(datum)
        if load:
            self.loaded = True
        self.logger.info("Closed file: {}".format(self.filename))
        
    def link(self, load=False):
        if self.loaded:
            raise RuntimeError("DataFile already loaded, cannot be linked")

        # Open a new file and write each row
        self.filename = "{}-{}.tsv".format(self.script_date, self.source.label())
        self.logger.info("Linking datasets")
        self.logger.info("  Opening for writing: {}".format(self.filename))
        if load:
            self.loaded_rows = []
        with open(os.path.join("linked", self.filename), 'w', encoding='utf-8') as f:
            writer = csv.DictWriter(
                f, self.source.columns(), delimiter='\t', quoting=csv.QUOTE_MINIMAL)
            writer.writeheader()
            for datum in self.source.link():
                writer.writerow(datum)
                if load:
                    self.loaded_rows.append(datum)
        if load:
            self.loaded = True
        self.logger.info("Closed file: {}".format(self.filename))
        
    def _load(self):
        if self.loaded:
            self.logger.warning("File {} already loaded".format(self.filename))
            return self.loaded_rows
        self.logger.info("Opening {} for reading".format(self.filename))
        count = 0
        excluded = 0
        self.loaded_rows = []
        with open(os.path.join(self.directory, self.filename), 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f, delimiter='\t')
            for row in reader:
                if self.exclude is not None:
                    if self.exclude(row):
                        excluded += 1
                        continue
                count += 1
                typed_row = {}
                for column, type in self.source.column_types().items():
                    try:
                        typed_row[column] = type(row[column])
                    except ValueError as err:
                        if row[column] == '':
                            typed_row[column] = None
                        else:
                            raise err
                self.loaded_rows.append(typed_row)
                yield(typed_row)
        self.loaded = True
        self.logger.info("  Done")
        self.logger.info("  Included {} rows".format(count))
        self.logger.info("  Excluded {} rows".format(excluded))
        
    def rows(self):
        if self.loaded:
            for row in self.loaded_rows:
                yield row
        else:
            for row in self._load():
                yield row
