from datetime import datetime, timedelta
import csv
import json
import logging
import math
import os
import pytz
from tqdm import tqdm

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

class DataSource:
    """Base class for data sources used to extract and validate DataFiles."""

    def __init__(self):
        self.resume = None
        self.resumed = False
        self.processed = False
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.exclude_columns = []

    def columns(self):
        return [
            column for column, column_type in self.column_types().items()
            if column not in self.exclude_columns
        ]

    def exclude_column(column):
        self.exclude_columns.append(column)
        
    def label(self):
        raise NotImplementedError

    def column_types(self):
        raise NotImplementedError

    def connect(self):
        raise NotImplementedError
    
    def extract(self):
        raise NotImplementedError

    def resume(self, resume):
        """Resume an extraction starting from where a previous extraction left off.
    
        PARAMETERS
        ----------
        `resume`: The filename of the previous extraction data.
        """
        if self.resumed or self.processed:
            raise ValueError("Cannot resume: source has already resumed or processed data")
        self.resume = resume

        
class DataFile:
    def __init__(self, Source, directory=None, exclude_cols=None, prefix=None):
        """Extracts, writes, and reads tabular data files.
    
        This class can be used to extract data from a `DataSource` and automatically write it
        to a tabular data file. It can also be used to validate and read tabular data, either
        one record at a time, or entirely.
    
        The `extract()` method is used to extract data from a `DataSource`. Data is extrated
        one record at a time and automatically written to a file. If `load` is `True`,
        the rows are cached in memory as well.
    
        The `transform()` method works similarly, but takes data from previously extracted
        `DataFiles` and produces a new `DataFile` with new columns derived from previous data.
    
        Once a `DataFile` has been extracted, transformed, or replayed from a file, the
        data can be read using `rows()`, which returns an iterator over the records. When a
        DataFile reproduced from a file, it is not automatically loaded into memory unless
        `load` is `True`.
    
        PARAMETERS
        ----------
        `Source`: The `DataSource` subclass used to extract, transform, or validate the data.
        
        `directory`: The directory to read/write the data to/from (default `extracted/`).

        `prefix`: The prefix to use when creating a file.

        """
        self.logger = logging.getLogger('CivilServant-Analysis')
        self.source = Source()
        self.directory = directory
        self.prefix = prefix
        self.loaded_rows = []
        self.loaded = False
        self.script_date = datetime.now().strftime('%Y-%m-%d')
        self.exclude_rows = None

    def replay(self, filename, load=False, exclude_rows=None):  
        """
        filename: Records will be read from the specified filename (default `None`).
        
        load: Whether to automatically cache records in memory (default `False`).
        
        exclude_rows: When reading a file, records will only be included if `exclude_row(row)`
            is False (default: `None`).
        """
        self.filename = filename
        self.load = load
        self.exclude_rows = exclude_rows
        if load:
            self.loaded_rows = list(self._load())
        return self
            
    def create_filename(self):
        parts = [self.script_date]
        if self.prefix is not None:
            parts.append("prefix")
        parts.append(self.source.label())
        return "-".join(parts)        

    def _process(self, directory, load):
        """Helper called by extract(), transform(), etc."""
        if self.loaded:
            raise RuntimeError("DataFile already loaded, cannot be modified")

        # Open a new file and write each row
        self.filename = self.create_filename()
        self.logger.info("  Opening for writing: {}".format(self.filename))

        if self.directory is not None:
            filename = os.path.join(self.directory, directory, self.filename)
        else:
            filename = os.path.join(directory, self.filename)
        with open(filename, 'w', encoding='utf-8') as f:
            writer = csv.DictWriter(
                f, self.source.columns(), delimiter='\t', quoting=csv.QUOTE_MINIMAL)
            writer.writeheader()

            # Extract data from the DataSource
            count = 0
            for datum in self.source.extract():
                writer.writerow(datum)
                count += 1
                if load:
                    self.loaded_rows.append(datum)            
        if load:
            self.loaded = True
            
        self.logger.info("Closed file: {}".format(self.filename))
        self.logger.info("  {} records written".format(count))
        return self
    
    def extract(self, load=False):
        self.logger.info("Extracting")
        self._process("extracted", load)
        return self
        
    def transform(self, load=False):
        self.logger.info("Transforming")
        self._process("transformed", load)
        return self
        
    def _load(self):
        if self.loaded:
            self.logger.warning("File {} already loaded".format(self.filename))
            return self.loaded_rows
        self.logger.info("Opening {} for reading".format(self.filename))
        count = 0
        excluded = 0
        self.loaded_rows = []
        if self.directory is None:
            filename = self.filename
        else:
            filename = os.path.join(self.directory, self.filename)
        with open(filename, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f, delimiter='\t')
            for row in reader:
                if self.exclude_rows is not None:
                    if self.exclude_rows(row):
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
