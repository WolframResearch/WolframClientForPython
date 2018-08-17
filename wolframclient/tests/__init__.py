# -*- coding: utf-8 -*-
import os
import json
from wolframclient.logger.utils import setup_logging_to_file

__all__ = [ 'create_dir_if_missing', 'dir_test_data', 'json_config' ]

def create_dir_if_missing(path):
    ''' Create the directory structure represented by a `path`.
    
    Path can represent a file or a directory depending if it ends with
    a path separator.
    
    If the path represents a file, the parent directory is created, otherwise
    the directory itself is created.
    '''
    file_dir = os.path.dirname(path)
    if not os.path.exists(file_dir):
        os.makedirs(file_dir)

def dir_test_data():
    ''' Return path to the data directory in tests'''
    current_file_dir = os.path.dirname(__file__)
    return os.path.join(current_file_dir, 'data')

current_file_dir = os.path.dirname(__file__)
test_log_file = os.path.join(current_file_dir, 'log', 'python_testsuites.log')
create_dir_if_missing(test_log_file)

setup_logging_to_file(test_log_file)

with open(os.path.join(current_file_dir, 'local_config.json'), 'r') as fp:
    json_config = json.load(fp)
