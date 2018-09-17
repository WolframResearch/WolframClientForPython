# -*- coding: utf-8 -*-
import json
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils.api import os
__all__ = [ 'create_dir_if_missing', 'dir_test_data', 'json_config' ]

def create_dir_if_missing(path):
    ''' Create the directory structure represented by a `path`.
    
    Path can represent a file or a directory depending if it ends with
    a path separator.
    
    If the path represents a file, the parent directory is created, otherwise
    the directory itself is created.
    '''
    file_dir = os.dirname(path)
    if not os.exists(file_dir):
        os.makedirs(file_dir)

def dir_test_data():
    ''' Return path to the data directory in tests'''
    current_file_dir = os.dirname(__file__)
    return os.path_join(current_file_dir, 'data')


log_file = os.environ.get('WOLFRAMCLIENT_PY_LOG_FILE', None)
if log_file is not None:
    create_dir_if_missing(log_file)
    setup_logging_to_file(log_file)

json_config=None
json_config_path = os.environ.get('WOLFRAMCLIENT_PY_JSON_CONFIG', None)
if json_config_path is not None:
    expended_path = os.expanduser(os.expandvars(json_config_path))
    try:
        with open(expended_path, 'r') as fp:
            json_config = json.load(fp)
    except:
        raise ValueError('Failed to find json configuration file %s' % json_config_path)
