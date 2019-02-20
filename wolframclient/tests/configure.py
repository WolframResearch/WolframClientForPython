# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import json
import logging

from wolframclient.utils import six
from wolframclient.utils.api import os
from wolframclient.utils.logger import setup_logging_to_file

logger = logging.getLogger(__name__)

__all__ = [
    'create_dir_if_missing', 'dir_test_data', 'json_config',
    'secured_authentication_key', 'user_configuration', 'server', 'kernel_path'
]


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


def _parse_config(config):
    sak = None
    user_cred = None
    server = None
    kernel_path = None
    try:
        cloud_config = config['cloud_credentials']
        try:
            from wolframclient.evaluation import SecuredAuthenticationKey
            sak = SecuredAuthenticationKey(
                cloud_config['SAK']['consumer_key'],
                cloud_config['SAK']['consumer_secret'])
        except KeyError as e:
            logger.warning('Failed to read SAK from json config.', e)
        from wolframclient.evaluation import UserIDPassword
        if 'User' in cloud_config:
            user_cred = UserIDPassword(cloud_config['User']['id'],
                                       cloud_config['User']['password'])
        else:
            user_cred = None
    except KeyError as e:
        logger.warning('Failed to parse json config.', e)
    try:
        from wolframclient.evaluation import WolframServer
        server_json = config['server']
        server = WolframServer(
            server_json['host'],
            server_json['request_token_endpoint'],
            server_json['access_token_endpoint'],
            xauth_consumer_key=server_json.get('xauth_consumer_key', None),
            xauth_consumer_secret=server_json.get('xauth_consumer_secret',
                                                  None),
            certificate=server_json.get('certificate', None))
    except KeyError as e:
        logger.warning('Failed to parse json config.', e)

    kernel_path = json_config.get('kernel', None)
    return sak, user_cred, server, kernel_path


log_file = os.environ.get('WOLFRAMCLIENT_PY_LOG_FILE', None)
if log_file is not None:
    create_dir_if_missing(log_file)
    setup_logging_to_file(log_file)

json_config = None
secured_authentication_key = None
user_configuration = None
server = None
kernel_path = None

_json_config_path = os.environ.get('WOLFRAMCLIENT_PY_JSON_CONFIG', None)
if six.PY_35 and _json_config_path is not None:
    expended_path = os.expanduser(os.expandvars(_json_config_path))
    try:
        with open(expended_path, 'r') as fp:
            json_config = json.load(fp)
            secured_authentication_key, user_configuration, server, kernel_path = _parse_config(
                json_config)
    except IOError:
        raise ValueError(
            'Failed to find json configuration file %s' % _json_config_path)

MSG_JSON_NOT_FOUND = "Environment variable WOLFRAMCLIENT_PY_JSON_CONFIG not set."
