# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.utils import six
from wolframclient.utils.encoding import force_text

if six.PY2:

    def setup_logging_to_file(path, level=None):
        logging.basicConfig(
            filename=path,
            filemode='a',
            format='%(asctime)s, %(name)s %(levelname)s %(message)s',
            level=(level is not None) or logging.INFO)
else:

    def setup_logging_to_file(path, level=None):
        """Setup a basic Python logging configuration to a given file."""
        logging.basicConfig(
            format=u'%(asctime)s, %(name)s %(levelname)s %(message)s',
            handlers=[logging.FileHandler(path, mode='a', encoding='utf-8')],
            level=(level is not None) or logging.INFO)


def str_trim(o, max_char=80):
    """Return the string representation of an object, trimmed to keep up to `max_char` characters.
    """
    as_str = force_text(o)
    if len(as_str) > max_char:
        return '%s...(%i more)' % (as_str[:max_char], len(as_str) - max_char)
    else:
        return as_str
