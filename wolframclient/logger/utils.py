# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import logging

def setup_logging_to_file(path, level=logging.INFO):
    """Setup a basic Python logging configuration to a given file."""
    logging.basicConfig(filename='/tmp/python_testsuites.log',
        filemode='a',
        format='%(asctime)s, %(name)s %(levelname)s %(message)s',
        level=level
    )
