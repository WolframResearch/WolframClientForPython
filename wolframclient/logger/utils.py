# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.utils.six import PY2
import logging

if PY2:
    def setup_logging_to_file(path, level=logging.INFO):
        logging.basicConfig(filename=path,
            filemode='a',
            format='%(asctime)s, %(name)s %(levelname)s %(message)s',
            level=level
        )
else:
    def setup_logging_to_file(path, level=logging.INFO):
        """Setup a basic Python logging configuration to a given file."""
        logging.basicConfig(
            format=u'%(asctime)s, %(name)s %(levelname)s %(message)s',
            handlers=[logging.FileHandler(path, 'a', 'utf-8')],
            level=level
        )
