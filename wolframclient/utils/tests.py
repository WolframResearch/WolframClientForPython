# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import os
import unittest


class TestCase(unittest.TestCase):
    pass


HERE = os.path.dirname(__file__)
DATA_DIR = os.path.abspath(os.path.join(HERE, '..', 'tests', 'data'))


def path_to_file_in_data_dir(file_name):
    return os.path.join(DATA_DIR, file_name)
