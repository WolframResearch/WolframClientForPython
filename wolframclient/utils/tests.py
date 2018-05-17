# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import re
import unittest

class TestCase(unittest.TestCase):

    @classmethod
    def discover_tests(cls, pattern = re.compile('^test_[A-Za-z_0-9-]+$')):
        for attribute in dir(cls):
            if pattern.match(attribute):
                yield attribute