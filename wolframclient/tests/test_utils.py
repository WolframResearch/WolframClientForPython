# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.logger import str_trim
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_str_trim_no_limit(self):
        self.assertEqual(str_trim('abcde'), 'abcde')

    def test_str_trim_above_limit(self):
        self.assertEqual(str_trim('abcde', max_char=3), 'abc...(2 more)')

    def test_str_trim_eq_limit(self):
        self.assertEqual(str_trim('abc', max_char=3), 'abc')
