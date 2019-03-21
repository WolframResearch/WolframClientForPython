# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language import wl
from wolframclient.utils.externalevaluate import execute_from_string
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_execute_from_string(self):

        context = {}

        result = execute_from_string("a = 2+2", context)

        self.assertEqual(result, wl.Null)
        self.assertEqual(context.get('a', None), 4)

        result = execute_from_string("a", context)

        self.assertEqual(result, 4)

        result = execute_from_string("z = a + 4\nz", context)

        self.assertEqual(result, 8)
        self.assertEqual(context.get('a', None), 4)
        self.assertEqual(context.get('z', None), 8)
