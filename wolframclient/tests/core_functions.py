# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.tests.utils.base import TestCase as BaseTestCase
from wolframclient.utils.functional import partition, riffle

class TestCase(BaseTestCase):

    def test_riffle(self):
        self.assertEqual(
            list(riffle(range(5), "/")),
            [0, "/", 1, "/", 2, "/", 3, "/", 4]
        )

        self.assertEqual(
            list(riffle([], "/")),
            []
        )

    def test_partition(self):

        self.assertEqual(
            list(partition(range(10), 5)),
            [(0, 1, 2, 3, 4), (5, 6, 7, 8, 9)]
        )

        self.assertEqual(
            list(partition(range(10), 3)),
            [(0, 1, 2), (3, 4, 5), (6, 7, 8), (9,)]
        )

        self.assertEqual(
            list(partition([], 3)),
            []
        )