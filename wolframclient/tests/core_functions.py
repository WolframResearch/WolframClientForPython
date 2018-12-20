# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import math

from wolframclient.utils import six
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import partition, riffle, iterate, flatten
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):

    def test_riffle(self):
        self.assertEqual(
            list(riffle(range(5), "/")), [0, "/", 1, "/", 2, "/", 3, "/", 4])

        self.assertEqual(list(riffle([], "/")), [])

    def test_iterate(self):

        self.assertEqual(
            list(iterate(2, 3, "a", [2, 3])),
            [2, 3, "a", 2, 3]
        )

    def test_flatten(self):

        self.assertEqual(
            list(flatten(2, 3, "a", [2, 3, str, [5, [10, 89]]])),
            [2, 3, "a", 2, 3, str, 5, 10, 89]
        )

    def test_partition(self):

        self.assertEqual(
            list(partition(range(10), 5)), [(0, 1, 2, 3, 4), (5, 6, 7, 8, 9)])

        self.assertEqual(
            list(partition(range(10), 3)), [(0, 1, 2), (3, 4, 5), (6, 7, 8),
                                            (9, )])

        self.assertEqual(list(partition([], 3)), [])

    def test_dispatch(self):

        normalizer = Dispatch()

        class Person(object):
            pass

        class SportPlayer(Person):
            pass

        class FootballPlayer(SportPlayer):
            pass


        @normalizer.default()
        def implementation(o):
            return o

        @normalizer.dispatch((int, float))
        def implementation(o):
            return o * 2

        @normalizer.dispatch(six.text_type)
        def implementation(o):
            return 'Hello %s' % o

        @normalizer.dispatch(Person)
        def implementation(o):
            return 'Hello person'

        @normalizer.dispatch(FootballPlayer)
        def implementation(o):
            return 'Hello football player'

        self.assertEqual(normalizer('Ric'), 'Hello Ric')
        self.assertEqual(normalizer(2), 4)
        self.assertEqual(normalizer(None), None)

        self.assertEqual(normalizer(Person()), 'Hello person')
        self.assertEqual(normalizer(SportPlayer()), 'Hello person')
        self.assertEqual(normalizer(FootballPlayer()), 'Hello football player')


        normalizer.unregister(six.text_type)
        normalizer.register(six.text_type, lambda s: 'Goodbye %s' % s)

        self.assertEqual(normalizer('Ric'), 'Goodbye Ric')

    def test_class_dispatch(self):

        normalizer = Dispatch()

        @normalizer.default()
        def implementation(self, o):
            return o

        @normalizer.dispatch(six.text_type)
        def implementation(self, o):
            return 'Hello %s' % o

        @normalizer.dispatch(int)
        def implementation(self, o):
            return o * 2

        class Foo(object):
            attr = normalizer.as_method()

        self.assertEqual(Foo().attr('Ric'), 'Hello Ric')
        self.assertEqual(Foo().attr(2), 4)
        self.assertEqual(Foo().attr(None), None)
