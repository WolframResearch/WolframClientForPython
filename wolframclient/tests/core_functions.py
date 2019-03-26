# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import (composition, flatten, iterate, map,
                                            partition, riffle)
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_composition(self):
        self.assertEqual(composition()(1), 1)
        self.assertEqual(composition(lambda s: s + 2)(1), 3)
        self.assertEqual(composition(lambda s: s + 2, lambda s: s * 3)(1), 9)

    def test_riffle(self):
        self.assertEqual(
            list(riffle(range(5), "/")), [0, "/", 1, "/", 2, "/", 3, "/", 4])

        self.assertEqual(list(riffle([], "/")), [])

    def test_iterate(self):

        self.assertEqual(list(iterate(2, 3, "a", [2, 3])), [2, 3, "a", 2, 3])

    def test_flatten(self):

        self.assertEqual(
            list(flatten(2, 3, "a", [2, 3, str, [5, [10, 89]]])),
            [2, 3, "a", 2, 3, str, 5, 10, 89])

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

        @normalizer.dispatch(object)
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
        normalizer.register(lambda s: 'Goodbye %s' % s, six.text_type)

        self.assertEqual(normalizer('Ric'), 'Goodbye Ric')

        normalizer.unregister(object)
        normalizer.register(lambda s: 5, object)

        self.assertEqual(normalizer(None), 5)

    def test_class_dispatch(self):

        normalizer = Dispatch()

        @normalizer.dispatch(object)
        def implementation(self, o):
            return o

        @normalizer.dispatch(six.text_type)
        def implementation(self, o):
            return 'Hello %s' % o

        @normalizer.dispatch(int)
        def implementation(self, o):
            return o * 2

        @normalizer.dispatch(int, replace_existing=True)
        def implementation(self, o):
            return o * 3

        class Foo(object):
            attr = normalizer.as_method()

        self.assertEqual(Foo().attr('Ric'), 'Hello Ric')
        self.assertEqual(Foo().attr(2), 6)
        self.assertEqual(Foo().attr(None), None)

        @normalizer.dispatch(int, keep_existing=True)
        def implementation(self, o):
            return o * 4

        self.assertEqual(Foo().attr(2), 6)

        # inconsistent option values
        with self.assertRaises(ValueError):

            @normalizer.dispatch(
                int, replace_existing=True, keep_existing=True)
            def implementation(self, o):
                return o * 4

        # already mapped.
        with self.assertRaises(TypeError):

            @normalizer.dispatch(int)
            def implementation(self, o):
                return o * 4
