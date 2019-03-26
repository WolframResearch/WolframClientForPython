# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import export, wolfram_encoder
from wolframclient.utils.tests import TestCase as BaseTestCase


class foo(object):
    pass


class subfoo(foo):
    pass


class subsubfoo(subfoo):
    pass


class subsubfoo2(subfoo):
    pass


class bar(object):
    pass


class TestCase(BaseTestCase):
    @classmethod
    def setUpClass(cls):
        @wolfram_encoder.dispatch(foo)
        def encode_foo(s, o):
            return s.serialize_symbol(b'foo')

        @wolfram_encoder.dispatch(subfoo)
        def encode_subfoo(s, o):
            return s.serialize_symbol(b'subfoo')

        @wolfram_encoder.dispatch(subsubfoo)
        def encode_subsubfoo(s, o):
            return s.serialize_symbol(b'subsubfoo')

    @classmethod
    def tearDownClass(cls):
        wolfram_encoder.unregister((foo, subfoo, subsubfoo))

    def test_encode_parent_class(self):
        wl = export(foo())
        self.assertEqual(wl, b'foo')

    def test_encode_sub_class(self):
        wl = export(subfoo())
        self.assertEqual(wl, b'subfoo')

    def test_encode_sub_sub_class(self):
        wl = export(subsubfoo())
        self.assertEqual(wl, b'subsubfoo')

    def test_encode_sub_sub_class_no_mapping(self):
        wl = export(subsubfoo2())
        self.assertEqual(wl, b'subfoo')

    def test_encode_class_mix(self):
        wl = export([subfoo(), foo(), subsubfoo2()])
        self.assertEqual(wl, b'{subfoo, foo, subfoo}')

    def test_encode_not_registered(self):
        with self.assertRaises(NotImplementedError):
            wl = export(bar())

    def test_register_twice_no_force(self):
        with self.assertRaises(TypeError):

            @wolfram_encoder.dispatch(subsubfoo)
            def encode_subsubfoo_again(s, o):
                return s.serialize_symbol('subsubfooAGAIN')

    def test_register_twice_force(self):
        @wolfram_encoder.dispatch(subsubfoo, replace_existing=True)
        def encode_subsubfoo_again(s, o):
            return s.serialize_symbol('subsubfooFORCE')

        wl = export(subsubfoo())
        self.assertEqual(wl, b'subsubfooFORCE')
