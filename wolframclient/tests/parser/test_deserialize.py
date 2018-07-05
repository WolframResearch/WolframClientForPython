# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.deserializer.wxfparser import WXFToken
from wolframclient.serializers.wxfencoder.serializer import write_varint
from wolframclient.deserializer.wxfparser import parse_varint
from wolframclient.utils import six
class TestCase(BaseTestCase):
    def test_token_dimensions(self):
        token = WXFToken(None)
        token.dimensions = [1, 2, 3]
        self.assertEqual(token.element_count, 6)

        token.dimensions = [2, 2, 3]
        self.assertEqual(token.element_count, 12)
    
    def test_token_dimensions_negative(self):
        token = WXFToken(None)
        with self.assertRaises(TypeError):
            token.dimensions = [2, -2, 3]
            token.element_count
    
    def test_token_dimensions_notint(self):
        token = WXFToken(None)
        with self.assertRaises(TypeError):
            token.dimensions = [2, 2.1, 3]
            token.element_count

    def varint_round_trip_integer(self, int_value):
        buffer = six.BytesIO()
        write_varint(int_value, buffer)
        buffer.seek(0)
        self.assertEqual(parse_varint(buffer), int_value)

    def test_varint_parsing(self):
        self.varint_round_trip_integer(0)
        self.varint_round_trip_integer(127)
        self.varint_round_trip_integer(128)
        self.varint_round_trip_integer((1 << (7 * 2)) - 1)
        self.varint_round_trip_integer((1 << (7 * 2)))
        self.varint_round_trip_integer((1 << (7 * 3)) - 1)
        self.varint_round_trip_integer((1 << (7 * 8)))
        self.varint_round_trip_integer((1 << (7 * 9)) - 1)
