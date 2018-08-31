# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.deserializers import binary_deserialize, WXFToken, WXFConsumer
from wolframclient.deserializers.wxf.wxfparser import parse_varint
from wolframclient.serializers import export
from wolframclient.serializers.wxfencoder.serializer import write_varint
from wolframclient.serializers.wxfencoder import wxfexpr
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.exception import WolframParserException
import unittest


try:
    import numpy
except ImportError:
    numpy = False

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

    def wxf_assert_roundtrip(self, value):
        wxf = export(value, target_format='wxf')
        o = binary_deserialize(wxf)
        self.assertEqual(value, o)
    #Strings
    def testBasicString(self):
        value = u"maître & élève"
        self.wxf_assert_roundtrip(value)

    def testBinaryString(self):
        value = bytearray([0,128,255])
        self.wxf_assert_roundtrip(value)

    def test_max_3bytes_unicode(self):
        max = (1 << 16) - 1
        if six.PY2:
            value = unichr(max)
        else:
            value = chr(max)
        self.wxf_assert_roundtrip(value)

    ### INTEGER TESTS
    @unittest.skipIf(six.JYTHON, None)
    def test_integer8(self):
        value = [0, 1, 127, -1, -128]
        self.wxf_assert_roundtrip(value)

    @unittest.skipIf(six.JYTHON, None)
    def test_int16(self):
        value = [-(1 << 15), (1 << 15) - 1]
        self.wxf_assert_roundtrip(value)

    @unittest.skipIf(six.JYTHON, None)
    def test_int32(self):
        value = [-(1 << 31), (1 << 31) - 1]
        self.wxf_assert_roundtrip(value)

    @unittest.skipIf(six.JYTHON, None)
    def test_int64(self):
        value = [-(1 << 63), (1 << 63) - 1]
        self.wxf_assert_roundtrip(value)

    @unittest.skipIf(six.JYTHON, None)
    def test_bigint_as_int(self):
        value = 10**20
        self.wxf_assert_roundtrip(value)

    def test_real(self):
        value = [1.2345, 0., 1.23456789e100]
        self.wxf_assert_roundtrip(value)

    def test_bigreal(self):
        wxf = b'8:R\x0710.`10.'
        res = binary_deserialize(wxf)
        self.assertTrue(isinstance(res, wxfexpr.WXFExprBigReal))
        self.assertEqual(res.value, b'10.`10.')

    def test_empty_lists(self):
        value = [[], [[]], [1, []], []]
        self.wxf_assert_roundtrip(value)

    def test_simple_dict(self):
        value = {1: 2}
        self.wxf_assert_roundtrip(value)

    def test_empty_dict(self):
        self.wxf_assert_roundtrip({})

    def test_rules(self):
        # BinarySerialize[<|"1" -> 1, "2" -> {0}, "3" -> <||>|>]
        wxf = b'8:A\x03-S\x011C\x01-S\x012f\x01s\x04ListC\x00-S\x013A\x00'
        res = binary_deserialize(wxf)
        self.assertEqual(res, {'1': 1, '2': [0], '3': {}})

    def test_bad_bignum(self):
        # replace last digit by 'A'
        wxf = b'8:I\x171234567890123456789012A'
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf)

    # Numpy arrays
    @unittest.skipIf(not numpy, 'NumPy not found. Skipping numpy tests.')
    def test_numpy(self):
        arr = numpy.array([0,1], 'uint8')
        wxf = export(arr, target_format='wxf')
        res = binary_deserialize(wxf)
        self.assertListEqual(res.tolist(), arr.tolist())

    # Numpy arrays
    @unittest.skipIf(not numpy, 'NumPy not found. Skipping numpy tests.')
    def test_numpy(self):
        arr = numpy.array([[0,1], [1,1], [2,1]], 'uint8')
        wxf = export(arr, target_format='wxf')
        res = binary_deserialize(wxf)
        self.assertListEqual(res.tolist(), arr.tolist())

    # Custom consumers
    class BadGreedyConsumer(WXFConsumer):
        def consume_function(self, current_token, tokens, **kwargs):
            # fetch too many elements
            while True:
                self.next_expression(tokens, **kwargs)

    def test_bad_greedy_consumer(self):
        with self.assertRaises(WolframParserException) as e:
            binary_deserialize(
                export([1, 2, 3], target_format='wxf'), consumer=self.BadGreedyConsumer())
            self.assertEqual(e.msg, 'Input data does not represent a valid expression in WXF format. Expecting more input data.')

    class BadIncompleteConsumer(WXFConsumer):
        def consume_function(self, current_token, tokens, **kwargs):
            # fetch too few elements, head comes first and is not accounted for in length.
            args = []
            for i in range(current_token.length):
                self.next_expression(tokens, **kwargs)

    def test_bad_incomplete_consumer(self):
        with self.assertRaises(WolframParserException) as e:
            binary_deserialize(
                export([1, 2, 3], target_format='wxf'), consumer=self.BadIncompleteConsumer())
            self.assertEqual(e.msg, 'Input data does not represent a valid expression in WXF format. Some expressions are imcomplete.')

