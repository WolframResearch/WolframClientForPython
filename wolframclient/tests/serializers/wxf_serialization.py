# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal
import unittest
from collections import OrderedDict

from wolframclient.language import wl, wlexpr
from wolframclient.serializers import export
from wolframclient.serializers.wxfencoder.serializer import WXFExprSerializer
from wolframclient.serializers.wxfencoder.utils import (
    float_to_bytes, integer_size, integer_to_bytes, write_varint)
from wolframclient.serializers.wxfencoder.wxfexpr import (
    WXFExprBinaryString, WXFExprInteger, WXFExprString)
from wolframclient.serializers.wxfencoder.wxfexprprovider import (
    WXFExprProvider)
from wolframclient.utils import six
from wolframclient.utils.datastructures import Association
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils.tests import path_to_file_in_data_dir


def init(compress=False, enforce=True):
    expr_provider = WXFExprProvider()
    stream = six.BytesIO()
    serializer = WXFExprSerializer(
        stream, expr_provider=expr_provider, enforce=True, compress=compress)
    return (serializer, stream)


class SerializeTest(BaseTestCase):
    def serialize_compare(self,
                          pythonExpr,
                          expected_wxf,
                          compress=False,
                          enforce=True):
        serializer, stream = init(compress=compress)
        serializer.serialize(pythonExpr)
        self.assertSequenceEqual(stream.getvalue(), expected_wxf)


class TestCase(SerializeTest):
    def test_zero(self):
        buffer = six.BytesIO()
        write_varint(0, buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()), bytearray([0x00]))

    def test_one_byte(self):
        buffer = six.BytesIO()
        write_varint(127, buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()), bytearray([127]))

    def test_two_bytes(self):
        buffer = six.BytesIO()
        write_varint(128, buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()), bytearray([0x80, 0x01]))

        buffer = six.BytesIO()
        write_varint((1 << (7 * 2)) - 1, buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()), bytearray([0xFF, 0x7F]))

    def test_three_bytes(self):
        buffer = six.BytesIO()
        write_varint((1 << (7 * 2)), buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()), bytearray([0x80, 0x80, 0x01]))

        buffer = six.BytesIO()
        write_varint((1 << (7 * 3)) - 1, buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()), bytearray([0xFF, 0xFF, 0x7F]))

    def test_max_bytes(self):
        buffer = six.BytesIO()
        write_varint((1 << (7 * 8)), buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()),
            bytearray([0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01]))

        buffer = six.BytesIO()
        write_varint((1 << (7 * 9)) - 1, buffer)
        self.assertSequenceEqual(
            bytearray(buffer.getvalue()),
            bytearray([0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F]))

    ### STRING TESTS

    def testBasicString(self):
        value = u"maître & élève"
        wxfexpr = WXFExprString(value)
        self.assertEqual(wxfexpr.value, value.encode(encoding='utf-8'))

    def testWrongStringType(self):
        value = 1
        with self.assertRaises(TypeError):
            WXFExprString(value)

        value = [1, 2, 3]
        with self.assertRaises(TypeError):
            WXFExprString(value)

        value = {1: 'foo'}
        with self.assertRaises(TypeError):
            WXFExprString(value)

        value = bytearray([1, 2, 3])
        with self.assertRaises(TypeError):
            WXFExprString(value)

    def testBinaryString(self):
        value = bytearray([0, 128, 255])
        wxfexpr = WXFExprBinaryString(value)
        self.assertSequenceEqual(wxfexpr.data, value)

    def test_unicode(self):
        value = u'élève'
        wxf = b'\x38\x3a\x53\x07\xc3\xa9\x6c\xc3\xa8\x76\x65'
        self.serialize_compare(value, wxf)

    def test_max_3bytes_unicode(self):
        max = (1 << 16) - 1
        if six.PY2:
            value = unichr(max)
        else:
            value = chr(max)
        wxf = b'\x38\x3a\x53\x03\xef\xbf\xbf'
        self.serialize_compare(value, wxf)

    @unittest.skipIf(six.JYTHON, "Different surrogate handling in Jython.")
    def test_high_surrogates(self):
        values = [0xD800, 0xDB7F, 0xDC00, 0xDFFF]
        wxf_outs = [
            b'\x38\x3a\x53\x03\xed\xa0\x80', b'\x38\x3a\x53\x03\xed\xad\xbf',
            b'\x38\x3a\x53\x03\xed\xb0\x80', b'\x38\x3a\x53\x03\xed\xbf\xbf'
        ]
        # PY2 does not care about high surrogate, so does the WL.
        # PY3 on the other hand raises exception when it encounters one.
        for val, wxf in zip(values, wxf_outs):
            if six.PY2:
                self.serialize_compare(unichr(val), wxf)
            else:
                with self.assertRaises(UnicodeEncodeError):
                    self.serialize_compare(chr(val), wxf)

    ### INTEGER TESTS
    ''' Mostly useful for Python 2.7. Otherwise we test int.to_bytes.
    '''

    @unittest.skipIf(six.JYTHON, None)
    def test_int8(self):
        values = [0, 1, 127, -1, -128]
        res = [0, 1, 127, 255, 128]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertEqual(wxf_expr.to_bytes()[0], res[i])

    @unittest.skipIf(six.JYTHON, None)
    def test_int16(self):
        values = [-(1 << 15), (1 << 15) - 1]
        res = [(0x00, 0x80), (0xFF, 0x7F)]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertSequenceEqual(wxf_expr.to_bytes(), res[i])

    @unittest.skipIf(six.JYTHON, None)
    def test_int32(self):
        values = [-(1 << 31), (1 << 31) - 1]
        res = [(0x00, 0x00, 0x00, 0x80), (0xFF, 0xFF, 0xFF, 0x7F)]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertSequenceEqual(wxf_expr.to_bytes(), res[i])

    @unittest.skipIf(six.JYTHON, None)
    def test_int64(self):
        values = [-(1 << 63), (1 << 63) - 1]
        res = [(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80),
               (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F)]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertSequenceEqual(wxf_expr.to_bytes(), res[i])

    @unittest.skipIf(six.JYTHON, None)
    def test_bigint_as_int(self):
        value = 10**20
        with self.assertRaises(ValueError):
            WXFExprInteger(value)

    ### REAL TESTS

    def test_real(self):
        value = 1.2345
        wxf = b'\x38\x3a\x72\x8d\x97\x6e\x12\x83\xc0\xf3\x3f'
        self.serialize_compare(value, wxf)

    def test_real0(self):
        value = 0.
        wxf = b'\x38\x3a\x72\x00\x00\x00\x00\x00\x00\x00\x00'
        self.serialize_compare(value, wxf)

    def test_small_real(self):
        value = 1.07151e-158
        wxf = b'\x38\x3a\x72\x1d\x69\x14\x91\xa1\xd4\x22\x1f'
        self.serialize_compare(value, wxf)

    def test_big_real(self):
        value = 1.23456789e100
        wxf = b'\x38\x3a\x72\x17\x3f\x94\xe8\xd8\x93\xb6\x54'
        self.serialize_compare(value, wxf)

    ### LISTS TESTS

    def test_simple_list(self):
        value = [1, 2, 3]
        wxf = b'\x38\x3a\x66\x03\x73\x04\x4c\x69\x73\x74\x43\x01\x43\x02\x43\x03'
        self.serialize_compare(value, wxf)

    def test_empty_list(self):
        wxf = b'\x38\x3a\x66\x00\x73\x04\x4c\x69\x73\x74'
        self.serialize_compare([], wxf)

    def test_empty_lists(self):
        value = [[], [[]], [1, []], []]
        wxf = b'\x38\x3a\x66\x04\x73\x04\x4c\x69\x73\x74\x66\x00\x73\x04\x4c\x69\x73\x74\x66\x01\x73\x04\x4c\x69\x73\x74\x66\x00\x73\x04\x4c\x69\x73\x74\x66\x02\x73\x04\x4c\x69\x73\x74\x43\x01\x66\x00\x73\x04\x4c\x69\x73\x74\x66\x00\x73\x04\x4c\x69\x73\x74'
        self.serialize_compare(value, wxf)

    ### ASSOCIATION TESTS

    def test_simple_dict(self):

        # Note: Dict must be used with care as the key ordering is not guaranted.
        # One way to make reproducible tests is to use at most one key.

        value = {1: 2}
        wxf = b'8:A\x01-C\x01C\x02'
        self.serialize_compare(value, wxf)

    def test_empty_dict(self):
        wxf = b'\x38\x3a\x41\x00'
        self.serialize_compare({}, wxf)

    def test_dicts(self):
        value = OrderedDict(enumerate('abc'))
        wxf = b'8:A\x03-C\x00S\x01a-C\x01S\x01b-C\x02S\x01c'
        self.serialize_compare(value, wxf)

    def test_no_enforcing_valid(self):
        value = OrderedDict(enumerate('abc'))
        wxf = b'8:A\x03-C\x00S\x01a-C\x01S\x01b-C\x02S\x01c'
        self.serialize_compare(value, wxf, enforce=False)


    @unittest.skipIf(not six.PY2, 'Python2 str test skipped.')
    def test_all_str_py2(self):
        str_all_chr = b''.join([chr(i) for i in range(0,256)])
        wxf = export(str_all_chr, target_format='wxf')
        with open(path_to_file_in_data_dir('allbytes.wxf'), 'rb') as r_file:
            res = bytearray(r_file.read())
        self.assertSequenceEqual(res, wxf)

    ### MIXED TESTS

    def test_all_char(self):
        import itertools
        all_char = u''
        unicode_no_surrogate = itertools.chain(
            range(0xD800), range(1 + 0xDFFF, 1 << 16))
        count = 0
        for i in unicode_no_surrogate:
            count += 1
            if six.PY2:
                all_char += unichr(i)
            else:
                all_char += chr(i)

        with open(path_to_file_in_data_dir('allchars.wxf'), 'rb') as r_file:
            ba = bytearray(r_file.read())
        self.serialize_compare(all_char, ba)

    def test_default(self):
        expr_provider = WXFExprProvider(default=list)
        stream = six.BytesIO()
        serializer = WXFExprSerializer(stream, expr_provider=expr_provider)
        serializer.serialize(range(1, 4))
        wxf = b'\x38\x3a\x66\x03\x73\x04\x4c\x69\x73\x74\x43\x01\x43\x02\x43\x03'
        self.assertSequenceEqual(stream.getvalue(), wxf)

    def test_export(self):

        for value in (1, 2, "aaaa", 2.0, {
                1: 2
        }, [1, 2, 3], ['hello', decimal.Decimal('1.23')], wl.Foo,
                      wl.Foo(2, wl.Context.Internal)):
            self.serialize_compare(value, export(value, target_format='wxf'))

        self.assertEqual(
            export(Association(enumerate('abc')), target_format='wxf'),
            export(
                wl.Association(*(wl.Rule(i, v) for i, v in enumerate('abc'))),
                target_format='wxf'),
        )

        self.assertEqual(
            export(wlexpr("2+2"), target_format='wxf'),
            export(wl.ToExpression("2+2"), target_format='wxf'),
        )

        self.assertEqual(
            export(wl.Foo(wlexpr("2+2"), 1, 2), target_format='wxf'),
            export(wl.Foo(wl.ToExpression("2+2"), 1, 2), target_format='wxf'),
        )

    def test_small_compression(self):
        wxf = b'\x38\x3a\x43\x01'
        self.serialize_compare(1, wxf, compress=False)
        wxf_compressed = b'\x38\x43\x3a\x78\x9c\x73\x66\x04\x00\x00\x89\x00\x45'
        self.serialize_compare(1, wxf_compressed, compress=True)

    def test_string_compression(self):
        wxf = b'\x38\x43\x3a\x78\x9c\x0b\x66\x4e\xcb\xcf\x07\x00\x04\x2f\x01\x9b'
        self.serialize_compare("foo", wxf, compress=True)
