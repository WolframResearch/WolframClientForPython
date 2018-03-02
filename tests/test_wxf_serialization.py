# -*- coding: utf-8 -*-
import unittest

from wxfserializer.utils import six

import os

from wxfserializer.wxfexpr import WXFExprString, WXFExprInteger, WXFExprReal, WXFExprBinaryString
from wxfserializer.wxfencoder import DefaultWXFEncoder
from wxfserializer.wxfexprprovider import WXFExprProvider
from wxfserializer.wxfdataconsumer import InMemoryWXFDataConsumer
from wxfserializer.serializer import WXFExprSerializer, write_varint

def init():
    expr_provider = WXFExprProvider()
    data_consumer = InMemoryWXFDataConsumer()
    serializer = WXFExprSerializer(expr_provider, data_consumer)
    return (serializer, data_consumer)


class SerializeTest(unittest.TestCase):
    def serialize_compare(self, pythonExpr, expected_wxf):
        serializer, dataconsumer = init()
        serializer.serialize(pythonExpr)
        self.assertSequenceEqual(dataconsumer.data(), expected_wxf)

    def path_to_file_in_data_dir(self, file_name):
        current_file_dir = os.path.dirname(__file__)
        return os.path.join(current_file_dir, 'data', file_name)


class TestVarint(unittest.TestCase):
    def test_zero(self):
        buffer = bytearray()
        write_varint(0, buffer)
        self.assertSequenceEqual(buffer, bytearray([0x00]))
    
    def test_one_byte(self):
        buffer = bytearray()
        write_varint(127, buffer)
        self.assertSequenceEqual(buffer, bytearray([127]))

    def test_two_bytes(self):
        buffer = bytearray()
        write_varint(128, buffer)
        self.assertSequenceEqual(
            buffer, bytearray([0x80, 0x01]))

        buffer = bytearray()
        write_varint((1<<(7*2))-1, buffer)
        self.assertSequenceEqual(
            buffer, bytearray([0xFF, 0x7F]))

    def test_three_bytes(self):
        buffer = bytearray()
        write_varint((1 << (7 * 2)), buffer)
        self.assertSequenceEqual(
            buffer, bytearray([0x80, 0x80, 0x01]))
        
        buffer = bytearray()
        write_varint((1 << (7 * 3)) - 1, buffer)
        self.assertSequenceEqual(
            buffer, bytearray([0xFF, 0xFF, 0x7F]))

    def test_max_bytes(self):
        buffer = bytearray()
        write_varint((1 << (7 * 8)), buffer)
        self.assertSequenceEqual(
            buffer, bytearray([0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01]))

        buffer = bytearray()
        write_varint((1 << (7 * 9)) - 1, buffer)
        self.assertSequenceEqual(
            buffer, bytearray([0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F]))


class TestWXFString(SerializeTest):
    def testBasicString(self):
        value = u"maître & élève"
        wxfExpr = WXFExprString(value)
        self.assertEqual(wxfExpr.value, value.encode(encoding='utf-8'))

    def testBasicStringOfBytes(self):
        value = u"maître & élève"
        wxfExpr = WXFExprString(value)
        self.assertEqual(wxfExpr.value, value.encode(encoding='utf-8'))

    def testWrongStringType(self):
        value = 1
        with self.assertRaises(TypeError):
            WXFExprString(value)

        value = [1,2,3]
        with self.assertRaises(TypeError):
            WXFExprString(value)

        value = {1 : 'foo'}
        with self.assertRaises(TypeError):
            WXFExprString(value)

        value = bytearray([1,2,3])
        with self.assertRaises(TypeError):
            WXFExprString(value)

    def testBinaryString(self):
        value = bytearray([1,2,3])
        wxfExpr = WXFExprBinaryString(value)
        self.assertSequenceEqual(wxfExpr.data, value)

    def test_unicode(self):
        value = u'élève'
        wxf = b'\x38\x3a\x53\x07\xc3\xa9\x6c\xc3\xa8\x76\x65'
        self.serialize_compare(value, wxf)

    def test_max_3bytes_unicode(self):
        max = (1<<16)-1
        if six.PY2:
            value = unichr(max)
        else:
            value = chr(max)
        wxf = b'\x38\x3a\x53\x03\xef\xbf\xbf'
        self.serialize_compare(value, wxf)

    def test_high_surrogates(self):
        values = [0xD800, 0xDB7F, 0xDC00, 0xDFFF]
        wxf_outs = [
            b'\x38\x3a\x53\x03\xed\xa0\x80',
            b'\x38\x3a\x53\x03\xed\xad\xbf',
            b'\x38\x3a\x53\x03\xed\xb0\x80',
            b'\x38\x3a\x53\x03\xed\xbf\xbf'
        ]
        # PY2 does not care about high surrogate, so does the WL.
        # PY3 on the other hand raises exception when it encounters one.
        for val, wxf in zip(values, wxf_outs):
            if six.PY2:
                self.serialize_compare(unichr(val), wxf)
            else:
                with self.assertRaises(UnicodeEncodeError):
                    self.serialize_compare(chr(val), wxf)
        

class TestWXFInteger(unittest.TestCase):
    ''' Mostly useful for Python 2.7. Otherwise we test int.to_bytes.
    '''
    def test_int8(self):
        values = [0, 1, 127, -1, -128]
        res = [0, 1, 127, 255, 128]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertEqual(wxf_expr.to_bytes()[0], res[i])

    def test_int16(self):
        values = [-(1 << 15), (1 << 15) - 1]
        res = [(0x00, 0x80), (0xFF, 0x7F)]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertSequenceEqual(wxf_expr.to_bytes(), res[i])

    def test_int32(self):
        values = [-(1 << 31), (1 << 31) - 1]
        res = [(0x00, 0x00, 0x00, 0x80),
               (0xFF, 0xFF, 0xFF, 0x7F)]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertSequenceEqual(wxf_expr.to_bytes(), res[i])

    def test_int64(self):
        values = [-(1 << 63), (1 << 63) - 1]
        res = [(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80),
               (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F)]
        for i in range(0, len(values)):
            wxf_expr = WXFExprInteger(values[i])
            self.assertSequenceEqual(wxf_expr.to_bytes(), res[i])

class TestWXFReal(SerializeTest):
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


class TestLists(SerializeTest):
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

class TestAssociation(SerializeTest):
    def test_simple_dic(self):
        value = {1:2}
        wxf = b'\x38\x3a\x41\x01\x2d\x43\x01\x43\x02'
        self.serialize_compare(value, wxf)
    
    def test_empty_dic(self):
        wxf = b'\x38\x3a\x41\x00'
        self.serialize_compare({}, wxf)

    def test_empty_dics(self):
        value = {'k' : {1:2, 3:4}, 'e':{}}
        wxf = b'\x38\x3a\x41\x02\x2d\x53\x01\x6b\x41\x02\x2d\x43\x01\x43\x02\x2d\x43\x03\x43\x04\x2d\x53\x01\x65\x41\x00'
        self.serialize_compare(value, wxf)

class MixingAll(SerializeTest):
    def test_all_char(self):
        import itertools
        all_char = u''
        unicode_no_surrogate = itertools.chain(
            range(0xD800),
            range(1 + 0xDFFF, 1<<16))
        count=0
        for i in unicode_no_surrogate:
            count+=1
            if six.PY2:
                all_char += unichr(i)
            else:
                all_char += chr(i)

        with open(self.path_to_file_in_data_dir('allchars.wxf'), 'rb') as r_file:
            ba = bytearray(r_file.read())
        self.serialize_compare(all_char, ba)        

    def test_default(self):
        expr_provider = WXFExprProvider(default=list)
        data_consumer = InMemoryWXFDataConsumer()
        serializer = WXFExprSerializer(expr_provider, data_consumer)
        serializer.serialize(range(1, 4))
        wxf = b'\x38\x3a\x66\x03\x73\x04\x4c\x69\x73\x74\x43\x01\x43\x02\x43\x03'
        self.assertSequenceEqual(data_consumer.data(),wxf)
        