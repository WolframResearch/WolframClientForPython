# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import unittest

from wolframclient.serializers import export
from wolframclient.serializers.encoders.numpy import to_little_endian
from wolframclient.serializers.wxfencoder.serializer import WXFExprSerializer
from wolframclient.serializers.wxfencoder.wxfencoder import DefaultWXFEncoder
from wolframclient.serializers.wxfencoder.wxfexprprovider import (
    WXFExprProvider)
from wolframclient.serializers.wxfencoder.wxfnumpyencoder import (
    NumPyWXFEncoder)
from wolframclient.utils import six
from wolframclient.utils.api import numpy
from wolframclient.utils.encoding import force_text
from wolframclient.utils.tests import TestCase as BaseTestCase


@unittest.skipIf(six.JYTHON, "numpy is not supported in jython")
class TestCase(BaseTestCase):
    @classmethod
    def initDefault(cls):
        return cls.init(True, False)

    @classmethod
    def initBothArraySupport(cls):
        return cls.init(True, True)

    @classmethod
    def initOnlyRA(cls):
        return cls.init(False, True)

    def compare_serializer(self, serializer, array, value, test_export_wxf=False):
        serializer.serialize(array)
        self.assertEqual(serializer._writer.getvalue(), value)

        if test_export_wxf:
            self.assertEqual(export(array, target_format='wxf'), value)

    @staticmethod
    def init(pa, ra):
        expr_provider = WXFExprProvider()
        numpy_encoder = NumPyWXFEncoder(
            packed_array_support=pa, numeric_array_support=ra)

        expr_provider.add_encoder(numpy_encoder)
        expr_provider.add_encoder(DefaultWXFEncoder())
        serializer = WXFExprSerializer(
            six.BytesIO(), expr_provider=expr_provider)
        return serializer

    def test_dimensions(self):
        provider = WXFExprProvider(NumPyWXFEncoder())
        arr = numpy.ndarray([2, 1, 3])
        wxfExpr = next(provider.provide_wxfexpr(arr))
        self.assertEqual(wxfExpr.dimensions, (2, 1, 3))

    def test_zero_dimension(self):
        provider = WXFExprProvider(NumPyWXFEncoder())
        arr = numpy.ndarray([2, 0, 3])
        with self.assertRaises(Exception) as err:
            next(provider.provide_wxfexpr(arr))

        self.assertEqual(
            force_text(err.exception), "Dimensions must be positive integers.")

    def test_int8_PA(self):
        self.compare_serializer(self.initDefault(), numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8),
                                b'\x38\x3a\xc1\x00\x02\x02\x02\x80\xff\x01\x7f')

    def test_int8_Both(self):
        self.compare_serializer(self.initBothArraySupport(),
                                numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8),
                                b'\x38\x3a\xc1\x00\x02\x02\x02\x80\xff\x01\x7f')

    def test_int8_RA(self):
        self.compare_serializer(self.initOnlyRA(), numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8),
                                b'\x38\x3a\xc2\x00\x02\x02\x02\x80\xff\x01\x7f', test_export_wxf=True)

    def test_int16(self):
        s = self.initDefault()
        sRA = self.initOnlyRA()
        arr = numpy.array([[-(1 << 15)], [(1 << 15) - 1]], numpy.int16)

        self.compare_serializer(s, arr, b'8:\xc1\x01\x02\x02\x01\x00\x80\xff\x7f')
        self.compare_serializer(sRA, arr, b'8:\xc2\x01\x02\x02\x01\x00\x80\xff\x7f', test_export_wxf=True)

    def test_int32(self):

        arr = numpy.array([[-(1 << 31)], [(1 << 31) - 1]], numpy.int32)

        self.compare_serializer(self.initDefault(), arr, b'8:\xc1\x02\x02\x02\x01\x00\x00\x00\x80\xff\xff\xff\x7f')
        self.compare_serializer(self.initOnlyRA(), arr, b'8:\xc2\x02\x02\x02\x01\x00\x00\x00\x80\xff\xff\xff\x7f',
                                test_export_wxf=True)

    def test_int64(self):

        arr = numpy.array([[-(1 << 62)], [(1 << 62)]], numpy.int64)
        self.compare_serializer(self.initDefault(), arr,
                                b'8:\xc1\x03\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00@')
        self.compare_serializer(self.initOnlyRA(), arr,
                                b'8:\xc2\x03\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00@')

    def to_little_endian_i2le(self):
        arr=numpy.arange(256, 259, dtype='<i2')
        data = arr.bytes()
        data_le = to_little_endian(arr).bytes()
        self.assertEqual(data, data_le)

    def to_little_endian_i2be(self):
        arr = numpy.arange(256, 259, dtype='>i2')
        data = arr.bytes()
        data_le = to_little_endian(arr).bytes()
        self.assertEqual(data_le, b'\x01\x00\x01\x01\x01\x02')
        self.assertEqual(data, b'\x00\x01\x01\x01\x02\x01')

        to_little_endian(arr, inplace=True)
        self.assertEqual(arr.bytes(), data_le)

    def to_little_endian_float(self):
        arr = numpy.arange(256, 259, dtype='f')
        data = arr.bytes()
        # no-op
        data_le = to_little_endian(arr).bytes()
        self.assertEqual(data_le, data)

    def test_int16_be(self):
        arr = numpy.array([[-(1 << 15)], [(1 << 15) - 1]], numpy.int16)
        wxf = export(arr, target_format='wxf')
        self.assertEqual(wxf, b'8:\xc2\x01\x02\x02\x01\x00\x80\xff\x7f')

    def test_int32_be(self):
        arr = numpy.array([[-(1 << 31)], [(1 << 31) - 1]], dtype='>i4')
        wxf = export(arr, target_format='wxf')
        self.assertEqual(wxf, b'8:\xc2\x02\x02\x02\x01\x00\x00\x00\x80\xff\xff\xff\x7f')


    def test_uint8_PA(self):
        self.compare_serializer(self.initDefault(), numpy.array([[0, (1 << 7)]], numpy.uint8),
                                b'\x38\x3a\xc1\x01\x02\x01\x02\x00\x00\x80\x00')

    def test_uint8_RA(self):
        self.compare_serializer(self.initBothArraySupport(), numpy.array([0, (1 << 8) - 1], numpy.uint8),
                                b'8:\xc2\x10\x01\x02\x00\xff')

    def test_uint16_RA(self):
        self.compare_serializer(self.initBothArraySupport(), numpy.array([0, (1 << 16) - 1], numpy.uint16),
                                b'8:\xc2\x11\x01\x02\x00\x00\xff\xff')

    def test_uint32_RA(self):
        self.compare_serializer(self.initBothArraySupport(), numpy.array([0, (1 << 32) - 1], numpy.uint32),
                                b'8:\xc2\x12\x01\x02\x00\x00\x00\x00\xff\xff\xff\xff')

    def test_uint64_RA(self):
        self.compare_serializer(self.initBothArraySupport(), numpy.array([0, (1 << 64) - 1], numpy.uint64),
                                b'8:\xc2\x13\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff')

    def test_numpy_float16(self):
        f16 = numpy.float16('1.234e-3')
        self.assertEqual(export(f16), b'0.0012340545654')

    def test_numpy_float32(self):
        f32 = numpy.float32('1.2345678e-1')
        self.assertEqual(export(f32), b'0.1234567835927')

    def test_numpy_float64(self):
        f64 = numpy.float64('-1.234567891234e-1')
        self.assertEqual(export(f64), b'-0.1234567891234')

    def test_numpy_float128(self):
        f128 = numpy.float128('1.23e-1234')
        self.assertEqual(
            export(f128), b'Times[0.5138393296553, Power[2, -4098.]]')

    def test_numpy_integers(self):
        int8 = [numpy.int8(127), numpy.int8(-128)]
        self.assertEqual(export(int8), b'{127, -128}')

        uint8 = [numpy.uint8(0), numpy.uint8(255)]
        self.assertEqual(export(uint8), b'{0, 255}')

        int16 = [numpy.int16(32767), numpy.int16(-32768)]
        self.assertEqual(export(int16), b'{32767, -32768}')

        ints = [
            numpy.uint16(65535),
            numpy.int32(-2147483648),
            numpy.uint32(4294967296),
            numpy.int64(-9223372036854775808),
            numpy.uint64(18446744073709551615)
        ]
        self.assertEqual(
            export(ints),
            b'{65535, -2147483648, 0, -9223372036854775808, 18446744073709551615}'
        )

    def test_bad_options(self):
        with self.assertRaises(ValueError):
            NumPyWXFEncoder(
                packed_array_support=False, numeric_array_support=False)
