# -*- coding: utf-8 -*-
import unittest
import numpy

from wxfserializer.wxfnumpyencoder import NumPyWXFEncoder
from wxfserializer.wxfencoder import WXFEncoder, DefaultWXFEncoder
from wxfserializer.wxfexprprovider import WXFExprProvider
from wxfserializer.wxfdataconsumer import WXFDataConsumer, InMemoryWXFDataConsumer
from wxfserializer.serializer import WXFExprSerializer

class TestNumpySerialization(unittest.TestCase):
    @staticmethod
    def initDefault():
        return TestNumpySerialization.init(True, False)

    @staticmethod
    def initBothArraySupport():
        return TestNumpySerialization.init(True, True)

    @staticmethod
    def initOnlyRA():
        return TestNumpySerialization.init(False, True)


    @staticmethod
    def init(pa, ra):
        expr_provider = WXFExprProvider()
        numpy_encoder = NumPyWXFEncoder(
            packed_array_support=pa, rawarray_support=ra)

        expr_provider.add_encoder(numpy_encoder)
        expr_provider.add_encoder(DefaultWXFEncoder())
        data_consumer = InMemoryWXFDataConsumer()
        serializer = WXFExprSerializer(expr_provider, data_consumer)
        return (serializer, data_consumer)

    def test_dimensions(self):
        provider = WXFExprProvider(NumPyWXFEncoder())
        arr = numpy.ndarray([2,1,3])
        wxfExpr = next(provider.provide_wxfexpr(arr))
        self.assertEqual(wxfExpr.dimensions, (2, 1, 3) )

    def test_zero_dimension(self):
        provider = WXFExprProvider(NumPyWXFEncoder())
        arr = numpy.ndarray([2, 0, 3])
        with self.assertRaises(Exception) as err:
            next(provider.provide_wxfexpr(arr))

        self.assertEqual(str(err.exception), "Dimensions must be positive integers.")

    def test_int8_PA(self):
        s, d = TestNumpySerialization.initDefault()
        arr = numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8)
        s.serialize(arr)
        self.assertEqual(
            d.data(), b'\x38\x3a\xc1\x00\x02\x02\x02\x80\xff\x01\x7f')

    def test_int8_Both(self):
        s, d = TestNumpySerialization.initBothArraySupport()
        arr = numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8)
        s.serialize(arr)
        self.assertEqual(
            d.data(), b'\x38\x3a\xc1\x00\x02\x02\x02\x80\xff\x01\x7f')

    def test_int8_RA(self):
        s, d = TestNumpySerialization.initOnlyRA()
        arr = numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8)
        s.serialize(arr)
        self.assertEqual(
            d.data(), b'\x38\x3a\xc2\x00\x02\x02\x02\x80\xff\x01\x7f')

    def test_uint8_PA(self):
        s, d = TestNumpySerialization.initDefault()
        arr = numpy.array([[0,(1 << 7)]], numpy.uint8)
        s.serialize(arr)
        self.assertEqual(
            d.data(), b'\x38\x3a\xc1\x01\x02\x01\x02\x00\x00\x80\x00')

    def test_uint8_RA(self):
        s, d = TestNumpySerialization.initBothArraySupport()
        arr = numpy.array([[0, (1 << 7)]], numpy.uint8)
        s.serialize(arr)
        self.assertEqual(
            d.data(), b'\x38\x3a\xc2\x10\x02\x01\x02\x00\x80')
