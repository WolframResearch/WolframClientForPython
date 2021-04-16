# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal
import os

from wolframclient.deserializers import (
    WXFConsumer,
    WXFConsumerNumpy,
    WXFToken,
    binary_deserialize,
)
from wolframclient.deserializers.wxf.wxfparser import parse_varint
from wolframclient.exception import WolframParserException
from wolframclient.serializers import export
from wolframclient.serializers.wxfencoder.utils import write_varint
from wolframclient.tests.configure import skip_for_jython
from wolframclient.utils import six
from wolframclient.utils.api import numpy
from wolframclient.utils.datastructures import immutabledict
from wolframclient.utils.tests import TestCase as BaseTestCase


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
        wxf = export(value, target_format="wxf")
        o = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(value, o)

    # Strings
    def testBasicString(self):
        value = "maître & élève"
        self.wxf_assert_roundtrip(value)

    def test_empty_string(self):
        value = ""
        self.wxf_assert_roundtrip(value)

    def test_binary_string(self):
        value = bytearray([0, 128, 255])
        self.wxf_assert_roundtrip(value)

    def test_empty_binary_string(self):
        value = bytearray([])
        self.wxf_assert_roundtrip(value)

    def test_max_3bytes_unicode(self):
        max = (1 << 16) - 1
        if six.PY2:
            value = unichr(max)
        else:
            value = chr(max)
        self.wxf_assert_roundtrip(value)

    def test_all_char(self):
        import itertools

        all_char = ""
        unicode_no_surrogate = itertools.chain(range(0xD800), range(1 + 0xDFFF, 1 << 16))
        count = 0
        for i in unicode_no_surrogate:
            count += 1
            if six.PY2:
                all_char += unichr(i)
            else:
                all_char += chr(i)
        path = current_file_dir = os.path.dirname(__file__)
        path = os.path.join(path, "..", "data", "allchars.wxf")

        with open(path, "rb") as r_file:
            res = binary_deserialize(r_file)
        self.assertEqual(res, all_char)

    ### INTEGER TESTS
    @skip_for_jython
    def test_integer8(self):
        value = (0, 1, 127, -1, -128)
        self.wxf_assert_roundtrip(value)

    @skip_for_jython
    def test_int16(self):
        value = (-(1 << 15), (1 << 15) - 1)
        self.wxf_assert_roundtrip(value)

    @skip_for_jython
    def test_int32(self):
        value = (-(1 << 31), (1 << 31) - 1)
        self.wxf_assert_roundtrip(value)

    @skip_for_jython
    def test_int64(self):
        value = (-(1 << 63), (1 << 63) - 1)
        self.wxf_assert_roundtrip(value)

    @skip_for_jython
    def test_bigint_as_int(self):
        value = 10 ** 20
        self.wxf_assert_roundtrip(value)

    def test_real(self):
        value = (1.2345, 0.0, 1.23456789e100)
        self.wxf_assert_roundtrip(value)

    def test_bigreal_precision(self):
        wxf = b"8:R\x0710.`10."
        res = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(res, decimal.Decimal(10))

    def test_bigreal_precision_exponent(self):
        wxf = b"8:R>9.999999999999996843873323328588479844`15.352529778863042*^999"
        res = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(res, decimal.Decimal("9.999999999999996843873323328588479844E+999"))

    def test_bigreal_precision_negexponent(self):
        wxf = b"8:RC4.590261537982443550699999999999999999999281`19.66183743091127*^-16"
        res = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(
            res, decimal.Decimal("4.590261537982443550699999999999999999999281E-16")
        )

    def test_empty_lists(self):
        value = ((), ((),), (1, ()), ())
        self.wxf_assert_roundtrip(value)

    def test_simple_dict(self):
        value = {1: 2}
        self.wxf_assert_roundtrip(value)

    def test_empty_dict(self):
        self.wxf_assert_roundtrip({})

    def test_rules(self):
        # BinarySerialize[<|"1" -> 1, "2" -> {0}, "3" -> <||>|>]
        wxf = b"8:A\x03-S\x011C\x01-S\x012f\x01s\x04ListC\x00-S\x013A\x00"
        res = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(res, {"1": 1, "2": (0,), "3": {}})

    # WXF format error
    def test_bad_header_version(self):
        wxf = b"1:"
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf, consumer=WXFConsumer())

    def test_bad_header_compress(self):
        wxf = b"8x:"
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf, consumer=WXFConsumer())

    def test_bad_header_separator(self):
        wxf = b"8C/"
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf, consumer=WXFConsumer())

    def test_bad_bignum(self):
        # replace last digit by 'A'
        wxf = b"8:I\x171234567890123456789012A"
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf, consumer=WXFConsumer())

    def test_compressed_input(self):
        expr = (1, 2, 3)
        wxf = export(expr, target_format="wxf", compress=True)
        res = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(expr, res)

    def test_nested_associations(self):
        expr = immutabledict((("a", 2), ("a", 3)))
        expr = immutabledict(((expr, expr), (2, 3)))
        wxf = export(expr, target_format="wxf", compress=True)
        res = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(expr, res)

    # Custom consumers
    class BadGreedyConsumer(WXFConsumer):
        def consume_function(self, current_token, tokens, **kwargs):
            # fetch too many elements
            while True:
                self.next_expression(tokens, **kwargs)

    def test_bad_greedy_consumer(self):
        with self.assertRaises(WolframParserException) as e:
            binary_deserialize(
                export([1, 2, 3], target_format="wxf"), consumer=self.BadGreedyConsumer()
            )
            self.assertEqual(
                e.msg,
                "Input data does not represent a valid expression in WXF format. Expecting more input data.",
            )

    class BadIncompleteConsumer(WXFConsumer):
        def consume_function(self, current_token, tokens, **kwargs):
            # fetch too few elements, head comes first and is not accounted for in length.
            args = []
            for i in range(current_token.length):
                self.next_expression(tokens, **kwargs)

    def test_bad_incomplete_consumer(self):
        with self.assertRaises(WolframParserException) as e:
            binary_deserialize(
                export([1, 2, 3], target_format="wxf"), consumer=self.BadIncompleteConsumer()
            )
            self.assertEqual(
                e.msg,
                "Input data does not represent a valid expression in WXF format. Some expressions are imcomplete.",
            )

    def test_bad_wxf_buffer(self):
        wxf = 1
        with self.assertRaises(TypeError):
            binary_deserialize(wxf, consumer=WXFConsumer())


@skip_for_jython
class TestCaseNumPyArray(BaseTestCase):
    def test_numpy_1d_array(self):
        arr = numpy.array([0, 1], "uint8")
        wxf = export(arr, target_format="wxf")
        res = binary_deserialize(wxf)
        numpy.assert_array_equal(res, arr)

    def test_numpy_2d_array(self):
        arr = numpy.array([[0, 1], [1, 1], [2, 1]], "uint8")
        wxf = export(arr, target_format="wxf")
        res = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(res.tolist(), arr.tolist())

    def test_numpy_packedarray(self):
        # Range[1]
        wxf = b"8:\xc1\x00\x01\x01\x01"
        res = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(res.tolist(), [1])

    def test_packedarray_ndim_int8(self):
        # ConstantArray[1, {2, 3, 1}]
        wxf = b"8:\xc1\x00\x03\x02\x03\x01\x01\x01\x01\x01\x01\x01"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 3, 1))
        self.assertEqual(a.dtype, "int8")
        self.assertEqual(a.tolist(), [[[1], [1], [1]], [[1], [1], [1]]])

    def test_packedarray_ndim_complex(self):
        # ConstantArray[I + 1., {2, 3, 1}]
        wxf = b"8:\xc14\x03\x02\x03\x01\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 3, 1))
        self.assertEqual(a.dtype, "complex128")

    def test_int8_array(self):
        # ConstantArray[1, {2, 2}]
        wxf = b"8:\xc1\x00\x02\x02\x02\x01\x01\x01\x01"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "int8")
        self.assertEqual(a.tolist(), [[1, 1], [1, 1]])

    def test_int16_array(self):
        # ConstantArray[2^15 - 1, {2, 2}]
        wxf = wxf = b"8:\xc2\x01\x02\x02\x02\xff\x7f\xff\x7f\xff\x7f\xff\x7f"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "int16")
        self.assertEqual(
            a.tolist(), [[-1 + 2 ** 15, -1 + 2 ** 15], [-1 + 2 ** 15, -1 + 2 ** 15]]
        )

    def test_int32_array(self):
        # ConstantArray[2^16, {2, 2}]
        wxf = b"8:\xc1\x02\x02\x02\x02\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "int32")
        self.assertEqual(a.tolist(), [[2 ** 16, 2 ** 16], [2 ** 16, 2 ** 16]])

    def test_int64_array(self):
        # ConstantArray[2^40, {2, 1}]
        wxf = b"8:\xc1\x03\x02\x02\x01\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "int64")
        self.assertEqual(a.tolist(), [[2 ** 40], [2 ** 40]])

    def test_double_array(self):
        # ConstantArray[1., {2, 1}]
        wxf = b"8:\xc1#\x02\x02\x01\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "float64")
        self.assertAlmostEqual(a.tolist(), [[1.0], [1.0]])

    def test_int8_numeric_array(self):
        # NumericArray from ConstantArray[1, {2, 2}]
        wxf = b"8:\xc2\x00\x02\x02\x02\x01\x01\x01\x01"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "int8")
        self.assertEqual(a.tolist(), [[1, 1], [1, 1]])

    def test_uint8_numeric_array(self):
        # NumericArray from ConstantArray[255, {2, 2}]
        wxf = b"8:\xc2\x10\x02\x02\x02\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "uint8")
        self.assertEqual(a.tolist(), [[255, 255], [255, 255]])

    def test_int16_numeric_array(self):
        # NumericArray from ConstantArray[2^15 - 1, {2, 2}]
        wxf = b"8:\xc2\x01\x02\x02\x02\xff\x7f\xff\x7f\xff\x7f\xff\x7f"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "int16")
        self.assertEqual(
            a.tolist(), [[-1 + 2 ** 15, -1 + 2 ** 15], [-1 + 2 ** 15, -1 + 2 ** 15]]
        )

    def test_uint16_numeric_array(self):
        # NumericArray from ConstantArray[2^16 - 1, {2, 2}]
        wxf = b"8:\xc2\x11\x02\x02\x02\xff\xff\xff\xff\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "uint16")
        self.assertEqual(
            a.tolist(), [[-1 + 2 ** 16, -1 + 2 ** 16], [-1 + 2 ** 16, -1 + 2 ** 16]]
        )

    def test_int32_numeric_array(self):
        # NumericArray from ConstantArray[2^16, {2, 2}]
        wxf = b"8:\xc2\x02\x02\x02\x02\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 2))
        self.assertEqual(a.dtype, "int32")
        self.assertEqual(a.tolist(), [[2 ** 16, 2 ** 16], [2 ** 16, 2 ** 16]])

    def test_uint32_numeric_array(self):
        # NumericArray from ConstantArray[2^32-1, {2, 1}]
        wxf = b"8:\xc2\x12\x02\x02\x01\xff\xff\xff\xff\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "uint32")
        self.assertEqual(a.tolist(), [[-1 + 2 ** 32], [-1 + 2 ** 32]])

    def test_int64_numeric_array(self):
        # NumericArray from ConstantArray[2^40, {2, 1}]
        wxf = b"8:\xc2\x03\x02\x02\x01\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "int64")
        self.assertEqual(a.tolist(), [[2 ** 40], [2 ** 40]])

    def test_uint64_numeric_array(self):
        # NumericArray from ConstantArray[2^64-1, {2, 1}]
        wxf = b"8:\xc2\x13\x02\x02\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "uint64")
        self.assertEqual(a.tolist(), [[-1 + 2 ** 64], [-1 + 2 ** 64]])

    def test_float_numeric_array(self):
        # NumericArray["Real32", ConstantArray[1., {2, 1}]]
        wxf = b'8:\xc2"\x02\x02\x01\x00\x00\x80?\x00\x00\x80?'
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "float32")
        self.assertAlmostEqual(a.tolist(), [[1.0], [1.0]])

    def test_double_numeric_array(self):
        # NumericArray["Real64", ConstantArray[1., {2, 1}]]
        wxf = b"8:\xc2#\x02\x02\x01\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?"
        a = binary_deserialize(wxf, consumer=WXFConsumerNumpy())
        self.assertEqual(a.shape, (2, 1))
        self.assertEqual(a.dtype, "float64")
        self.assertAlmostEqual(a.tolist(), [[1.0], [1.0]])


class TestCaseArrayAsList(BaseTestCase):
    def test_zero_array_rank(self):
        wxf_rank0 = b"8:\xc2\x00\x00\x01\x00"
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf_rank0)

    def test_zero_array_dimensions(self):
        wxf_dim0 = b"8:\xc2\x00\x01\x00\x00"
        with self.assertRaises(WolframParserException):
            binary_deserialize(wxf_dim0)

    def test_packedarray_ndim_int8(self):
        # ConstantArray[1, {2, 3, 1}]
        wxf = b"8:\xc1\x00\x03\x02\x03\x01\x01\x01\x01\x01\x01\x01"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(a, [[[1], [1], [1]], [[1], [1], [1]]])

    def test_packedarray_ndim_complex(self):
        # ConstantArray[I + 1., {2, 3, 1}]
        wxf = b"8:\xc14\x03\x02\x03\x01\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 3)
        self.assertEqual(len(a[0][0]), 1)
        for e1 in a:
            for e2 in e1:
                for e3 in e2:
                    self.assertAlmostEqual(e3, complex(1.0, 1.0))

    def test_int8_array(self):
        # ConstantArray[1, {2, 2}]
        wxf = b"8:\xc1\x00\x02\x02\x02\x01\x01\x01\x01"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[1, 1], [1, 1]])

    def test_int16_array(self):
        # ConstantArray[2^15 - 1, {2, 2}]
        wxf = wxf = b"8:\xc2\x01\x02\x02\x02\xff\x7f\xff\x7f\xff\x7f\xff\x7f"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[-1 + 2 ** 15, -1 + 2 ** 15], [-1 + 2 ** 15, -1 + 2 ** 15]])

    def test_int32_array(self):
        # ConstantArray[2^16, {2, 2}]
        wxf = b"8:\xc1\x02\x02\x02\x02\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[2 ** 16, 2 ** 16], [2 ** 16, 2 ** 16]])

    def test_int64_array(self):
        # ConstantArray[2^40, {2, 1}]
        wxf = b"8:\xc1\x03\x02\x02\x01\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[2 ** 40], [2 ** 40]])

    def test_double_array(self):
        # ConstantArray[1., {2, 1}]
        wxf = b"8:\xc1#\x02\x02\x01\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[1.0], [1.0]])

    def test_int8_numeric_array(self):
        # NumericArray from ConstantArray[1, {2, 2}]
        wxf = b"8:\xc2\x00\x02\x02\x02\x01\x01\x01\x01"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[1, 1], [1, 1]])

    def test_uint8_numeric_array(self):
        # NumericArray from ConstantArray[255, {2, 2}]
        wxf = b"8:\xc2\x10\x02\x02\x02\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[255, 255], [255, 255]])

    def test_int16_numeric_array(self):
        # NumericArray from ConstantArray[2^15 - 1, {2, 2}]
        wxf = b"8:\xc2\x01\x02\x02\x02\xff\x7f\xff\x7f\xff\x7f\xff\x7f"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[-1 + 2 ** 15, -1 + 2 ** 15], [-1 + 2 ** 15, -1 + 2 ** 15]])

    def test_uint16_numeric_array(self):
        # NumericArray from ConstantArray[2^16 - 1, {2, 2}]
        wxf = b"8:\xc2\x11\x02\x02\x02\xff\xff\xff\xff\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[-1 + 2 ** 16, -1 + 2 ** 16], [-1 + 2 ** 16, -1 + 2 ** 16]])

    def test_int32_numeric_array(self):
        # NumericArray from ConstantArray[2^16, {2, 2}]
        wxf = b"8:\xc2\x02\x02\x02\x02\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 2)
        self.assertEqual(a, [[2 ** 16, 2 ** 16], [2 ** 16, 2 ** 16]])

    def test_uint32_numeric_array(self):
        # NumericArray from ConstantArray[2^32-1, {2, 1}]
        wxf = b"8:\xc2\x12\x02\x02\x01\xff\xff\xff\xff\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[-1 + 2 ** 32], [-1 + 2 ** 32]])

    def test_int64_numeric_array(self):
        # NumericArray from ConstantArray[2^40, {2, 1}]
        wxf = b"8:\xc2\x03\x02\x02\x01\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[2 ** 40], [2 ** 40]])

    def test_uint64_numeric_array(self):
        # NumericArray from ConstantArray[2^64-1, {2, 1}]
        wxf = b"8:\xc2\x13\x02\x02\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[-1 + 2 ** 64], [-1 + 2 ** 64]])

    def test_float_numeric_array(self):
        # NumericArray["Real32", ConstantArray[1., {2, 1}]]
        wxf = b'8:\xc2"\x02\x02\x01\x00\x00\x80?\x00\x00\x80?'
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[1.0], [1.0]])

    def test_double_numeric_array(self):
        # NumericArray["Real64", ConstantArray[1., {2, 1}]]
        wxf = b"8:\xc2#\x02\x02\x01\x00\x00\x00\x00\x00\x00\xf0?\x00\x00\x00\x00\x00\x00\xf0?"
        a = binary_deserialize(wxf, consumer=WXFConsumer())
        self.assertEqual(len(a), 2)
        self.assertEqual(len(a[0]), 1)
        self.assertEqual(a, [[1.0], [1.0]])


class TestArrayRoundTrip(BaseTestCase):
    @staticmethod
    def ensure_roundtrip(pa):
        wxf = export(pa, target_format="wxf")
        res = binary_deserialize(wxf)
        numpy.assert_array_equal(res, pa)

    def test_int8_PA(self):
        pa = numpy.array([[-(1 << 7), -1], [1, (1 << 7) - 1]], numpy.int8).view(
            numpy.PackedArray
        )
        self.ensure_roundtrip(pa)

    def test_int16(self):
        pa = numpy.array([[-(1 << 15)], [(1 << 15) - 1]], numpy.int16).view(numpy.PackedArray)
        self.ensure_roundtrip(pa)

    def test_int32(self):
        pa = numpy.array([[-(1 << 31)], [(1 << 31) - 1]], numpy.int32).view(numpy.PackedArray)
        self.ensure_roundtrip(pa)

    def test_int64(self):
        pa = numpy.array([[-(1 << 62)], [(1 << 62)]], numpy.int64).view(numpy.PackedArray)
        self.ensure_roundtrip(pa)

    def test_uint8_RA(self):
        pa = numpy.array([0, (1 << 8) - 1], numpy.uint8).view(numpy.PackedArray)
        res = binary_deserialize(export(pa, target_format="wxf"))
        numpy.assert_array_equal(res, numpy.array([0, (1 << 8) - 1], numpy.int16))

    def test_uint16_RA(self):
        pa = numpy.array([0, (1 << 16) - 1], numpy.uint16).view(numpy.PackedArray)
        res = binary_deserialize(export(pa, target_format="wxf"))
        numpy.assert_array_equal(res, numpy.array([0, (1 << 16) - 1], numpy.int32))

    def test_uint32_RA(self):
        pa = numpy.array([0, (1 << 32) - 1], numpy.uint32).view(numpy.PackedArray)
        res = binary_deserialize(export(pa, target_format="wxf"))
        numpy.assert_array_equal(res, numpy.array([0, (1 << 32) - 1], numpy.int64))

    def test_uint64_RA(self):
        with self.assertRaises(NotImplementedError):
            pa = numpy.array([0, (1 << 64) - 1], numpy.uint64).view(numpy.PackedArray)
            export(pa, target_format="wxf")

    def test_float_PA(self):
        pa = numpy.array([[1.0, -2.0], [-3.0, 4.0]], dtype="float").view(numpy.PackedArray)
        self.ensure_roundtrip(pa)

    def test_double_PA(self):
        pa = numpy.array([[1.0, -2.0], [-3.0, 4.0]], dtype="double").view(numpy.PackedArray)
        self.ensure_roundtrip(pa)

    def test_complex64_PA(self):
        pa = numpy.array(
            [[complex(1, 0.1), complex(-2.0, 0)], [complex(-3.0, 1), complex(4.0, -1.0)]],
            dtype="complex64",
        ).view(numpy.PackedArray)
        self.ensure_roundtrip(pa)

    def test_complex128_PA(self):
        pa = numpy.array(
            [[complex(1, 0.1), complex(-2.0, 0)], [complex(-3.0, 1), complex(4.0, -1.0)]],
            dtype="complex128",
        ).view(numpy.PackedArray)
        self.ensure_roundtrip(pa)
