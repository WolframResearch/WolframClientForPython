# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wxfserializer.serializer import write_varint, WXFSerializerException
from wxfserializer.utils import six, wxfutils

import struct

if six.JYTHON:
    import jarray

__all__ = [
    'WXFExprFunction',
    'WXFExprInteger',
    'WXFExprString',
    'WXFExprSymbol',
    'WXFExprBinaryString',
    'WXFConstants'
]

class WXFConstants:
    """ The list of all the WXF tokens. """
    Function = ord('f')
    Symbol = ord('s')
    String = ord('S')
    BinaryString = ord('b')
    Integer8 = ord('C')
    Integer16 = ord('j')
    Integer32 = ord('i')
    Integer64 = ord('L')
    Real64 = ord('r')
    BigInteger = ord('I')
    BigReal = ord('R')
    PackedArray = 0xC1
    RawArray = 0xC2
    Association = ord('A')
    Rule = ord('-')
    RuleDelayed = ord(':')

class ArrayTypes:
    ''' The list of all array value type tokens. '''
    Integer8 = 0x00
    Integer16 = 0x01
    Integer32 = 0x02
    Integer64 = 0x03
    UnsignedInteger8 = 0x10
    UnsignedInteger16 = 0x11
    UnsignedInteger32 = 0x12
    UnsignedInteger64 = 0x13
    Real32 = 0x22
    Real64 = 0x23
    ComplexReal32 = 0x33
    ComplexReal64 = 0x34

''' A set of all valid value type tokens for PackedArray.
There is no restriction for RawArray value types. '''
VALID_PACKED_ARRAY_TYPES = set([
    ArrayTypes.Integer8,
    ArrayTypes.Integer16,
    ArrayTypes.Integer32,
    ArrayTypes.Integer64,
    ArrayTypes.Real32,
    ArrayTypes.Real64,
    ArrayTypes.ComplexReal32,
    ArrayTypes.ComplexReal64]
)

class _WXFExpr(object):
    __slots__ = 'wxfType'

    def __init__(self, wxfType):
        self.wxfType = wxfType

    def _serialize_to_wxf(self, context, data_consumer):
        ''' Write the serialized form of a given WXFExpr. '''
        raise NotImplementedError

class WXFExprFunction(_WXFExpr):
    ''' Functions have a length representing the number of parts (including zero).
    Each function has a head which is itself a expr, usually a `WXFExprSymbol` which
    is not accounted in the length.
    '''
    __slots__ = 'length'

    def __init__(self, length):
        super(WXFExprFunction, self).__init__(WXFConstants.Function)
        self.length = length

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        # Function has a head which account for one part element contrary to association
        context.step_in_new_expr(self.length + 1)
        write_varint(self.length, data_consumer)

class WXFExprInteger(_WXFExpr):
    ''' Integers have various length, from one byte up to eigth and are signed
    values. Values above 2^63-1 are represented with `WXFExprBigInteger`.
    Internally WXF uses the two's complement representation of integer values.
    The endianness is system independant and is always little-endian.
    '''
    __slots__ = 'value', 'int_size'

    def __init__(self, value):
        if not isinstance(value, six.integer_types):
            raise TypeError('WXFExprInteger must be initialize with an integer value.')
        if value < wxfutils.INT8_MAX and value >= wxfutils.INT8_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer8)
            self.int_size = 1
        elif value < wxfutils.INT16_MAX and value >= wxfutils.INT16_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer16)
            self.int_size = 2
        elif value < wxfutils.INT32_MAX and value >= wxfutils.INT32_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer32)
            self.int_size = 4
        elif value < wxfutils.INT64_MAX and value >= wxfutils.INT64_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer64)
            self.int_size = 8

        self.value = value

    StructInt8LE  = struct.Struct(b'<b')
    StructInt16LE = struct.Struct(b'<h')
    StructInt32LE = struct.Struct(b'<i')
    StructInt64LE = struct.Struct(b'<q')

    def _pack(self, buffer):
        if self.int_size == 1:
            WXFExprInteger.StructInt8LE.pack_into(buffer, 0, self.value)
        elif self.int_size == 2:
            WXFExprInteger.StructInt16LE.pack_into(buffer, 0, self.value)
        elif self.int_size == 4:
            WXFExprInteger.StructInt32LE.pack_into(buffer, 0, self.value)
        else:
            WXFExprInteger.StructInt64LE.pack_into(buffer, 0, self.value)


    ''' Encode the integer into bytes and return them in a `buffer`.

    Note that the buffer is an bytearray in python 2.7 and an array in 3.x.
    This method is only useful to hide the Python 2.7 implementation.
    It is proxying int.to_bytes for version 3.4 and above.
    '''

    if six.JYTHON:
        def to_bytes(self):
            buffer = jarray.zeros(8, 'c')
            self._pack(buffer)
            return buffer[:self.int_size].tostring() 
    elif six.PY2:
        def to_bytes(self):
            buffer = bytearray(8)
            self._pack(buffer)
            return buffer[:self.int_size]
    else:
        def to_bytes(self):
            return self.value.to_bytes(self.int_size, byteorder='little', signed=True)

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        context.add_part()
        data_consumer.extend(self.to_bytes())

class WXFExprReal(_WXFExpr):
    ''' Represent a floating point value. Internally WXF represents the value with
    double float-point value in the IEEE 754 standard. '''
    __slots__ = 'value'

    def __init__(self, value):
        if not isinstance(value, float):
            raise TypeError('WXFExprReal must be initialized with a float.')
        super(WXFExprReal, self).__init__(WXFConstants.Real64)
        self.value = value

    StructDouble = struct.Struct(b'<d')

    if six.JYTHON:
        def to_bytes(self):
            buffer = jarray.zeros(8, 'c')
            WXFExprReal.StructDouble.pack_into(buffer, 0, self.value)
            buffer.tostring()
            return buffer
    else:
        def to_bytes(self):
            buffer = bytearray(8)
            WXFExprReal.StructDouble.pack_into(buffer, 0, self.value)
            return buffer

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        context.add_part()
        data_consumer.extend(self.to_bytes())

class _WXFExprStringLike(_WXFExpr):
    ''' Parent class of all string based expressions.

    Store a given string value as a utf-8 encoded binary string.
    '''
    __slots__ = 'length', 'value'

    def __init__(self, wxfType, value, allow_binary = False):
        ''' Initialize using a given string `value`.

        If `allow_binary` is set to false, also accept binary types: `bytes` (py3) and `str` (py2)
        '''
        if isinstance(value, six.string_types) or allow_binary and isinstance(value, six.binary_type):
            super(_WXFExprStringLike, self).__init__(wxfType)
            self.value = value.encode('utf-8')
            self.length = len(self.value)
        else:
            raise TypeError(self.__class__.__name__ +
                            " must be initialize with a string. Was '" + value.__class__.__name__ + "'.")

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        context.add_part()
        write_varint(self.length, data_consumer)
        data_consumer.extend(self.value)

class WXFExprSymbol(_WXFExprStringLike):
    ''' A symbol represented by a string name. The name is always utf8 encoded.'''
    def __init__(self, value):
        super(WXFExprSymbol, self).__init__(WXFConstants.Symbol, value)

class WXFExprString(_WXFExprStringLike):
    ''' A string of unicode character. The string is always utf8 encoded.

    Notabene: Python 3 does not allow utf8 encoding of the surrogate range
    from `0xD800` to `0xDFFF`. Python 2 and the Wolfram Language on the other
    hand handle those characters as any other unicode code points.
    '''
    def __init__(self, value):
        super(WXFExprString, self).__init__(WXFConstants.String, value)

class WXFExprBigInteger(_WXFExprStringLike):
    ''' A string of digits representing a big integer'''
    def __init__(self, value):
        super(WXFExprBigInteger, self).__init__(WXFConstants.BigInteger, value)

class WXFExprBigReal(_WXFExprStringLike):
    ''' A string representation of a real value with arbitrary precision. The
    string format matches the one of the `InputForm` string representation of
    the real in the Wolfram Language.
    '''
    def __init__(self, value):
        super(WXFExprBigReal, self).__init__(WXFConstants.BigReal, value)

class WXFExprBinaryString(_WXFExpr):
    '''A string of arbitrary bytes. Contrary to `WXFExprString` no encoding is
    required.'''
    __slots__ = 'data'
    def __init__(self, data):
        if isinstance(data, six.binary_type) or isinstance(data, bytearray):
            self.data = data
        else:
            raise TypeError(
                'WXFExprBinaryString must be initialized with binary data: bytes in Python 3, str in Python 2.7 or bytearray.')
        super(WXFExprBinaryString, self).__init__(WXFConstants.BinaryString)

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        context.add_part()
        write_varint(len(self.data), data_consumer)
        data_consumer.extend(self.data)

class _WXFExprArray(_WXFExpr):
    '''Arrays are multidimensional tables of machine-precision numeric values.
    The `dimensions` is a list of strictly positive integers representing the
    array shape. The data contains the flatten binary representation of the
    values.'''
    __slots__ = 'dimensions', 'value_type', 'data'

    def __init__(self, wxfType, dimensions, value_type, data = None):
        super(_WXFExprArray, self).__init__(wxfType)
        if not (isinstance(dimensions, tuple) or isinstance(dimensions, list)) or len(dimensions) == 0:
            raise TypeError('Dimensions must be a non-empty list.')
        for dim in dimensions:
            if dim <= 0 or not isinstance(dim, six.integer_types):
                raise Exception('Dimensions must be positive integers.')
        self.dimensions = dimensions
        self.value_type = value_type
        self.data = data

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        context.add_part()
        data_consumer.append(self.value_type)
        write_varint(len(self.dimensions), data_consumer)
        for dim in self.dimensions:
            write_varint(dim, data_consumer)
        if self.data is not None:
            data_consumer.extend(self.data)
        else:
            raise WXFSerializerException("Missing array data.")

class WXFExprPackedArray(_WXFExprArray):
    ''' Packed array is a type of array that only supports a subset of all the
    possible type of values: signed integers, reals, complexes. See `VALID_PACKED_ARRAY_TYPES`.'''
    def __init__(self, dimensions, value_type, data=None):
        if value_type not in VALID_PACKED_ARRAY_TYPES:
            raise Exception('Invalid packed array value type ({}).', value_type)
        super(WXFExprPackedArray, self).__init__(WXFConstants.PackedArray, dimensions, value_type, data)

class WXFExprRawArray(_WXFExprArray):
    ''' Raw array is an array that supports many type of values:
    signed and unsigned integers, reals, complexes. See `ArrayTypes`.'''
    def __init__(self, dimensions, value_type, data=None):
        super(WXFExprRawArray, self).__init__(WXFConstants.RawArray, dimensions, value_type, data)

class WXFExprAssociation(_WXFExpr):
    ''' Association is a key value store similar to `dict`. `WXFExprAssociation`
    requires a length, the number of entries. Only `WXFExprRule` and `WXFExprRuleDelayed`
    are valid entry types in an association.
    '''
    __slot__ = 'length'
    def __init__(self, length):
        if not isinstance(length, six.integer_types) or length < 0:
            raise TypeError('WXFExprAssociation must be instanciated with a length.')
        super(WXFExprAssociation, self).__init__(WXFConstants.Association)
        self.length = length

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        context.step_in_new_expr(self.length, is_assoc=True)
        write_varint(self.length, data_consumer)

class _WXFExprRule(_WXFExpr):
    def __init__(self, wxf_type):
        super(_WXFExprRule, self).__init__(wxf_type)

    def _serialize_to_wxf(self, data_consumer, context):
        data_consumer.append(self.wxfType)
        # make sure those special tokens are correctly used inside an association.
        if not context.is_in_assoc():
            raise WXFSerializerException('WXF Rule and RuleDelayed must be part of an Association. Use a Function with head Symbol "Rule(Delayed)" outside associations.')
        # rule always has two parts.
        context.step_in_new_expr(2)

class WXFExprRule(_WXFExprRule):
    ''' Represent a rule in an association. Rule have two parts but no head.
    Rules that are not part of an association (e.g list of rules) must be encoded
    as a function with head `'Rule'`.
    '''
    def __init__(self):
        super(WXFExprRule, self).__init__(WXFConstants.Rule)

class WXFExprRuleDelayed(_WXFExprRule):
    ''' Represent a rule delayed in an association. See `WXFExprRule`'''
    def __init__(self):
        super(WXFExprRuleDelayed,
                self).__init__(WXFConstants.RuleDelayed)
