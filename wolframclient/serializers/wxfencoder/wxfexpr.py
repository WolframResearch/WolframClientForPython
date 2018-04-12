# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder import wxfutils
from wolframclient.serializers.wxfencoder.serializer import write_varint, WXFSerializerException
from wolframclient.utils import six
from wolframclient.utils.datastructures import Settings

import struct

if six.JYTHON:
    import jarray

__all__ = [
    'WXFExprFunction',
    'WXFExprInteger',
    'WXFExprString',
    'WXFExprSymbol',
    'WXFExprBinaryString',
    'WXF_CONSTANTS'
]

if six.PY3:
    def _bytes(value):
        return bytes((value,))
else:
    def _bytes(value):
        return chr(value)

#The list of all the WXF tokens.
WXF_CONSTANTS = Settings(
    Function     = b'f',
    Symbol       = b's',
    String       = b'S',
    BinaryString = b'B',
    Integer8     = b'C',
    Integer16    = b'j',
    Integer32    = b'i',
    Integer64    = b'L',
    Real64       = b'r',
    BigInteger   = b'I',
    BigReal      = b'R',
    PackedArray  = _bytes(0xC1),
    RawArray     = _bytes(0xC2),
    Association  = b'A',
    Rule         = b'-',
    RuleDelayed  = b':',
)

#The list of all array value type tokens.
ARRAY_TYPES = Settings(
    Integer8          = _bytes(0x00),
    Integer16         = _bytes(0x01),
    Integer32         = _bytes(0x02),
    Integer64         = _bytes(0x03),
    UnsignedInteger8  = _bytes(0x10),
    UnsignedInteger16 = _bytes(0x11),
    UnsignedInteger32 = _bytes(0x12),
    UnsignedInteger64 = _bytes(0x13),
    Real32            = _bytes(0x22),
    Real64            = _bytes(0x23),
    ComplexReal32     = _bytes(0x33),
    ComplexReal64     = _bytes(0x34),
)

''' A set of all valid value type tokens for PackedArray.
There is no restriction for RawArray value types. '''
VALID_PACKED_ARRAY_TYPES = frozenset((
    ARRAY_TYPES.Integer8,
    ARRAY_TYPES.Integer16,
    ARRAY_TYPES.Integer32,
    ARRAY_TYPES.Integer64,
    ARRAY_TYPES.Real32,
    ARRAY_TYPES.Real64,
    ARRAY_TYPES.ComplexReal32,
    ARRAY_TYPES.ComplexReal64,
))

class _WXFExpr(object):
    __slots__ = 'wxf_type'

    def __init__(self, wxf_type):
        self.wxf_type = wxf_type

    def _serialize_to_wxf(self, context, stream):
        ''' Write the serialized form of a given WXFExpr. '''
        raise NotImplementedError

class WXFExprFunction(_WXFExpr):
    ''' Functions have a length representing the number of parts (including zero).
    Each function has a head which is itself a expr, usually a `WXFExprSymbol` which
    is not accounted in the length.
    '''
    __slots__ = 'length'

    def __init__(self, length):
        super(WXFExprFunction, self).__init__(WXF_CONSTANTS.Function)
        self.length = length

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        # Function has a head which account for one part element contrary to association
        context.step_in_new_expr(self.length + 1)
        write_varint(self.length, stream)

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
            super(WXFExprInteger, self).__init__(WXF_CONSTANTS.Integer8)
            self.int_size = 1
        elif value < wxfutils.INT16_MAX and value >= wxfutils.INT16_MIN:
            super(WXFExprInteger, self).__init__(WXF_CONSTANTS.Integer16)
            self.int_size = 2
        elif value < wxfutils.INT32_MAX and value >= wxfutils.INT32_MIN:
            super(WXFExprInteger, self).__init__(WXF_CONSTANTS.Integer32)
            self.int_size = 4
        elif value < wxfutils.INT64_MAX and value >= wxfutils.INT64_MIN:
            super(WXFExprInteger, self).__init__(WXF_CONSTANTS.Integer64)
            self.int_size = 8
        else:
            raise ValueError('Value %i is not a machine-sized integer.' % value)

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

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        stream.write(self.to_bytes())

class WXFExprReal(_WXFExpr):
    ''' Represent a floating point value. Internally WXF represents the value with
    double float-point value in the IEEE 754 standard. '''
    __slots__ = 'value'

    def __init__(self, value):
        if not isinstance(value, float):
            raise TypeError('WXFExprReal must be initialized with a float.')
        super(WXFExprReal, self).__init__(WXF_CONSTANTS.Real64)
        self.value = value

    StructDouble = struct.Struct(b'<d')

    if six.JYTHON:
        def to_bytes(self):
            buffer = jarray.zeros(8, 'c')
            WXFExprReal.StructDouble.pack_into(buffer, 0, self.value)
            return buffer.tostring()
    else:
        def to_bytes(self):
            buffer = bytearray(8)
            WXFExprReal.StructDouble.pack_into(buffer, 0, self.value)
            return buffer

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        stream.write(self.to_bytes())

class _WXFExprStringLike(_WXFExpr):
    ''' Parent class of all string based expressions.

    Store a given string value as a utf-8 encoded binary string.
    '''
    __slots__ = 'length', 'value'

    def __init__(self, wxf_type, value, allow_binary = False):
        ''' Initialize using a given string `value`.

        If `allow_binary` is set to false, also accept binary types: `bytes` (py3) and `str` (py2)
        '''
        super(_WXFExprStringLike, self).__init__(wxf_type)
        if isinstance(value, six.string_types):
            self.value = value.encode('utf-8')
        elif allow_binary and isinstance(value, six.binary_type):
            self.value = value
        else:
            raise TypeError(self.__class__.__name__ +
                            " must be initialize with a string. Was '" + value.__class__.__name__ + "'.")

        self.length = len(self.value)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        write_varint(self.length, stream)
        stream.write(self.value)

class WXFExprSymbol(_WXFExprStringLike):
    ''' A symbol represented by a string name. The name is always utf8 encoded.'''
    def __init__(self, value):
        super(WXFExprSymbol, self).__init__(WXF_CONSTANTS.Symbol, value, allow_binary = True)

class WXFExprString(_WXFExprStringLike):
    ''' A string of unicode character. The string is always utf8 encoded.

    Notabene: Python 3 does not allow utf8 encoding of the surrogate range
    from `0xD800` to `0xDFFF`. Python 2 and the Wolfram Language on the other
    hand handle those characters as any other unicode code points.
    '''
    def __init__(self, value):
        super(WXFExprString, self).__init__(WXF_CONSTANTS.String, value)

class WXFExprBigInteger(_WXFExprStringLike):
    ''' A string of digits representing a big integer'''
    def __init__(self, value):
        super(WXFExprBigInteger, self).__init__(WXF_CONSTANTS.BigInteger, value)

class WXFExprBigReal(_WXFExprStringLike):
    ''' A string representation of a real value with arbitrary precision. The
    string format matches the one of the `InputForm` string representation of
    the real in the Wolfram Language.
    '''
    def __init__(self, value):
        super(WXFExprBigReal, self).__init__(WXF_CONSTANTS.BigReal, value)

class WXFExprBinaryString(_WXFExpr):
    '''A string of arbitrary bytes. Contrary to `WXFExprString` no encoding is
    required.'''
    __slots__ = 'data'
    def __init__(self, data):
        if isinstance(data, (six.binary_type, bytearray)):
            self.data = data
        else:
            raise TypeError(
                'WXFExprBinaryString must be initialized with binary data: bytes in Python 3, str in Python 2.7 or bytearray.')
        super(WXFExprBinaryString, self).__init__(WXF_CONSTANTS.BinaryString)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        write_varint(len(self.data), stream)
        stream.write(self.data)

class _WXFExprArray(_WXFExpr):
    '''Arrays are multidimensional tables of machine-precision numeric values.
    The `dimensions` is a list of strictly positive integers representing the
    array shape. The data contains the flatten binary representation of the
    values.'''
    __slots__ = 'dimensions', 'value_type', 'data'

    def __init__(self, wxf_type, dimensions, value_type, data = None):
        super(_WXFExprArray, self).__init__(wxf_type)
        if not isinstance(dimensions, (list, tuple)) or len(dimensions) == 0:
            raise TypeError('Dimensions must be a non-empty list.')

        if not all(not isinstance(dim, six.integer_types) or dim <= 0 for dim in dimensions):
            raise ValueError('Dimensions must be positive integers.')

        self.dimensions = dimensions
        self.value_type = value_type
        self.data = data

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        stream.write(self.value_type)
        write_varint(len(self.dimensions), stream)
        for dim in self.dimensions:
            write_varint(dim, stream)
        if self.data is not None:
            stream.write(self.data)
        else:
            raise WXFSerializerException("Missing array data.")

class WXFExprPackedArray(_WXFExprArray):
    ''' Packed array is a type of array that only supports a subset of all the
    possible type of values: signed integers, reals, complexes. See `VALID_PACKED_ARRAY_TYPES`.'''
    def __init__(self, dimensions, value_type, data=None):
        if value_type not in VALID_PACKED_ARRAY_TYPES:
            raise Exception('Invalid packed array value type ({}).', value_type)
        super(WXFExprPackedArray, self).__init__(WXF_CONSTANTS.PackedArray, dimensions, value_type, data)

class WXFExprRawArray(_WXFExprArray):
    ''' Raw array is an array that supports many type of values:
    signed and unsigned integers, reals, complexes. See `ARRAY_TYPES`.'''
    def __init__(self, dimensions, value_type, data=None):
        super(WXFExprRawArray, self).__init__(WXF_CONSTANTS.RawArray, dimensions, value_type, data)

class WXFExprAssociation(_WXFExpr):
    ''' Association is a key value store similar to `dict`. `WXFExprAssociation`
    requires a length, the number of entries. Only `WXFExprRule` and `WXFExprRuleDelayed`
    are valid entry types in an association.
    '''
    __slot__ = 'length'
    def __init__(self, length):
        if not isinstance(length, six.integer_types) or length < 0:
            raise TypeError('WXFExprAssociation must be instanciated with a length.')
        super(WXFExprAssociation, self).__init__(WXF_CONSTANTS.Association)
        self.length = length

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.step_in_new_expr(self.length, is_assoc=True)
        write_varint(self.length, stream)

class _WXFExprRule(_WXFExpr):
    def __init__(self, wxf_type):
        super(_WXFExprRule, self).__init__(wxf_type)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        # make sure those special tokens are correctly used inside an association.
        if not context.is_rule_valid():
            raise WXFSerializerException('WXF Rule and RuleDelayed must be part of an Association. Use a Function with head Symbol "Rule(Delayed)" outside associations.')
        # rule always has two parts.
        context.step_in_new_expr(2)

class WXFExprRule(_WXFExprRule):
    ''' Represent a rule in an association. Rule have two parts but no head.
    Rules that are not part of an association (e.g list of rules) must be encoded
    as a function with head `'Rule'`.
    '''
    def __init__(self):
        super(WXFExprRule, self).__init__(WXF_CONSTANTS.Rule)

class WXFExprRuleDelayed(_WXFExprRule):
    ''' Represent a rule delayed in an association. See `WXFExprRule`'''
    def __init__(self):
        super(WXFExprRuleDelayed,
                self).__init__(WXF_CONSTANTS.RuleDelayed)