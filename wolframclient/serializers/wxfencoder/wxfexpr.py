# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder.constants import (
    VALID_PACKED_ARRAY_TYPES, WXF_CONSTANTS, StructDouble, StructInt8LE,
    StructInt16LE, StructInt32LE, StructInt64LE)
from wolframclient.serializers.wxfencoder.serializer import (
    WXFSerializerException)
from wolframclient.serializers.wxfencoder.utils import (
    float_to_bytes, integer_size, integer_to_bytes, write_varint)
from wolframclient.utils import six

__all__ = [
    'WXFExprFunction', 'WXFExprInteger', 'WXFExprString', 'WXFExprSymbol',
    'WXFExprBinaryString', 'WXF_CONSTANTS'
]


class WXFExpr(object):
    """Represent a WXF expression.

    This is an abstract class. Only its children must be used.
    """

    __slots__ = 'wxf_type'

    def __init__(self, wxf_type):
        self.wxf_type = wxf_type

    def _serialize_to_wxf(self, context, stream):
        ''' Write the serialized form of a given WXFExpr. '''
        raise NotImplementedError


class WXFExprFunction(WXFExpr):
    """Functions have a length representing the number of parts (including zero).
    Each function has a head which is itself a expr, usually a `WXFExprSymbol` which
    is not accounted in the length.
    """
    __slots__ = 'length'

    def __init__(self, length):
        super(WXFExprFunction, self).__init__(WXF_CONSTANTS.Function)
        self.length = length

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        # Function has a head which account for one part element contrary to association
        context.step_into_new_function(self.length)
        write_varint(self.length, stream)


class WXFExprInteger(WXFExpr):
    ''' Integers have various length, from one byte up to eight and are signed
    values. Values above 2^63-1 are represented with `WXFExprBigInteger`.
    Internally WXF uses the two's complement representation of integer values.
    The endianness is system independent and is always little-endian.
    '''
    __slots__ = 'value', 'int_size'

    def __init__(self, value):
        if not isinstance(value, six.integer_types):
            raise TypeError(
                'WXFExprInteger must be initialize with an integer value.')
        wxf_type, self.int_size = integer_size(value)
        super(WXFExprInteger, self).__init__(wxf_type)
        self.value = value

    ''' Encode the integer into bytes and return them in a `buffer`.

    Note that the buffer is an bytearray in python 2.7 and an array in 3.x.
    This method is only useful to hide the Python 2.7 implementation.
    It is proxying int.to_bytes for version 3.4 and above.
    '''

    def to_bytes(self):
        return integer_to_bytes(self.value, self.int_size)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        stream.write(self.to_bytes())


class WXFExprReal(WXFExpr):
    ''' Represent a floating point value. Internally WXF represents the value with
    double float-point value in the IEEE 754 standard. '''
    __slots__ = 'value'

    def __init__(self, value):
        if not isinstance(value, float):
            raise TypeError('WXFExprReal must be initialized with a float.')
        super(WXFExprReal, self).__init__(WXF_CONSTANTS.Real64)
        self.value = value

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        stream.write(float_to_bytes(self.value))


class _WXFExprStringLike(WXFExpr):
    ''' Parent class of all string based expressions.

    Store a given string value as a utf-8 encoded binary string.
    '''
    __slots__ = 'length', 'value'

    def __init__(self, wxf_type, value, allow_binary=False):
        ''' Initialize using a given string `value`.

        If `allow_binary` is set to false, also accept binary types: `bytes` (py3) and `str` (py2)
        '''
        super(_WXFExprStringLike, self).__init__(wxf_type)
        if allow_binary and isinstance(value, six.binary_type):
            self.value = value
        elif isinstance(value, six.text_type):
            self.value = value.encode('utf-8')
        else:
            raise TypeError(self.__class__.__name__ +
                            " must be initialize with a string. Was '" +
                            value.__class__.__name__ + "'.")

        self.length = len(self.value)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        write_varint(self.length, stream)
        stream.write(self.value)


class WXFExprSymbol(_WXFExprStringLike):
    ''' A symbol represented by a string name. The name is always utf8 encoded.'''

    def __init__(self, value):
        super(WXFExprSymbol, self).__init__(
            WXF_CONSTANTS.Symbol, value, allow_binary=True)


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
        super(WXFExprBigInteger, self).__init__(
            WXF_CONSTANTS.BigInteger, value, allow_binary=True)


class WXFExprBigReal(_WXFExprStringLike):
    ''' A string representation of a real value with arbitrary precision. The string format matches the one of the
    :wl:`InputForm` string representation of the real in the Wolfram Language.
    '''

    def __init__(self, value):
        super(WXFExprBigReal, self).__init__(
            WXF_CONSTANTS.BigReal, value, allow_binary=True)


class WXFExprBinaryString(WXFExpr):
    '''A string of arbitrary bytes. Contrary to `WXFExprString` no encoding is
    required.'''
    __slots__ = 'data'

    def __init__(self, data):
        if isinstance(data, (six.binary_type, bytearray, six.buffer_types)):
            self.data = data
        else:
            raise TypeError(
                'WXFExprBinaryString must be initialized with binary data: bytes in Python 3, str in Python 2.7 or bytearray.'
            )
        super(WXFExprBinaryString, self).__init__(WXF_CONSTANTS.BinaryString)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.add_part()
        write_varint(len(self.data), stream)
        stream.write(self.data)


class _WXFExprArray(WXFExpr):
    '''Arrays are multidimensional tables of machine-precision numeric values.
    The `dimensions` is a list of strictly positive integers representing the
    array shape. The data contains the flatten binary representation of the
    values.'''
    __slots__ = 'dimensions', 'value_type', 'data'

    def __init__(self, wxf_type, dimensions, value_type, data=None):
        super(_WXFExprArray, self).__init__(wxf_type)
        if not isinstance(dimensions, (list, tuple)) or len(dimensions) == 0:
            raise TypeError('Dimensions must be a non-empty list.')

        if not all(
                isinstance(dim, six.integer_types) and dim > 0
                for dim in dimensions):
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
            raise Exception('Invalid packed array value type ({}).',
                            value_type)
        super(WXFExprPackedArray, self).__init__(WXF_CONSTANTS.PackedArray,
                                                 dimensions, value_type, data)


class WXFExprNumericArray(_WXFExprArray):
    ''' Raw array is an array that supports many type of values:
    signed and unsigned integers, reals, complexes. See `ARRAY_TYPES`.'''

    def __init__(self, dimensions, value_type, data=None):
        super(WXFExprNumericArray, self).__init__(WXF_CONSTANTS.NumericArray,
                                                  dimensions, value_type, data)


class WXFExprAssociation(WXFExpr):
    ''' Association is a key value store similar to `dict`. `WXFExprAssociation`
    requires a length, the number of entries. Only `WXFExprRule` and `WXFExprRuleDelayed`
    are valid entry types in an association.
    '''
    __slot__ = 'length'

    def __init__(self, length):
        if not isinstance(length, six.integer_types) or length < 0:
            raise TypeError(
                'WXFExprAssociation must be instantiated with a length.')
        super(WXFExprAssociation, self).__init__(WXF_CONSTANTS.Association)
        self.length = length

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        context.step_into_new_assoc(self.length)
        write_varint(self.length, stream)


class _WXFExprRule(WXFExpr):
    def __init__(self, wxf_type):
        super(_WXFExprRule, self).__init__(wxf_type)

    def _serialize_to_wxf(self, stream, context):
        stream.write(self.wxf_type)
        # make sure those special tokens are correctly used inside an association.
        if not context.is_rule_valid():
            raise WXFSerializerException(
                'WXF Rule and RuleDelayed must be part of an Association. Use a Function with head Symbol "Rule(Delayed)" outside associations.'
            )
        # rule always has two parts.
        context.step_into_new_rule()


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
        super(WXFExprRuleDelayed, self).__init__(WXF_CONSTANTS.RuleDelayed)
