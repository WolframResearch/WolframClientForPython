import wxfserializer.wxfutils as wxfutils
from wxfserializer.utils import six

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
    Read64 = ord('r')
    BigInteger = ord('I')
    BigReal = ord('R')
    PackedArray = 0xC1
    RawArray = 0xC2
    Association = ord('A')
    Rule = ord('-')
    RuleDelayed = ord(':')


class ArrayTypes:
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

ValidPackedArrayTypes = set([
    ArrayTypes.Integer8,
    ArrayTypes.Integer16,
    ArrayTypes.Integer32,
    ArrayTypes.Integer64,
    ArrayTypes.Real32,
    ArrayTypes.Real64,
    ArrayTypes.ComplexReal32,
    ArrayTypes.ComplexReal64]
)


class _WXFExpr():

    __slots__ = 'wxfType'

    def __init__(self, wxfType):
        self.wxfType = wxfType


class WXFExprFunction(_WXFExpr):
    __slots__ = 'length'

    def __init__(self, length):
        super(WXFExprFunction, self).__init__(WXFConstants.Function)
        self.length = length

class WXFExprInteger(_WXFExpr):

    __slots__ = 'value', 'intSize'

    def __init__(self, value):
        if not isinstance(value, six.integer_types):
            raise TypeError('WXFExprInteger must be initialize with an integer value.')
        if value < wxfutils.INT8_MAX and value >= wxfutils.INT8_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer8)
            self.intSize = 1
        elif value < wxfutils.INT16_MAX and value >= wxfutils.INT16_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer16)
            self.intSize = 2
        elif value < wxfutils.INT32_MAX and value >= wxfutils.INT32_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer32)
            self.intSize = 4
        elif value < wxfutils.INT64_MAX and value >= wxfutils.INT64_MIN:
            super(WXFExprInteger, self).__init__(WXFConstants.Integer64)
            self.intSize = 8

        self.value = value

class WXFExprReal(_WXFExpr):
    __slots__ = 'value'

    def __init__(self, value):
        if not isinstance(value, float):
            raise TypeError('WXFExprReal must be initialized with a float.')
        super(WXFExprReal, self).__init__(WXFConstants.Read64)
        self.value = value

class _WXFExprStringLike(_WXFExpr):

    __slots__ = 'length', 'value'

    def __init__(self, wxfType, value):
        if not isinstance(value, six.text_type):
            raise TypeError(
                self.__class__.__name__, 'must be initialize with a string.')
        super(_WXFExprStringLike, self).__init__(wxfType)
        self.value = value.encode('utf-8')
        self.length = len(self.value)


class WXFExprSymbol(_WXFExprStringLike):
    def __init__(self, value):
        super(WXFExprSymbol, self).__init__(WXFConstants.Symbol, value)


class WXFExprString(_WXFExprStringLike):
    def __init__(self, value):
        super(WXFExprString, self).__init__(WXFConstants.String, value)


class WXFExprBigInteger(_WXFExprStringLike):
    def __init__(self, value):
        super(WXFExprBigInteger, self).__init__(WXFConstants.BigInteger, value)

class WXFExprBigReal(_WXFExprStringLike):
    def __init__(self, value):
        super(WXFExprBigReal, self).__init__(WXFConstants.BigReal, value)

class WXFExprBinaryString(_WXFExpr):
    __slots__ = 'data'
    def __init__(self, data):
        if isinstance(data, six.binary_type):
            self.data = data
        else:
            raise TypeError(
                'WXFExprBinaryString must be initialized with a bytearray.')
        super(WXFExprBinaryString, self).__init__(WXFConstants.BinaryString)
        

class _WXFExprArray(_WXFExpr):
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
    
class WXFExprPackedArray(_WXFExprArray):
    def __init__(self, dimensions, value_type, data=None):
        if value_type not in ValidPackedArrayTypes:
            raise Exception('Invalid packed array value type ({}).', value_type)
        super(WXFExprPackedArray, self).__init__(WXFConstants.PackedArray, dimensions, value_type, data)


class WXFExprRawArray(_WXFExprArray):
    def __init__(self, dimensions, value_type, data=None):
        super(WXFExprRawArray, self).__init__(WXFConstants.RawArray, dimensions, value_type, data)

class WXFExprAssociation(_WXFExpr):
    __slot__ = 'length'
    def __init__(self, length):
        if not isinstance(length, six.integer_types) or length < 0:
            raise TypeError('WXFExprAssociation must be instanciated with a length.')
        super(WXFExprAssociation, self).__init__(WXFConstants.Association)
        self.length = length
    
class WXFExprRule(_WXFExpr):
    def __init__(self):
        super(WXFExprRule, self).__init__(WXFConstants.Rule)

class WXFExprRuleDelayed(_WXFExpr):
    def __init__(self):
        super(WXFExprRuleDelayed,
                self).__init__(WXFConstants.RuleDelayed)
