from wxfserializer.wxfencoder import WXFEncoder
import wxfserializer.wxfexpr as wxfexpr
from wxfserializer.wxfexpr import ArrayTypes

import numpy

class NumPyWXFEncoder(WXFEncoder):
    '''
    NumPy array encoder. Encode numpy array as instances of packed array and / or raw array.
    By default only packed arrays are generated. Unsigned integer data are cast to a type that
    can fit the maximum value.

    It's possible to add support for raw arrays only for unsigned data, in which case both 
    `packed_array_support` and `rawarray_support` must be true.

    >>> NumPyWXFEncoder(packed_array_support=True, rawarray_support=True)

    Finally it's possible to only output raw arrays with:

    >>> NumPyWXFEncoder(packed_array_support=False, rawarray_support=True)
    
    '''

    __slots__ = 'packed_array_support', 'rawarray_support'

    def __init__(self, packed_array_support=True, rawarray_support=False):
        if not packed_array_support and not rawarray_support:
            raise Exception(
                'At least one of the two parameters packed_array_support or rawarray_support must be True.')
        self.packed_array_support = packed_array_support
        self.rawarray_support = rawarray_support

    def encode(self, pythonExpr):
        if isinstance(pythonExpr, numpy.ndarray):
            if self.packed_array_support:
                array_class = wxfexpr.WXFExprPackedArray
            else:
                array_class = wxfexpr.WXFExprRawArray

            if pythonExpr.dtype == numpy.int8:
                value_type = ArrayTypes.Integer8
                data = pythonExpr.astype('<i1')
            elif pythonExpr.dtype == numpy.int16:
                data = pythonExpr.astype('<i2')
                value_type = ArrayTypes.Integer16
            elif pythonExpr.dtype == numpy.int32:
                data = pythonExpr.astype('<i4')
                value_type = ArrayTypes.Integer32
            elif pythonExpr.dtype == numpy.int64:
                data = pythonExpr.astype('<i8')
                value_type = ArrayTypes.Integer64
            elif pythonExpr.dtype == numpy.uint8:
                if self.rawarray_support:
                    value_type = ArrayTypes.UnsignedInteger8
                    data = pythonExpr.astype('<u1')
                    array_class = wxfexpr.WXFExprRawArray
                else :
                    value_type = ArrayTypes.Integer16
                    data = pythonExpr.astype('<i2')
            elif pythonExpr.dtype == numpy.uint16:
                if self.rawarray_support:
                    value_type = ArrayTypes.UnsignedInteger16
                    data = pythonExpr.astype('<u2')
                    array_class = wxfexpr.WXFExprRawArray
                else:
                    value_type = ArrayTypes.Integer32
                    data = pythonExpr.astype('<i4')
            elif pythonExpr.dtype == numpy.uint32:
                if self.rawarray_support:
                    value_type = ArrayTypes.UnsignedInteger32
                    data = pythonExpr.astype('<u4')
                    array_class = wxfexpr.WXFExprRawArray
                else:
                    value_type = ArrayTypes.Integer64
                    data = pythonExpr.astype('<i8')
            # no one to one mapping to signed values, even if most of the time 
            # the values would fit
            elif pythonExpr.dtype == numpy.uint64:
                if self.rawarray_support:
                    value_type = ArrayTypes.UnsignedInteger64
                    data = pythonExpr.astype('<u8')
                    array_class = wxfexpr.WXFExprRawArray
                else:
                    TypeError('Cannot represent data of type uint64 as signed int64')
            elif pythonExpr.dtype == numpy.float32:
                value_type = ArrayTypes.Real32
                data = pythonExpr
            elif pythonExpr.dtype == numpy.float64:
                value_type = ArrayTypes.Real64
                data = pythonExpr
            elif pythonExpr.dtype == numpy.complex64:
                value_type = ArrayTypes.ComplexReal32
                data = pythonExpr
            elif pythonExpr.dtype == numpy.complex128:
                value_type = ArrayTypes.ComplexReal64
                data = pythonExpr
            else:
                raise Exception(
                    'NumPy serialization not implemented for ', repr(pythonExpr.dtype))

            yield array_class(pythonExpr.shape, value_type, data.tobytes())
