# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder.wxfencoder import WXFEncoder
from wolframclient.serializers.wxfencoder.wxfexpr import ARRAY_TYPES, WXFExprPackedArray, WXFExprRawArray

import numpy

__all__ = [
    'NumPyWXFEncoder',
]

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

    def encode(self, python_expr):
        if isinstance(python_expr, numpy.ndarray):
            if self.packed_array_support:
                array_class = WXFExprPackedArray
            else:
                array_class = WXFExprRawArray

            if python_expr.dtype == numpy.int8:
                value_type = ARRAY_TYPES.Integer8
                data = python_expr.astype('<i1')
            elif python_expr.dtype == numpy.int16:
                data = python_expr.astype('<i2')
                value_type = ARRAY_TYPES.Integer16
            elif python_expr.dtype == numpy.int32:
                data = python_expr.astype('<i4')
                value_type = ARRAY_TYPES.Integer32
            elif python_expr.dtype == numpy.int64:
                data = python_expr.astype('<i8')
                value_type = ARRAY_TYPES.Integer64
            elif python_expr.dtype == numpy.uint8:
                if self.rawarray_support:
                    value_type = ARRAY_TYPES.UnsignedInteger8
                    data = python_expr.astype('<u1')
                    array_class = WXFExprRawArray
                else :
                    value_type = ARRAY_TYPES.Integer16
                    data = python_expr.astype('<i2')
            elif python_expr.dtype == numpy.uint16:
                if self.rawarray_support:
                    value_type = ARRAY_TYPES.UnsignedInteger16
                    data = python_expr.astype('<u2')
                    array_class = WXFExprRawArray
                else:
                    value_type = ARRAY_TYPES.Integer32
                    data = python_expr.astype('<i4')
            elif python_expr.dtype == numpy.uint32:
                if self.rawarray_support:
                    value_type = ARRAY_TYPES.UnsignedInteger32
                    data = python_expr.astype('<u4')
                    array_class = WXFExprRawArray
                else:
                    value_type = ARRAY_TYPES.Integer64
                    data = python_expr.astype('<i8')
            # no one to one mapping to signed values, even if most of the time
            # the values would fit
            elif python_expr.dtype == numpy.uint64:
                if self.rawarray_support:
                    value_type = ARRAY_TYPES.UnsignedInteger64
                    data = python_expr.astype('<u8')
                    array_class = WXFExprRawArray
                else:
                    TypeError('Cannot represent data of type uint64 as signed int64')
            elif python_expr.dtype == numpy.float32:
                value_type = ARRAY_TYPES.Real32
                data = python_expr
            elif python_expr.dtype == numpy.float64:
                value_type = ARRAY_TYPES.Real64
                data = python_expr
            elif python_expr.dtype == numpy.complex64:
                value_type = ARRAY_TYPES.ComplexReal32
                data = python_expr
            elif python_expr.dtype == numpy.complex128:
                value_type = ARRAY_TYPES.ComplexReal64
                data = python_expr
            else:
                raise NotImplementedError(
                    'NumPy serialization not implemented for %s' % repr(python_expr.dtype)
                )

            if hasattr(data, 'tobytes'):
                #Numpy 1.9+ support array.tobytes, but previous versions don't and use tostring instead.
                yield array_class(python_expr.shape, value_type, data.tobytes())
            else:
                yield array_class(python_expr.shape, value_type, data.tostring())