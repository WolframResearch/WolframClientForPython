# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from operator import methodcaller

from wolframclient.serializers.wxfencoder.wxfexpr import ARRAY_TYPES
from wolframclient.utils.functional import identity

import numpy

NUMPY_MAPPING = {
    numpy.int8:       (ARRAY_TYPES.Integer8,          methodcaller('astype', '<i1')),
    numpy.int16:      (ARRAY_TYPES.Integer16,         methodcaller('astype', '<i2')),
    numpy.int32:      (ARRAY_TYPES.Integer32,         methodcaller('astype', '<i4')),
    numpy.int64:      (ARRAY_TYPES.Integer64,         methodcaller('astype', '<i8')),
    numpy.uint8:      (ARRAY_TYPES.UnsignedInteger8,  methodcaller('astype', '<u1')),
    numpy.uint16:     (ARRAY_TYPES.UnsignedInteger16, methodcaller('astype', '<u2')),
    numpy.uint32:     (ARRAY_TYPES.UnsignedInteger32, methodcaller('astype', '<u4')),
    numpy.uint64:     (ARRAY_TYPES.UnsignedInteger64, methodcaller('astype', '<u8')),
    numpy.float32:    (ARRAY_TYPES.Real32,            identity),
    numpy.float64:    (ARRAY_TYPES.Real64,            identity),
    numpy.complex64:  (ARRAY_TYPES.ComplexReal32,     identity),
    numpy.complex128: (ARRAY_TYPES.ComplexReal64,     identity),
}

def update_dispatch(dispatch):

    @dispatch.multi(numpy.ndarray)
    def normalizer(self, o):

        try:
            value_type, handler = NUMPY_MAPPING[o.dtype]
        except KeyError:
            raise NotImplementedError('NumPy serialization not implemented for %s' % repr(o.dtype))

        data = handler(o)

        if hasattr(data, 'tobytes'):
            #Numpy 1.9+ support array.tobytes, but previous versions don't and use tostring instead.
            yield array_class(python_expr.shape, value_type, data.tobytes())
        else:
            yield array_class(python_expr.shape, value_type, data.tostring())

        return self.serialize_raw_array(
            o.shape,
            value_type,
            data
        )