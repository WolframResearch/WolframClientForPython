# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from operator import methodcaller

from wolframclient.utils.api import numpy
from wolframclient.utils.functional import identity

NUMPY_MAPPING = {
    numpy.dtype('int8'): ('Integer8', methodcaller('astype', '<i1')),
    numpy.dtype('int16'): ('Integer16', methodcaller('astype', '<i2')),
    numpy.dtype('int32'): ('Integer32', methodcaller('astype', '<i4')),
    numpy.dtype('int64'): ('Integer64', methodcaller('astype', '<i8')),
    numpy.dtype('uint8'): ('UnsignedInteger8', methodcaller('astype', '<u1')),
    numpy.dtype('uint16'): ('UnsignedInteger16', methodcaller('astype',
                                                              '<u2')),
    numpy.dtype('uint32'): ('UnsignedInteger32', methodcaller('astype',
                                                              '<u4')),
    numpy.dtype('uint64'): ('UnsignedInteger64', methodcaller('astype',
                                                              '<u8')),
    numpy.dtype('float32'): ('Real32', identity),
    numpy.dtype('float64'): ('Real64', identity),
    numpy.dtype('complex64'): ('ComplexReal32', identity),
    numpy.dtype('complex128'): ('ComplexReal64', identity),
}


def update_dispatch(dispatch):
    @dispatch.multi(numpy.ndarray)
    def normalizer(self, o):

        try:
            wl_type, handler = NUMPY_MAPPING[o.dtype]
        except KeyError:
            raise NotImplementedError(
                'NumPy serialization not implemented for %s. Choices are: %s' %
                (repr(o.dtype), ', '.join(map(repr, NUMPY_MAPPING.keys()))))

        data = handler(o)

        if hasattr(o, 'tobytes'):
            #Numpy 1.9+ support array.tobytes, but previous versions don't and use tostring instead.
            data = o.tobytes()
        else:
            data = o.tostring()

        return self.serialize_numeric_array(data, o.shape, wl_type)
