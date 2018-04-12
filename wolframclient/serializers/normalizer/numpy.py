# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from operator import methodcaller

from wolframclient.utils.functional import identity

import numpy

NUMPY_MAPPING = {
    numpy.int8:       ('Integer8',          methodcaller('astype', '<i1')),
    numpy.int16:      ('Integer16',         methodcaller('astype', '<i2')),
    numpy.int32:      ('Integer32',         methodcaller('astype', '<i4')),
    numpy.int64:      ('Integer64',         methodcaller('astype', '<i8')),
    numpy.uint8:      ('UnsignedInteger8',  methodcaller('astype', '<u1')),
    numpy.uint16:     ('UnsignedInteger16', methodcaller('astype', '<u2')),
    numpy.uint32:     ('UnsignedInteger32', methodcaller('astype', '<u4')),
    numpy.uint64:     ('UnsignedInteger64', methodcaller('astype', '<u8')),
    numpy.float32:    ('Real32',            identity),
    numpy.float64:    ('Real64',            identity),
    numpy.complex64:  ('ComplexReal32',     identity),
    numpy.complex128: ('ComplexReal64',     identity),
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
            data = data.tobytes()
        else:
            data = data.tostring()

        return self.serialize_raw_array(
            data,
            o.shape,
            value_type,
        )