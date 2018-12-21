# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from operator import methodcaller

from wolframclient.utils.api import numpy
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import identity, map

encoder = Dispatch()

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


@encoder.dispatch(numpy.ndarray)
def encode_ndarray(serializer, o):

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

    return serializer.serialize_numeric_array(data, o.shape, wl_type)


@encoder.dispatch(numpy.integer)
def encode_numpy_int(serializer, o):
    return serializer.serialize_int(int(o))


@encoder.dispatch(numpy.floating)
def encode_numpy_floating(serializer, o):
    # mantissa, and base 2 exponent.
    mantissa, exp = numpy.frexp(o)
    return serializer.serialize_function(
        serializer.serialize_symbol(b'Times'), (
            serializer.serialize_float(mantissa),
            serializer.serialize_function(
                serializer.serialize_symbol(b'Power'),
                (
                    serializer.serialize_int(2),
                    serializer.serialize_float(exp),
                ),
            ),
        ))


@encoder.dispatch((numpy.float16, numpy.float32, numpy.float64))
def encode_numpy_mp_float(serializer, o):
    return serializer.serialize_float(o)


@encoder.dispatch(numpy.complexfloating)
def encode_complex(serializer, o):
    return serializer.serialize_complex(o)
