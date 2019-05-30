# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder.constants import (
    ARRAY_TYPES,
    VALID_PACKED_ARRAY_TYPES,
    WXF_CONSTANTS,
    StructDouble,
    StructFloat,
    StructInt8LE,
    StructUInt8LE,
    StructInt16LE,
    StructUInt16LE,
    StructInt32LE,
    StructUInt32LE,
    StructInt64LE,
    StructUInt64LE
)
from wolframclient.utils import six
from wolframclient.exception import WolframLanguageException

if six.JYTHON:
    import jarray


def write_varint(int_value, stream):
    """Serialize `int_value` into varint bytes and write them to
    `stream`, return the stream.
    """
    stream.write(varint_bytes(int_value))


def varint_bytes(int_value):
    """Serialize `int_value` into varint bytes and return them as a byetarray."""
    buf = bytearray(9)
    if int_value < 0:
        raise TypeError("Negative values cannot be encoded as varint.")
    count = 0
    while True:
        next = int_value & 0x7F
        int_value >>= 7
        if int_value:
            buf[count] = next | 0x80
            count += 1
        else:
            buf[count] = next
            count += 1
            break

    return buf[:count]


_exceptions = {
    0: (WXF_CONSTANTS.Integer8, 1),
    -(1 << 7): (WXF_CONSTANTS.Integer8, 1),
    -(1 << 15): (WXF_CONSTANTS.Integer16, 2),
    -(1 << 31): (WXF_CONSTANTS.Integer32, 4),
    -(1 << 63): (WXF_CONSTANTS.Integer64, 8),
}
_size = dict(
    (j, (WXF_CONSTANTS["Integer%i" % ih], ih // 8))
    for il, ih in ((1, 8), (9, 16), (17, 32), (33, 64))
    for j in range(il, ih + 1)
)


def integer_size(value):
    try:
        return _exceptions.get(value, None) or _size[value.bit_length() + 1]
    except KeyError:
        raise ValueError("Value %i is not a machine-sized integer." % value)


_packing = {1: StructInt8LE, 2: StructInt16LE, 4: StructInt32LE, 8: StructInt64LE}

if six.JYTHON:

    def integer_to_bytes(value, int_size):
        buffer = jarray.zeros(8, "c")
        _packing.get(int_size).pack_into(buffer, 0, value)
        return buffer[:int_size].tostring()


elif six.PY2:

    def integer_to_bytes(value, int_size):
        buffer = bytearray(8)
        _packing.get(int_size).pack_into(buffer, 0, value)
        return buffer[:int_size]


else:

    def integer_to_bytes(value, int_size):
        return value.to_bytes(int_size, byteorder="little", signed=True)


if six.JYTHON:

    def float_to_bytes(value):
        buffer = jarray.zeros(8, "c")
        StructDouble.pack_into(buffer, 0, value)
        return buffer.tostring()


else:

    def float_to_bytes(value):
        buffer = bytearray(8)
        StructDouble.pack_into(buffer, 0, value)
        return buffer


def valid_dimension_or_fail(dimension):
    if dimension <= 0:
        raise WolframLanguageException('Invalid array dimensions: %s. Expecting strictly positive integer.' % dimension)


def array_to_wxf(wxf_token, data, dimensions, array_type_token):
    yield wxf_token
    yield array_type_token
    yield varint_bytes(len(dimensions))
    for dim in dimensions:
        valid_dimension_or_fail(dim)
        yield varint_bytes(dim)
    yield data


def numeric_array_to_wxf(data, dimensions, wl_type):
    return array_to_wxf(WXF_CONSTANTS.NumericArray, data, dimensions, ARRAY_TYPES[wl_type])


def packed_array_to_wxf(data, dimensions, wl_type):
    array_type_token = ARRAY_TYPES[wl_type]
    if array_type_token not in VALID_PACKED_ARRAY_TYPES:
        raise ValueError('Invalid PackedArray type %s' % array_type_token)
    return array_to_wxf(WXF_CONSTANTS.PackedArray, data, dimensions, array_type_token)


def array_to_list(data, dimensions, wl_type):
    for dimension in dimensions:
        valid_dimension_or_fail(dimension)
    return _array_to_list(data, dimensions, ARRAY_TYPES[wl_type])


if hasattr(memoryview, "cast"):
    unpack_mapping = {
        ARRAY_TYPES.Integer8: "b",
        ARRAY_TYPES.UnsignedInteger8: "B",
        ARRAY_TYPES.Integer16: "h",
        ARRAY_TYPES.UnsignedInteger16: "H",
        ARRAY_TYPES.Integer32: "i",
        ARRAY_TYPES.UnsignedInteger32: "I",
        ARRAY_TYPES.Integer64: "q",
        ARRAY_TYPES.UnsignedInteger64: "Q",
        ARRAY_TYPES.Real32: "f",
        ARRAY_TYPES.Real64: "d",
        ARRAY_TYPES.ComplexReal32: "f",
        ARRAY_TYPES.ComplexReal64: "d",
    }

    def _to_complex(array, max_depth, curr_depth):
        # recursivelly traverse the array until the last (real) dimension is reached
        # it correspond to an array of (fake) array of two elements (real and im parts).
        if curr_depth < max_depth - 1:
            for sub in array:
                _to_complex(sub, max_depth, curr_depth + 1)
            return
        # iterate over the pairs
        for index, complex_pair in enumerate(array):
            array[index] = complex(*complex_pair)


    def _array_to_list(data, shape, array_type):
        view = memoryview(data)
        if (
                array_type == ARRAY_TYPES.ComplexReal32
                or array_type == ARRAY_TYPES.ComplexReal64
        ):
            dimensions = list(shape)
            array_depth = len(dimensions)
            # In the given array, 2 reals give one complex,
            # adding one last dimension to represent it.
            dimensions.append(2)
            as_list = view.cast(
                unpack_mapping[array_type], shape=dimensions
            ).tolist()
            _to_complex(as_list, array_depth, 0)
            return as_list
        else:
            return view.cast(
                unpack_mapping[array_type],
                shape=shape,
            ).tolist()

else:
    unpack_mapping = {
        ARRAY_TYPES.Integer8: StructInt8LE,
        ARRAY_TYPES.UnsignedInteger8: StructUInt8LE,
        ARRAY_TYPES.Integer16: StructInt16LE,
        ARRAY_TYPES.UnsignedInteger16: StructUInt16LE,
        ARRAY_TYPES.Integer32: StructInt32LE,
        ARRAY_TYPES.UnsignedInteger32: StructUInt32LE,
        ARRAY_TYPES.Integer64: StructInt64LE,
        ARRAY_TYPES.UnsignedInteger64: StructUInt64LE,
        ARRAY_TYPES.Real32: StructFloat,
        ARRAY_TYPES.Real64: StructDouble,
        ARRAY_TYPES.ComplexReal32: StructFloat,
        ARRAY_TYPES.ComplexReal64: StructDouble,
    }


    def _array_to_list(data, shape, array_type):
        value, _ = _build_array_from_bytes(
            data, 0, array_type, shape, 0
        )
        return value

    def _build_array_from_bytes(data, offset, array_type, dimensions, current_dim):
        new_array = list()
        if current_dim < len(dimensions) - 1:
            for i in range(dimensions[current_dim]):
                new_elem, offset = _build_array_from_bytes(
                    data, offset, array_type, dimensions, current_dim + 1
                )
                new_array.append(new_elem)
        else:
            struct = unpack_mapping[array_type]
            # complex values, need two reals for each.
            if (
                array_type == ARRAY_TYPES.ComplexReal32
                or array_type == ARRAY_TYPES.ComplexReal64
            ):
                for i in range(dimensions[-1]):
                    # this returns a tuple.
                    re = struct.unpack_from(data, offset=offset)
                    offset = offset + struct.size
                    im = struct.unpack_from(data, offset=offset)
                    offset = offset + struct.size
                    new_array.append(complex(re[0], im[0]))
            else:
                for i in range(dimensions[-1]):
                    # this returns a tuple.
                    value = struct.unpack_from(data, offset=offset)
                    offset = offset + struct.size
                    new_array.append(value[0])
        return new_array, offset
