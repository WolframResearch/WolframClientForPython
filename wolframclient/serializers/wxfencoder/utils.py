# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder.constants import (
    ARRAY_TYPES, VALID_PACKED_ARRAY_TYPES, WXF_CONSTANTS, StructDouble,
    StructInt8LE, StructInt16LE, StructInt32LE, StructInt64LE)
from wolframclient.utils import six

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
        raise TypeError('Negative values cannot be encoded as varint.')
    count = 0
    while True:
        next = int_value & 0x7f
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
_size = dict((j, (WXF_CONSTANTS['Integer%i' % ih], ih // 8))
             for il, ih in ((1, 8), (9, 16), (17, 32), (33, 64))
             for j in range(il, ih + 1))


def integer_size(value):
    try:
        return _exceptions.get(value, None) or _size[value.bit_length() + 1]
    except KeyError:
        raise ValueError('Value %i is not a machine-sized integer.' % value)


_packing = {
    1: StructInt8LE,
    2: StructInt16LE,
    4: StructInt32LE,
    8: StructInt64LE
}

if six.JYTHON:

    def integer_to_bytes(value, int_size):
        buffer = jarray.zeros(8, 'c')
        _packing.get(int_size).pack_into(buffer, 0, value)
        return buffer[:int_size].tostring()

elif six.PY2:

    def integer_to_bytes(value, int_size):
        buffer = bytearray(8)
        _packing.get(int_size).pack_into(buffer, 0, value)
        return buffer[:int_size]
else:

    def integer_to_bytes(value, int_size):
        return value.to_bytes(int_size, byteorder='little', signed=True)


if six.JYTHON:

    def float_to_bytes(value):
        buffer = jarray.zeros(8, 'c')
        StructDouble.pack_into(buffer, 0, value)
        return buffer.tostring()
else:

    def float_to_bytes(value):
        buffer = bytearray(8)
        StructDouble.pack_into(buffer, 0, value)
        return buffer


def numeric_array_to_wxf(data, dimensions, wl_type):
    yield WXF_CONSTANTS.NumericArray
    yield ARRAY_TYPES[wl_type]
    yield varint_bytes(len(dimensions))
    for dim in dimensions:
        yield varint_bytes(dim)
    yield data
