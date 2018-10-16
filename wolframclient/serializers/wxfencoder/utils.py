# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals


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
