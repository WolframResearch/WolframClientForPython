# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain

from wolframclient.serializers.base import FormatSerializer
from wolframclient.serializers.utils import py_encode_decimal, safe_len
from wolframclient.serializers.wxfencoder.constants import (
    ARRAY_TYPES, WXF_CONSTANTS, WXF_HEADER_COMPRESS, WXF_HEADER_SEPARATOR,
    WXF_VERSION)
from wolframclient.serializers.wxfencoder.utils import (
    float_to_bytes, integer_size, integer_to_bytes, numeric_array_to_wxf,
    varint_bytes, write_varint)
from wolframclient.utils import six
from wolframclient.utils.api import zlib
from wolframclient.utils.encoding import force_bytes, force_text


def get_length(iterable, length=None):
    if length is not None:
        return iterable, length

    length = safe_len(iterable)

    if length is not None:
        return iterable, length

    iterable = tuple(iterable)

    return iterable, len(iterable)


class WXFSerializer(FormatSerializer):
    """ Serialize python objects to WXF. """

    def __init__(self, normalizer=None, compress=False, **opts):
        super(WXFSerializer, self).__init__(normalizer=normalizer, **opts)
        self.compress = compress

    def generate_bytes(self, data):

        yield WXF_VERSION

        if self.compress:
            yield WXF_HEADER_COMPRESS

        yield WXF_HEADER_SEPARATOR

        if self.compress:
            compressor = zlib.compressobj()
            if six.PY2:
                for payload in self.encode(data):
                    yield compressor.compress(six.binary_type(payload))
            else:
                for payload in self.encode(data):
                    yield compressor.compress(payload)
            yield compressor.flush()
        else:
            for payload in self.encode(data):
                yield payload

    def serialize_symbol(self, name):
        yield WXF_CONSTANTS.Symbol
        yield varint_bytes(len(name))
        yield force_bytes(name)

    def serialize_function(self, head, args, **opts):

        iterable, length = get_length(args, **opts)

        return chain((WXF_CONSTANTS.Function, varint_bytes(length)), head,
                     chain.from_iterable(iterable))

    #numeric
    def serialize_int(self, number):
        try:
            wxf_type, int_size = integer_size(number)
            yield wxf_type
            yield integer_to_bytes(number, int_size)

        except ValueError:
            #WXFExprInteger is raising a ValueError if the integer is not in the appropriate bounds.
            #that check needs to be done in case, it's better to do it only once.

            number = b'%i' % number

            yield WXF_CONSTANTS.BigInteger
            yield varint_bytes(len(number))
            yield number

    def serialize_float(self, number):
        yield WXF_CONSTANTS.Real64
        yield float_to_bytes(number)

    def serialize_decimal(self, number):
        number = py_encode_decimal(number)
        yield WXF_CONSTANTS.BigReal
        yield varint_bytes(len(number))
        yield number

    #text / bytes

    def serialize_string(self, string):
        string = force_bytes(string)
        yield WXF_CONSTANTS.String
        yield varint_bytes(len(string))
        yield string

    def serialize_bytes(self, bytes, as_byte_array = not six.PY2):
        if as_byte_array:
            yield WXF_CONSTANTS.BinaryString
            yield varint_bytes(len(bytes))
            yield bytes
        else:
            for token in self.serialize_string(force_text(bytes, encoding = 'iso8859-1')):
                yield token

    def serialize_mapping(self, keyvalue, **opts):
        #the normalizer is always sending an generator key, value

        iterable, length = get_length(keyvalue, **opts)

        return chain((WXF_CONSTANTS.Association, varint_bytes(length)),
                     chain.from_iterable(
                         chain((WXF_CONSTANTS.Rule, ), key, value)
                         for key, value in iterable))

    def serialize_numeric_array(self, data, dimensions, wl_type):
        return numeric_array_to_wxf(data, dimensions, wl_type)
