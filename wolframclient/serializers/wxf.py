# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain

from wolframclient.serializers.base import FormatSerializer
from wolframclient.serializers.utils import py_encode_decimal
from wolframclient.serializers.wxfencoder.constants import (
    ARRAY_TYPES, WXF_CONSTANTS, WXF_HEADER_COMPRESS, WXF_HEADER_SEPARATOR,
    WXF_VERSION)
from wolframclient.serializers.wxfencoder.streaming import ZipCompressedWriter
from wolframclient.serializers.wxfencoder.utils import (
    float_to_bytes, integer_size, integer_to_bytes, varint_bytes, write_varint)
from wolframclient.utils.encoding import force_bytes


class WXFSerializer(FormatSerializer):
    ''' TODO
    '''

    def __init__(self, normalizer=None, compress=False, enforce=False, **opts):
        super(WXFSerializer, self).__init__(normalizer=normalizer, **opts)
        self.compress = compress
        self.enforce = enforce

    def dump(self, data, stream):

        stream.write(WXF_VERSION)

        if self.compress:
            stream.write(WXF_HEADER_COMPRESS)

        stream.write(WXF_HEADER_SEPARATOR)

        if self.compress:
            with ZipCompressedWriter(stream) as zstream:
                for payload in self.normalize(data):
                    zstream.write(payload)
        else:
            for payload in self.normalize(data):
                stream.write(payload)

        return stream

    def serialize_symbol(self, name):
        yield WXF_CONSTANTS.Symbol
        yield varint_bytes(len(name))
        yield force_bytes(name)

    def serialize_function(self, head, args):
        #args is always a tuple

        try:
            l = len(args)
        except TypeError:
            #generator might not have a length
            args = tuple(args)
            l = len(args)

        return chain((WXF_CONSTANTS.Function, varint_bytes(l)), head,
                     chain.from_iterable(args))

    #numeric
    def serialize_int(self, number):
        try:
            wxf_type, int_size = integer_size(number)
            yield wxf_type
            yield integer_to_bytes(number, int_size)

        except ValueError:
            #WXFExprInteger is raising a ValueError if the integer is not in the appropriate bounds.
            #that check needs to be done in case, it's better to do it only once.

            number = force_bytes(number)

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

    def serialize_bytes(self, bytes):
        yield WXF_CONSTANTS.BinaryString
        yield varint_bytes(len(bytes))
        yield bytes

    def serialize_mapping(self, keyvalue):
        #the normalizer is always sending an generator key, value
        keyvalue = tuple(keyvalue)

        return chain((WXF_CONSTANTS.Association, varint_bytes(len(keyvalue))),
                     chain.from_iterable(
                         chain((WXF_CONSTANTS.RuleDelayed, ), key, value)
                         for key, value in keyvalue))

    def serialize_raw_array(self, data, dimensions, wl_type):
        yield WXF_CONSTANTS.RawArray
        yield ARRAY_TYPES[wl_type]
        yield varint_bytes(len(dimensions))
        for dim in dimensions:
            yield varint_bytes(dim)
        yield data
