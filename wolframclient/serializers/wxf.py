# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain

from wolframclient.serializers.base import FormatSerializer
from wolframclient.serializers.wxfencoder.serializer import WXFExprSerializer
from wolframclient.serializers.wxfencoder.wxfexpr import ARRAY_TYPES
from wolframclient.serializers.utils import py_encode_decimal

import wolframclient.serializers.wxfencoder.wxfexpr as wxfexpr

class WXFSerializer(FormatSerializer):

    def __init__(self, normalizer = None, compress = False, enforce = False, **opts):
        super(WXFSerializer, self).__init__(normalizer = normalizer, **opts)
        self.compress = compress
        self.enforce  = enforce

    def dump(self, data, stream):
        serializer = WXFExprSerializer(stream, enforce=self.enforce, compress = self.compress)
        serializer.serialize(self.normalize(data))
        return stream

    def serialize_symbol(self, name):
        yield wxfexpr.WXFExprSymbol(name)

    def serialize_function(self, head, args):
        #args is always a tuple

        try:
            l = len(args)
        except TypeError:
            #generator might not have a length
            args = tuple(args)
            l = len(args)

        return chain(
            (wxfexpr.WXFExprFunction(l), ),
            head,
            chain.from_iterable(args)
        )

    #numeric
    def serialize_int(self, number):
        try:
            yield wxfexpr.WXFExprInteger(number)
        except ValueError:
            #WXFExprInteger is raising a ValueError if the integer is not in the appropriate bounds.
            #that check needs to be done in case, it's better to do it only once.
            yield wxfexpr.WXFExprBigInteger('%i' % number)

    def serialize_float(self, number):
        yield wxfexpr.WXFExprReal(number)

    def serialize_decimal(self, number):
        yield wxfexpr.WXFExprBigReal(py_encode_decimal(number))

    #text / bytes

    def serialize_string(self, string):
        yield wxfexpr.WXFExprString(string)

    def serialize_bytes(self, bytes):
        yield wxfexpr.WXFExprBinaryString(bytes)

    def serialize_mapping(self, keyvalue):
        #the normalizer is always sending an generator key, value
        keyvalue = tuple(keyvalue)
        return chain(
            (wxfexpr.WXFExprAssociation(len(keyvalue)), ),
            chain.from_iterable(
                chain((wxfexpr.WXFExprRule(), ), key, value)
                for key, value in keyvalue
            )
        )

    def serialize_raw_array(self, data, shape, wl_type):

        yield wxfexpr.WXFExprRawArray(
            shape,
            ARRAY_TYPES[wl_type],
            data
        )