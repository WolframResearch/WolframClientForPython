# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLFunction, WLSymbol, WLInputExpression
from wolframclient.serializers.encoder import wolfram_encoder
from wolframclient.serializers.encoder.builtin import safe_key
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.serializers.utils import safe_len
from wolframclient.utils.datastructures import Association

@wolfram_encoder.register(WLSymbol)
def encode_symbol(serializer, o):
    return serializer.serialize_symbol(o.name)

@wolfram_encoder.register(WLFunction)
def encode_function(serializer, o):
    return serializer.serialize_function(
        serializer.encode(o.head), tuple(
            serializer.encode(arg) for arg in o.args))

@wolfram_encoder.register(WLInputExpression)
def encode_inputexpr(serializer, o):
    return serializer.serialize_input_form(o.input)

@wolfram_encoder.register(WLSerializable)
def encode_serializable(serializer, o):
    return serializer.encode(o.to_wl())

@wolfram_encoder.register(Association)
def encode_association(serializer, o):
    return serializer.serialize_association(
        ((serializer.encode(safe_key(key)), serializer.encode(value))
            for key, value in o.items()),
        length=safe_len(o))
