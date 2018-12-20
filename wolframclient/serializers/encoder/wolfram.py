# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import (WLFunction, WLInputExpression,
                                               WLSymbol)
from wolframclient.serializers.encoder.builtin import safe_key
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.serializers.utils import safe_len
from wolframclient.utils.datastructures import Association
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import map

encoder = Dispatch()


@encoder.dispatch(WLSymbol)
def encode_symbol(serializer, o):
    return serializer.serialize_symbol(o.name)


@encoder.dispatch(WLFunction)
def encode_function(serializer, o):
    return serializer.serialize_function(
        serializer.encode(o.head),
        map(serializer.encode, o.args),
        length=len(o.args))


@encoder.dispatch(WLInputExpression)
def encode_inputexpr(serializer, o):
    return serializer.serialize_input_form(o.input)


@encoder.dispatch(WLSerializable)
def encode_serializable(serializer, o):
    return serializer.encode(o.to_wl())


@encoder.dispatch(Association)
def encode_association(serializer, o):
    return serializer.serialize_association(
        ((serializer.encode(safe_key(key)), serializer.encode(value))
         for key, value in o.items()),
        length=safe_len(o))
