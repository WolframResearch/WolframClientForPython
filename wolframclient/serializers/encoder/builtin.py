# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import math

from wolframclient.serializers.utils import safe_len
from wolframclient.utils import six
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.encoding import force_bytes, force_text
from wolframclient.utils.functional import identity, map

encoder = Dispatch()

if six.PY2:
    #in py2 if you construct use dict(a=2) then "a" is binary
    #since using bytes as keys is a legit operation we are only fixing py2 here

    def safe_key(key):
        if isinstance(key, six.binary_type):
            return force_text(key)
        return key

    def _to_key_value(func, serializer, o):
        return func(((serializer.encode(safe_key(key)), serializer.encode(value))
             for key, value in o.items()), length = safe_len(o))
else:
    def _to_key_value(func, serializer, o):
        return func(((serializer.encode(key), serializer.encode(value))
             for key, value in o.items()), length = safe_len(o))

@encoder.dispatch((bool, six.none_type))
def encode_none(serializer, o):
    return serializer.serialize_symbol(force_bytes(o))


@encoder.dispatch((bytearray, six.binary_type, six.buffer_types))
def encode_bytes(serializer, o):
    return serializer.serialize_bytes(o)


@encoder.dispatch(six.text_type)
def encode_text(serializer, o):
    return serializer.serialize_string(o)


@encoder.dispatch(dict)
def encode_dict(serializer, o):
    return _to_key_value(serializer.serialize_mapping, serializer, o)


@encoder.dispatch(six.integer_types)
def encode_int(serializer, o):
    return serializer.serialize_int(o)


@encoder.dispatch(float)
def encode_float(serializer, o):

    if math.isinf(o):
        return serializer.serialize_function(
            serializer.serialize_symbol(b"DirectedInfinity"),
            (serializer.serialize_int(o < 0 and -1 or 1), ))

    if math.isnan(o):
        return serializer.serialize_symbol(b"Indeterminate")

    return serializer.serialize_float(o)


@encoder.dispatch(complex)
def encode_complex(serializer, o):
    return serializer.serialize_complex(o)


@encoder.dispatch(six.iterable_types)
def encode_iter(serializer, o):
    return serializer.serialize_iterable(
        map(serializer.encode, o), length=safe_len(o))
