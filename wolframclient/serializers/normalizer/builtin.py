# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import math

from wolframclient.serializers.utils import safe_len
from wolframclient.utils import six
from wolframclient.utils.encoding import force_bytes, force_text
from wolframclient.utils.functional import identity

if six.PY2:
    #in py2 if you construct use dict(a=2) then "a" is binary
    #since using bytes as keys is a legit operation we are only fixing py2 here

    def safe_key(key):
        if isinstance(key, six.binary_type):
            return force_text(key)
        return key
else:
    safe_key = identity


def update_dispatch(dispatch):
    @dispatch.multi((bool, six.none_type))
    def normalizer(self, o):
        return self.serialize_symbol(force_bytes(o))

    @dispatch.multi((bytearray, six.binary_type, six.buffer_types))
    def normalizer(self, o):
        return self.serialize_bytes(o)

    @dispatch.multi(six.text_type)
    def normalizer(self, o):
        return self.serialize_string(o)

    @dispatch.multi(dict)
    def normalizer(self, o):
        return self.serialize_mapping(
            ((self.normalize(safe_key(key)), self.normalize(value))
             for key, value in o.items()),
            length=safe_len(o))

    @dispatch.multi(six.integer_types)
    def normalizer(self, o):
        return self.serialize_int(o)

    @dispatch.multi(float)
    def normalizer(self, o):

        if math.isinf(o):
            return self.serialize_function(
                self.serialize_symbol(b"DirectedInfinity"),
                (self.serialize_int(o < 0 and -1 or 1), ))

        if math.isnan(o):
            return self.serialize_symbol(b"Indeterminate")

        return self.serialize_float(o)

    @dispatch.multi(complex)
    def normalizer(self, o):
        return self.serialize_complex(o)

    @dispatch.multi(six.iterable_types)
    def normalizer(self, o):
        return self.serialize_iterable((self.normalize(value) for value in o),
                                       length=safe_len(o))
