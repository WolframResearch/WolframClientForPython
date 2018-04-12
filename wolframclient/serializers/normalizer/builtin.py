# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six

import math

def update_dispatch(dispatch):

    @dispatch.multi((bool, six.none_type))
    def normalizer(self, o):
        return self.serialize_symbol('%s' % o)

    @dispatch.multi((bytearray, six.binary_type))
    def normalizer(self, o):
        return self.serialize_bytes(o)

    @dispatch.multi(six.text_type)
    def normalizer(self, o):
        return self.serialize_string(o)

    @dispatch.multi(dict)
    def normalizer(self, o):
        return self.serialize_mapping(
            (self.normalize(key), self.normalize(value))
            for key, value in o.items()
        )

    @dispatch.multi(six.integer_types)
    def normalizer(self, o):
        return self.serialize_int(o)

    @dispatch.multi(float)
    def normalizer(self, o):

        if math.isinf(o):
            return self.serialize_function(
                self.serialize_symbol(b"DirectedInfinity"), (
                    self.serialize_int(o < 0 and -1 or 1),
                )
            )

        if math.isnan(o):
            return self.serialize_symbol(b"Indeterminate")

        return self.serialize_float(o)

    @dispatch.multi(complex)
    def normalizer(self, o):
        return self.serialize_complex(o)

    @dispatch.multi(six.iterable_types)
    def normalizer(self, o):
        return self.serialize_iterable(
            self.normalize(value)
            for value in o
        )