# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.utils import six
from wolframclient.utils.dispatch import ClassDispatch
from wolframclient.utils.functional import composition, iterate
from wolframclient.utils.importutils import safe_import_string

import datetime
import decimal
import fractions
import inspect
import math

dispatch = ClassDispatch()

class Normalizer(object):

    def __init__(self, normalizer = None, allow_external_objects = False):
        self.normalize = self.chain_normalizers(normalizer)
        self.allow_external_objects = allow_external_objects

    def chain_normalizers(self, func):
        return composition(*map(
            safe_import_string,
            iterate(
                func or (),
                self.default_normalizer
            )
        ))

    @dispatch.multi((bool, six.none_type))
    def default_normalizer(self, o):
        return self.serialize_symbol('%s' % o)

    @dispatch.multi(datetime.datetime)
    def default_normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol(b"DateObject"), (
                self.serialize_iterable((
                    self.serialize_integer(o.year),
                    self.serialize_integer(o.month),
                    self.serialize_integer(o.day),
                    self.serialize_integer(o.hour),
                    self.serialize_integer(o.minute),
                    self.serialize_float(o.second + o.microsecond / 1000000.)
                )),
                self.serialize_string("Instant"),
                self.serialize_string("Gregorian"),
                self.serialize_tzinfo(o)
            )
        )

    @dispatch.multi(datetime.date)
    def default_normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol(b"DateObject"), (
                self.serialize_iterable((
                    self.serialize_integer(o.year),
                    self.serialize_integer(o.month),
                    self.serialize_integer(o.day),
                )),
            )
        )

    @dispatch.multi(datetime.time)
    def default_normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol(b"TimeObject"), (
                self.serialize_iterable((
                    self.serialize_integer(o.hour),
                    self.serialize_integer(o.minute),
                    self.serialize_float(o.second + o.microsecond / 1000000.)
                )),
                self.serialize_rule(
                    self.serialize_symbol(b"TimeZone"),
                    self.serialize_tzinfo(o)
                )
            )
        )

    @dispatch.multi(WLSymbol)
    def default_normalizer(self, o):
        return self.serialize_symbol(o.name)

    @dispatch.multi(WLFunction)
    def default_normalizer(self, o):
        return self.serialize_function(
            self.normalize(o.head),
            tuple(self.normalize(arg) for arg in o.args)
        )

    @dispatch.multi((bytearray, six.binary_type))
    def default_normalizer(self, o):
        return self.serialize_bytes(o)

    @dispatch.multi(six.text_type)
    def default_normalizer(self, o):
        return self.serialize_string(o)

    @dispatch.multi(dict)
    def default_normalizer(self, o):
        return self.serialize_mapping(
            (self.normalize(key), self.normalize(value))
            for key, value in o.items()
        )

    @dispatch.multi(six.integer_types)
    def default_normalizer(self, o):
        return self.serialize_integer(o)

    @dispatch.multi(decimal.Decimal)
    def default_normalizer(self, o):

        if o.is_infinite():
            return self.serialize_function(
                self.serialize_symbol(b"DirectedInfinity"), (
                    self.serialize_integer(o < 0 and -1 or 1),
                )
            )

        if o.is_nan():
            return self.serialize_symbol(b"Indeterminate")

        return self.serialize_decimal(o)

    @dispatch.multi(float)
    def default_normalizer(self, o):

        if math.isinf(o):
            return self.serialize_function(
                self.serialize_symbol(b"DirectedInfinity"), (
                    self.serialize_integer(o < 0 and -1 or 1),
                )
            )

        if math.isnan(o):
            return self.serialize_symbol(b"Indeterminate")

        return self.serialize_float(o)

    @dispatch.multi(fractions.Fraction)
    def default_normalizer(self, o):
        return self.serialize_fraction(o)

    @dispatch.multi(complex)
    def default_normalizer(self, o):
        return self.serialize_complex(o)

    @dispatch.multi(six.iterable_types)
    def default_normalizer(self, o):
        return self.serialize_iterable(
            self.normalize(value)
            for value in o
        )

    @dispatch.multi(WLSerializable)
    def default_normalizer(self, o):
        return self.normalize(o.to_wl())

    @dispatch.default()
    def default_normalizer(self, o):
        if not inspect.isclass(o) and hasattr(o, '__iter__'):
            return self.serialize_iterable(
                self.normalize(value)
                for value in o
            )
        if self.allow_external_objects:
            return self.serialize_external_object(o)

        raise NotImplementedError('Cannot serialize object of class %s' % o.__class__)