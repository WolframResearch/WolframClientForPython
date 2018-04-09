# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.utils import six
from wolframclient.utils.dispatch import ClassDispatch
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import composition, first, iterate
from wolframclient.utils.importutils import safe_import_string

import datetime
import decimal
import fractions
import inspect
import math
import re

dispatch = ClassDispatch()

class FormatSerializer(object):

    types = None

    def __init__(self, normalizer = None, allow_external_objects = False):
        self.normalize = self.chain_normalizers(normalizer)
        self.allow_external_objects = allow_external_objects

    def dump(self, data, stream):
        raise NotImplementedError

    def export(self, data, stream = None):
        if stream:
            if isinstance(stream, six.string_types):
                with open(stream, 'wb') as file:
                    self.dump(data, file)
                    return stream

            self.dump(data, stream)
            return stream

        stream = six.BytesIO()
        stream = self.dump(data, stream)
        stream.seek(0)
        return stream.read()

    def chain_normalizers(self, func):
        return composition(*map(
            safe_import_string,
            iterate(
                func or (),
                self.default_normalizer
            )
        ))

    def _normalize_tzinfo(self, date, name_match = re.compile('^[A-Za-z]+(/[A-Za-z]+)?$')):

        if date.tzinfo is None:
            return self.serialize_symbol("$TimeZone")

        name = date.tzinfo.tzname(None)

        if name and name_match.match(name):
            return self.serialize_string(name)

        return self.serialize_float(date.utcoffset().total_seconds() / 3600)

    @dispatch.multi((bool, six.none_type))
    def default_normalizer(self, o):
        return self.serialize_symbol('%s' % o)

    @dispatch.multi(datetime.datetime)
    def default_normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol("DateObject"), (
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
                self._normalize_tzinfo(o)
            )
        )

    @dispatch.multi(datetime.date)
    def default_normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol("DateObject"), (
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
            self.serialize_symbol("TimeObject"), (
                self.serialize_iterable((
                    self.serialize_integer(o.hour),
                    self.serialize_integer(o.minute),
                    self.serialize_float(o.second + o.microsecond / 1000000.)
                )),
                self.serialize_rule(
                    self.serialize_symbol("TimeZone"),
                    self._normalize_tzinfo(o)
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

    #implementation of several methods

    def serialize_function(self, head, args):
        raise NotImplementedError

    def serialize_symbol(self, symbol):
        raise NotImplementedError

    def serialize_string(self, obj):
        raise NotImplementedError

    def serialize_bytes(self, obj):
        raise NotImplementedError

    def serialize_float(self, obj):
        raise NotImplementedError

    def serialize_decimal(self, obj):
        raise NotImplementedError

    def serialize_integer(self, obj):
        raise NotImplementedError

    def serialize_iterable(self, iterable):
        return self.serialize_function(
            self.serialize_symbol('List'),
            iterable
        )

    def serialize_mapping(self, mappable):
        return self.serialize_function(
            self.serialize_symbol('Association'), (
                self.serialize_rule(key, value)
                for key, value in mappable
            )
        )

    def serialize_fraction(self, o):
        return self.serialize_function(
            self.serialize_symbol('Rational'), (
                self.serialize_integer(o.numerator),
                self.serialize_integer(o.denominator)
            )
        )

    def serialize_complex(self, o):
        return self.serialize_function(
            self.serialize_symbol('Complex'), (
                self.serialize_float(o.real),
                self.serialize_float(o.imag),
            )
        )

    def serialize_rule(self, lhs, rhs):
        return self.serialize_function(
            self.serialize_symbol('Rule'), (
                lhs,
                rhs
            )
        )

    def serialize_rule_delayed(self, lhs, rhs):
        return self.serialize_function(
            self.serialize_symbol('RuleDelayed'), (
                lhs,
                rhs
            )
        )

    def _serialize_external_object(self, o):

        yield "Type",       "PythonFunction"
        yield "Name",       force_text(o.__name__)
        yield "BuiltIn",    inspect.isbuiltin(o),

        is_module = inspect.ismodule(o)

        yield "IsModule", is_module

        if not is_module:
            module = inspect.getmodule(o)
            if module:
                yield "Module", force_text(module.__name__)

        yield "IsClass",    inspect.isclass(o),
        yield "IsFunction", inspect.isfunction(o),
        yield "IsMethod",   inspect.ismethod(o),
        yield "Callable",   callable(o)

        if callable(o):
            yield "Arguments", first(inspect.getargspec(o))

    def serialize_external_object(self, obj):
        return self.serialize_function(
            self.serialize_symbol('ExternalObject'), (
                self.serialize_mapping(
                    (self.normalize(key), self.normalize(value))
                    for key, value in self._serialize_external_object(obj)
                ),
            )
        )