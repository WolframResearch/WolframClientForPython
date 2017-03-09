# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from functools import reduce

from wolframclient.language.expression import Expression
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.settings import settings
from wolframclient.utils import six
from wolframclient.utils.dispatch import ClassDispatch
from wolframclient.utils.functional import composition, first, iterate, last
from wolframclient.utils.importutils import safe_import_string

import datetime
import decimal
import fractions
import math
import re

dispatch = ClassDispatch()

class FormatSerializer(object):

    types = None

    def __init__(self, normalizer = None):
        self.normalize  = self.chain_normalizers(normalizer)

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
                func or settings.NORMALIZATION_FUNCTION or (),
                self.default_normalizer
            )
        ))

    def _normalize_tzinfo(self, date, name_match = re.compile('^[A-Za-z]+(/[A-Za-z]+)?$')):

        if date.tzinfo is None:
            return self.types.WLSymbol("$TimeZone")

        name = date.tzinfo.tzname(None)

        if name and name_match.match(name):
            return self.types.WLString(name)

        return self.types.WLFloat(date.utcoffset().total_seconds() / 3600)

    @dispatch.multi((bool, six.none_type))
    def default_normalizer(self, o):
        return self.types.WLSymbol('%s' % o)

    @dispatch.multi(datetime.datetime)
    def default_normalizer(self, o):
        return self.types.WLFunction(
            self.types.WLSymbol("DateObject"), (
                self.types.WLList((
                    self.types.WLInteger(o.year),
                    self.types.WLInteger(o.month),
                    self.types.WLInteger(o.day),
                    self.types.WLInteger(o.hour),
                    self.types.WLInteger(o.minute),
                    self.types.WLFloat(o.second + o.microsecond / 1000000.)
                )),
                self.types.WLString("Instant"),
                self.types.WLString("Gregorian"),
                self._normalize_tzinfo(o)
            )
        )

    @dispatch.multi(datetime.date)
    def default_normalizer(self, o):
        return self.types.WLFunction(
            self.types.WLSymbol("DateObject"), (
                self.types.WLList((
                    self.types.WLInteger(o.year),
                    self.types.WLInteger(o.month),
                    self.types.WLInteger(o.day),
                )),
            )
        )

    @dispatch.multi(datetime.time)
    def default_normalizer(self, o):
        return self.types.WLFunction(
            self.types.WLSymbol("TimeObject"), (
                self.types.WLList((
                    self.types.WLInteger(o.hour),
                    self.types.WLInteger(o.minute),
                    self.types.WLFloat(o.second + o.microsecond / 1000000.)
                )),
                self.types.WLRule(
                    self.types.WLSymbol("TimeZone"),
                    self._normalize_tzinfo(o)
                )
            )
        )

    @dispatch.multi(Expression)
    def default_normalizer(self, o):
        return reduce(
            lambda head, args: self.types.WLFunction(
                head,
                tuple(
                    iterate(
                        (self.normalize(arg) for arg in first(args)),
                        (self.types.WLRule(
                            self.types.WLSymbol(o.fully_qualified_symbol(key)),
                            self.normalize(value)
                            ) for key, value in last(args).items()
                        ),
                    )
                )
            ),
            o.args,
            self.types.WLSymbol(o.fully_qualified_symbol())
        )

    @dispatch.multi(bytearray)
    def default_normalizer(self, o):
        return self.types.WLByteArray(o)

    if six.PY3:
        @dispatch.multi(bytes)
        def default_normalizer(self, o):
            return self.types.WLByteArray(o)

    @dispatch.multi(six.string_types)
    def default_normalizer(self, o):
        return self.types.WLString(o)

    @dispatch.multi(dict)
    def default_normalizer(self, o):
        return self.types.WLAssociation(
            (self.normalize(key), self.normalize(value))
            for key, value in o.items()
        )

    @dispatch.multi(six.integer_types)
    def default_normalizer(self, o):
        return self.types.WLInteger(o)

    @dispatch.multi(decimal.Decimal)
    def default_normalizer(self, o):

        if o.is_infinite():
            return self.types.WLFunction(
                self.types.WLSymbol(b"DirectedInfinity"), (
                    self.types.WLInteger(o < 0 and -1 or 1),
                )
            )

        if o.is_nan():
            return self.types.WLSymbol(b"Indeterminate")

        return self.types.WLDecimal(o)

    @dispatch.multi(float)
    def default_normalizer(self, o):

        if math.isinf(o):
            return self.types.WLFunction(
                self.types.WLSymbol(b"DirectedInfinity"), (
                    self.types.WLInteger(o < 0 and -1 or 1),
                )
            )

        if math.isnan(o):
            return self.types.WLSymbol(b"Indeterminate")

        return self.types.WLFloat(o)

    @dispatch.multi(fractions.Fraction)
    def default_normalizer(self, o):
        return self.types.WLRational(o)

    @dispatch.multi(complex)
    def default_normalizer(self, o):
        return self.types.WLComplex(o)

    @dispatch.multi(six.iterable_types)
    def default_normalizer(self, o):
        return self.types.WLList(
            self.normalize(value)
            for value in o
        )

    @dispatch.multi(WLSerializable)
    def default_normalizer(self, o):
        return self.normalize(o.to_wl())

    @dispatch.default()
    def default_normalizer(self, o):
        if hasattr(o, '__iter__'):
            return self.types.WLList(
                self.normalize(value)
                for value in o
            )
        raise NotImplementedError('Cannot serialize object of class %s' % o.__class__)