# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.importutils import API
from wolframclient.language.expression import system, wl
from wolframclient.serializers import export
from wolframclient.tests.utils.base import TestCase as BaseTestCase
from wolframclient.utils import six
from wolframclient.utils.datastructures import Association

import datetime
import decimal
import fractions

pytz = API(
    FixedOffset = 'pytz.FixedOffset',
    timezone    = 'pytz.timezone',
    utc         = 'pytz.utc',
    UnknownTimeZoneError = 'pytz.UnknownTimeZoneError'
)

def test_datetime():
    return datetime.datetime(
        year   = 2000,
        month  = 1,
        day    = 1,
        hour   = 11,
        minute = 15,
        second = 20
    )

#you can run those tests by doing
#> python wolfram.py test wolfram.tests.serialization

class TestCase(BaseTestCase):

    def dumps(self, data, format = 'wl', **opts):
        return export(data, format = format, **opts)

    def compare(self, data, output = None, **opts):

        if isinstance(output, six.binary_type):
            self.assertEqual(
                self.dumps(data, **opts),
                output
            )
        else:
            self.assertEqual(
                self.dumps(data, **opts),
                self.dumps(output, **opts),
            )

    def test_serialization(self):

        self.compare(
            [1, 2, 3],
            b'{1, 2, 3}'
        )
        self.compare(
            Association((('a', 2), ('c', False), ('b', True))),
            b'<|"a" -> 2, "c" -> False, "b" -> True|>'
        )

        self.compare(
            wl.YetAnotherSymbol,
            b'YetAnotherSymbol'
        )
        self.compare(
            wl.Expression(1, 2, 3),
            b'Expression[1, 2, 3]'
        )
        self.compare(
            wl.CurriedExpression(1, 2, 3)(4, 5, 6),
            b'CurriedExpression[1, 2, 3][4, 5, 6]'
        )

        self.compare(
            system.YetAnotherSymbol,
            b"System`YetAnotherSymbol"
        )
        self.compare(
            system.Expression(1, 2, 3),
            b'System`Expression[1, 2, 3]'
        )
        self.compare(
            system.CurriedExpression(1, 2, 3)(4, 5, 6),
            b'System`CurriedExpression[1, 2, 3][4, 5, 6]'
        )

        self.compare(
            system.Expression(1, a = 2),
            b'System`Expression[1, Rule[a, 2]]'
        )

    def test_datetime(self):

        self.compare(
            test_datetime(),
            b'DateObject[{2000, 1, 1, 11, 15, 20.000000}, "Instant", "Gregorian", $TimeZone]'
        )
        self.compare(
            pytz.FixedOffset(60).localize(test_datetime()),
            b'DateObject[{2000, 1, 1, 11, 15, 20.000000}, "Instant", "Gregorian", 1.000000]'
        )
        self.compare(
            pytz.timezone("Europe/Rome").localize(test_datetime()),
            b'DateObject[{2000, 1, 1, 11, 15, 20.000000}, "Instant", "Gregorian", "Europe/Rome"]'
        )

    def test_date(self):

        self.compare(
            test_datetime().date(),
            b'DateObject[{2000, 1, 1}]'
        )

    def test_time(self):

        self.compare(
            test_datetime().time(),
            b'TimeObject[{11, 15, 20.000000}, TimeZone -> $TimeZone]'
        )
        self.compare(
            pytz.timezone("Europe/Rome").localize(test_datetime()).timetz(),
            b'TimeObject[{11, 15, 20.000000}, TimeZone -> 1.000000]'
        )

    def test_encoding(self):

        self.compare("\t",  b'"\\t"')
        self.compare("\n",  b'"\\n"')
        self.compare("a\\", b'"a\\\\"')

    def test_numeric(self):

        self.compare(decimal.Decimal(10**20),          b'100000000000000000000`21')
        self.compare(decimal.Decimal('100'),           b'100`3')
        self.compare(decimal.Decimal('100.00'),        b'100.00`5')
        self.compare(decimal.Decimal('0.010'),         b'0.010`2')
        self.compare(decimal.Decimal('0.1534'),        b'0.1534`4')
        self.compare(decimal.Decimal('0.0000000010'),  b'0.0000000010`2')
        
        self.compare(decimal.Decimal('0'),             b'0``1')
        self.compare(decimal.Decimal('0.0'),           b'0``2')
        self.compare(decimal.Decimal('0.0000000000'),  b'0``11')

        self.compare(fractions.Fraction(1, 2),         wl.Rational(1, 2))

        self.compare(float('0.150000'),          b'0.150000')

        for special, result in (
            [float('inf'),  self.dumps(wl.DirectedInfinity(1))],
            [float('-inf'), self.dumps(wl.DirectedInfinity(-1))],
            [float('nan'),  self.dumps(wl.Indeterminate)],
            ):
            self.compare(special, result)
            self.compare(decimal.Decimal(special), result)