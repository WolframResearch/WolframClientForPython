# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import system, wl
from wolframclient.serializers import available_formats, export
from wolframclient.settings import settings
from wolframclient.tests.utils.base import TestCase as BaseTestCase
from wolframclient.tests.utils.static import test_date, test_datetime, test_time
from wolframclient.utils import six
from wolframclient.utils.datastructures import Association
from wolframclient.utils.functional import identity

import decimal

#you can run those tests by doing
#> python wolfram.py test wolfram.tests.serialization

class TestCase(BaseTestCase):

    def dumps(self, data, format = 'wl', **opts):
        return export(data, format = format, **opts)

    def compare(self, data, output = None, **opts):
        self.assertEqual(
            self.dumps(data, **opts),
            output
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
            test_date(),
            b'DateObject[{2000, 1, 1}]'
        )
        self.compare(
            test_datetime(),
            b'DateObject[{2000, 1, 1, 11, 15, 20.000000}, "Instant", "Gregorian", $TimeZone]'
        )
        self.compare(
            test_datetime(tzinfo = 1),
            b'DateObject[{2000, 1, 1, 11, 15, 20.000000}, "Instant", "Gregorian", 1.000000]'
        )
        self.compare(
            test_datetime(tzinfo = "Europe/Rome"),
            b'DateObject[{2000, 1, 1, 11, 15, 20.000000}, "Instant", "Gregorian", "Europe/Rome"]'
        )
        self.compare(
            test_time(),
            b'TimeObject[{11, 15, 20.000000}, TimeZone -> $TimeZone]'
        )
        self.compare(
            test_time(tzinfo = "UTC"),
            b'TimeObject[{11, 15, 20.000000}, TimeZone -> "UTC"]'
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

    def test_encoding(self):

        self.compare("\t",  b'"\\t"')
        self.compare("\n",  b'"\\n"')
        self.compare("a\\", b'"a\\\\"')

    def test_numeric(self):

        self.compare(decimal.Decimal('0E-10'), b'0.0000000000')
        self.compare(decimal.Decimal('0.15'),  b'0.15')
        self.compare(float('0.150000'),        b'0.150000')

        for special, result in (
            [float('inf'),  self.dumps(wl.DirectedInfinity(1))],
            [float('-inf'), self.dumps(wl.DirectedInfinity(-1))],
            [float('nan'),  self.dumps(wl.Indeterminate)],
            ):
            self.compare(special, result)
            self.compare(decimal.Decimal(special), result)