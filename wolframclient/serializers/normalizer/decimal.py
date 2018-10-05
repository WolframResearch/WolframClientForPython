# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal


def update_dispatch(dispatch):
    @dispatch.multi(decimal.Decimal)
    def normalizer(self, o):

        if o.is_infinite():
            return self.serialize_function(
                self.serialize_symbol(b"DirectedInfinity"),
                (self.serialize_int(o < 0 and -1 or 1), ))

        if o.is_nan():
            return self.serialize_symbol(b"Indeterminate")

        return self.serialize_decimal(o)
