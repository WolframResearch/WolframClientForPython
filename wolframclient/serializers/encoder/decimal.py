# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal

from wolframclient.utils.dispatch import Dispatch

encoder = Dispatch()


@encoder.dispatch(decimal.Decimal)
def encode_decimal(serializer, o):

    if o.is_infinite():
        return serializer.serialize_function(
            serializer.serialize_symbol(b"DirectedInfinity"),
            (serializer.serialize_int(o < 0 and -1 or 1), ))

    if o.is_nan():
        return serializer.serialize_symbol(b"Indeterminate")

    return serializer.serialize_decimal(o)
