# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import datetime

from wolframclient.utils.dispatch import Dispatch

encoder = Dispatch()


@encoder.dispatch(datetime.datetime)
def encode_datetime(serializer, o):
    return serializer.serialize_function(
        serializer.serialize_symbol(b"DateObject"),
        (serializer.serialize_iterable(
            (serializer.serialize_int(o.year), serializer.serialize_int(
                o.month), serializer.serialize_int(o.day),
             serializer.serialize_int(o.hour),
             serializer.serialize_int(o.minute),
             serializer.serialize_float(o.second + o.microsecond / 1000000.))),
         serializer.serialize_string("Instant"),
         serializer.serialize_string("Gregorian"),
         serializer.serialize_tzinfo(o.tzinfo, o)))


@encoder.dispatch(datetime.tzinfo)
def encode_tzinfo(serializer, o):
    return serializer.serialize_tzinfo(o)


@encoder.dispatch(datetime.timedelta)
def encode_timedelta(serializer, o):
    return serializer.serialize_function(
        serializer.serialize_symbol(b"Quantity"), (
            serializer.serialize_float(o.total_seconds()),
            serializer.serialize_string("Seconds"),
        ))


@encoder.dispatch(datetime.date)
def encode_date(serializer, o):
    return serializer.serialize_function(
        serializer.serialize_symbol(b"DateObject"),
        (serializer.serialize_iterable((
            serializer.serialize_int(o.year),
            serializer.serialize_int(o.month),
            serializer.serialize_int(o.day),
        )), ))


@encoder.dispatch(datetime.time)
def encode_time(serializer, o):

    inner = [
        serializer.serialize_iterable(
            (serializer.serialize_int(o.hour),
             serializer.serialize_int(o.minute),
             serializer.serialize_float(o.second + o.microsecond / 1000000.)))
    ]

    if o.tzinfo:
        inner.append(
            serializer.serialize_rule(
                serializer.serialize_symbol(b"TimeZone"),
                serializer.serialize_tzinfo(o.tzinfo, o, name_match=None)))

    return serializer.serialize_function(
        serializer.serialize_symbol(b"TimeObject"), inner)
