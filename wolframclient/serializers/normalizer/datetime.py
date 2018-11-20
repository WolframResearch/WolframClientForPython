# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import datetime


def update_dispatch(dispatch):
    @dispatch.multi(datetime.datetime)
    def normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol(b"DateObject"),
            (self.serialize_iterable(
                (self.serialize_int(o.year), self.serialize_int(o.month),
                 self.serialize_int(o.day), self.serialize_int(o.hour),
                 self.serialize_int(o.minute),
                 self.serialize_float(o.second + o.microsecond / 1000000.))),
             self.serialize_string("Instant"),
             self.serialize_string("Gregorian"),
             self.serialize_tzinfo(o.tzinfo, o)))

    @dispatch.multi(datetime.tzinfo)
    def normalizer(self, o):
        return self.serialize_tzinfo(o)

    @dispatch.multi(datetime.timedelta)
    def normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol(b"Quantity"), (
                self.serialize_float(o.total_seconds()),
                self.serialize_string("Seconds"),
            ))

    @dispatch.multi(datetime.date)
    def normalizer(self, o):
        return self.serialize_function(
            self.serialize_symbol(b"DateObject"), (self.serialize_iterable((
                self.serialize_int(o.year),
                self.serialize_int(o.month),
                self.serialize_int(o.day),
            )), ))

    @dispatch.multi(datetime.time)
    def normalizer(self, o):

        inner = [
            self.serialize_iterable(
                (self.serialize_int(o.hour), self.serialize_int(o.minute),
                 self.serialize_float(o.second + o.microsecond / 1000000.)))
        ]

        if o.tzinfo:
            inner.append(
                self.serialize_rule(
                    self.serialize_symbol(b"TimeZone"),
                    self.serialize_tzinfo(o.tzinfo, o, name_match=None)))

        return self.serialize_function(
            self.serialize_symbol(b"TimeObject"), inner)
