# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.importutils import API

import datetime
import decimal

pytz = API(
    FixedOffset = 'pytz.FixedOffset',
    timezone    = 'pytz.timezone',
    utc         = 'pytz.utc',
    UnknownTimeZoneError = 'pytz.UnknownTimeZoneError'
)

def new_date(date):
    return datetime.date(
        year        = date.year,
        month       = date.month,
        day         = date.day,
    )

def new_time(time, tzinfo = None):

    return localize(
            datetime.time(
            hour        = time.hour,
            minute      = time.minute,
            second      = time.second,
            microsecond = time.microsecond,
        ),
        tzinfo = tzinfo
    )

def new_datetime(date, time = None, tzinfo = None):
    return localize(
        datetime.datetime(
            year        = date.year,
            month       = date.month,
            day         = date.day,
            hour        = time and time.hour   or 0,
            minute      = time and time.minute or 0,
            second      = time and time.second or 0,
            microsecond = time and time.microsecond or 0,
        ),
        tzinfo = tzinfo
    )

def localize(date, tzinfo = None):
    if tzinfo is not None:
        return to_timezone(tzinfo).localize(date)
    return date

def to_timezone(value):
    if isinstance(value, (six.integer_types, float, decimal.Decimal)):
        return pytz.FixedOffset(value * 60)
    if isinstance(value, datetime.tzinfo):
        return value
    if isinstance(value, six.string_types):
        return pytz.timezone(value)
    if value is None:
        return None
    if isinstance(value, (datetime.datetime, datetime.time)):
        return to_timezone(value.tzinfo)
    raise TypeError('cannot cast timezone offset from %s' % value)