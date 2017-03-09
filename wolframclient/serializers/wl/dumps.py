# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import Expression, wl
from wolframclient.serializers.wl.escape import py_encode_basestring as to_string
from wolframclient.settings import settings
from wolframclient.utils import six
from wolframclient.utils.encoding import force_bytes
from wolframclient.utils.functional import first, identity, last, riffle
from wolframclient.utils.importutils import safe_import_string

import base64
import datetime
import decimal
import fractions
import math
import re

WL_INDETERMINATE = 'Indeterminate'
WL_POS_INFINITY  = 'Infinity'
WL_NEG_INFINITY  = '(-Infinity)'

def _serialize_tzinfo(date, name_match = re.compile('^[A-Za-z]+(/[A-Za-z]+)?$')):

    if date.tzinfo is None:
        return getattr(wl, '$TimeZone')

    name = date.tzinfo.tzname(None)

    if name and name_match.match(name):
        return name

    return date.utcoffset().total_seconds() / 3600

def to_indent(s, indent = 0, indent_level = 0):
    if indent:
        return b"\n%s%s" % (b" " * (indent * indent_level), force_bytes(s))
    return force_bytes(s)

def _serialize(o, indent = 0, indent_level = 0, normalize = identity):

    o = normalize(o)

    if hasattr(o, "to_wl"):
        for sub in _serialize(o.to_wl(), indent, indent_level, normalize = normalize):
            yield sub

    elif isinstance(o, Expression):

        yield to_indent(o.fully_qualified_symbol(), indent, indent_level)

        for curried in o.args:
            yield b'['

            for sub in riffle((_serialize(val, indent, indent_level + 1, normalize = normalize) for val in curried), (b', ', )):
                for inner in sub:
                    yield inner

            yield to_indent(b']', indent, indent_level)

    elif o is None:
        yield to_indent(b'None', indent, indent_level)
    elif o is True:
        yield to_indent(b'True', indent, indent_level)
    elif o is False:
        yield to_indent(b'False', indent, indent_level)

    #TODO we need to handle timezones
    elif isinstance(o, datetime.datetime):
        for sub in _serialize(
                wl.DateObject(
                    (o.year, o.month, o.day, o.hour, o.minute, o.second + o.microsecond / 1000000.),
                    "Instant",
                    "Gregorian",
                    _serialize_tzinfo(o)
                ),
                indent,
                indent_level,
                normalize = normalize
            ):
            yield sub

    elif isinstance(o, datetime.date):
        for sub in _serialize(
                wl.DateObject((o.year, o.month, o.day)),
                indent,
                indent_level,
                normalize = normalize
            ):
            yield sub

    elif isinstance(o, datetime.time):
        for sub in _serialize(
                wl.TimeObject(
                    (o.hour, o.minute, o.second + o.microsecond / 1000000.),
                    TimeZone = _serialize_tzinfo(o)
                ),
                indent,
                indent_level,
                normalize = normalize
            ):
            yield sub

    elif isinstance(o, bytearray) or (six.PY3 and isinstance(o, bytes)):
        for sub in _serialize(
                wl.ByteArray(
                    base64.b64encode(o).decode('utf-8')
                ),
                indent,
                indent_level,
                normalize = normalize
            ):
            yield sub

    elif isinstance(o, constants.string_types):
        yield to_indent(to_string(o), indent, indent_level)

    # elif isinstance(o, (float, decimal.Decimal)):
    #    #TODO: Decimal should be treated differently, to avoid precision loss
    #    yield to_indent(format(o, '.32f'), indent, indent_level)

    elif isinstance(o, six.integer_types):
        yield to_indent('%i' % o, indent, indent_level)

    elif isinstance(o, decimal.Decimal):

        #we need to us o.is_nan because jython math.isnan is broken for decimals

        if o.is_nan():
            yield to_indent(WL_INDETERMINATE, indent, indent_level)
        elif o.is_infinite():
            if o < 0:
                yield to_indent(WL_NEG_INFINITY, indent, indent_level)
            else:
                yield to_indent(WL_POS_INFINITY, indent, indent_level)
        else:
            yield to_indent('{0:f}'.format(o), indent, indent_level)

    elif isinstance(o, float):

        if math.isnan(o):
            yield to_indent(WL_INDETERMINATE, indent, indent_level)
        elif math.isinf(o):
            if o < 0:
                yield to_indent(WL_NEG_INFINITY, indent, indent_level)
            else:
                yield to_indent(WL_POS_INFINITY, indent, indent_level)
        else:
            yield to_indent('{0:f}'.format(o), indent, indent_level)

    elif isinstance(o, (fractions.Fraction)):
        yield to_indent('%i / %i' % (o.numerator, o.denominator), indent, indent_level)

    elif isinstance(o, (complex)):
        yield to_indent('%f + I*%f' % (o.real, o.imag), indent, indent_level)

    elif isinstance(o, dict):

        yield to_indent(b'<|', indent, indent_level)

        for items in riffle(o.items(), b", "):
            if isinstance(items, constants.binary_type):
                yield items
            else:
                for sub in _serialize(first(items), indent, indent_level + 1, normalize = normalize):
                    yield sub

                yield b" -> "

                for i, sub in enumerate(_serialize(last(items), indent, indent_level + 1, normalize = normalize)):
                    if not i:
                        yield sub.lstrip()
                    else:
                        yield sub

        yield to_indent(b'|>', indent, indent_level)

    elif isinstance(o, constants.iterable_types) or hasattr(o, '__iter__'):

        yield to_indent(b'{', indent, indent_level)

        for sub in riffle((_serialize(inner, indent, indent_level + 1, normalize = normalize) for inner in o), (b", ", )):
            for inner in sub:
                yield inner

        yield to_indent(b'}', indent, indent_level)

    else:
        raise NotImplementedError('cannot _serialize object of class %s' % o.__class__)

def _dump_to_stream(data, stream, normalize = None, **opts):

    for chunk in _serialize(
        data,
        normalize = normalize or safe_import_string(settings.NORMALIZATION_FUNCTION),
        **opts
        ):
        stream.write(chunk)
    return stream

def dumps(data, path = None, indent = settings.DEBUG and 4 or 0, **opts):
    if path:
        if isinstance(path, six.string_types):
            with open(path, 'w') as stream:
                _dump_to_stream(data, stream, indent = indent, **opts)
                return path

        _dump_to_stream(data, path, indent = indent, **opts)
        return path

    stream = _dump_to_stream(data, constants.BytesIO(), indent = indent, **opts)
    stream.seek(0)
    return stream.read()
