# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal
import re

from wolframclient.utils.encoding import force_bytes

#code borrowed from json

ESCAPE = re.compile(r'[\x00-\x1f\\"\b\f\n\r\t]')
ESCAPE_DCT = {
    '\\': '\\\\',
    '"': '\\"',
    '\b': '\\b',
    '\f': '\\f',
    '\n': '\\n',
    '\r': '\\r',
    '\t': '\\t',
}
for i in range(0x20):
    ESCAPE_DCT.setdefault(chr(i), '\\u{0:04x}'.format(i))


def replace(match):
    return ESCAPE_DCT[match.group(0)]


def py_encode_text(s):
    yield b'"'
    yield force_bytes(ESCAPE.sub(replace, s))
    yield b'"'


def py_encode_decimal(number):
    return '{0:f}``{1:d}'.format(number,
                                 decimal.getcontext().prec).encode('utf-8')


def safe_len(obj):
    try:
        return len(obj)
    except TypeError:
        return
