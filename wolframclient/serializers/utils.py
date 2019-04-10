# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal
import re

from wolframclient.utils.encoding import force_bytes

#replacement method borrowed from json

ESCAPE = re.compile(r'[\x00\\"\b\f\n\r\t]')
ESCAPE_DCT = {
    chr(0): '\.00',
    '\\': '\\\\',
    '"': '\\"',
    '\b': '\\b',
    '\f': '\\f',
    '\n': '\\n',
    '\r': '\\r',
    '\t': '\\t',
}


def replace(match):
    return ESCAPE_DCT[match.group(0)]


def py_encode_text(s):
    yield b'"'
    yield force_bytes(ESCAPE.sub(replace, s), encoding="iso-8859-1")
    yield b'"'


def py_encode_decimal(number, prec=decimal.getcontext().prec):
    return '{0:f}``{1:d}'.format(number, prec).encode('utf-8')


def safe_len(obj):
    try:
        return len(obj)
    except TypeError:
        return
