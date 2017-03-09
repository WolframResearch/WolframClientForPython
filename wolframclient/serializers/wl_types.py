# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain

from wolframclient.serializers.escape import py_encode_basestring as to_string
from wolframclient.utils.encoding import force_bytes

def yield_with_separators(iterable, separator = b', ', first = None, last = None):
    if first:
        yield first
    for i, arg in enumerate(iterable):
        if i:
            yield separator
        for sub in arg:
            yield sub
    if last:
        yield last

#symbols

def WLSymbol(name):
    yield force_bytes(name)

def WLFunction(head, args):
    return chain(
        head,
        yield_with_separators(args, first = b'[', last = b']')
    )

#numeric

def WLInteger(number):
    yield ('%i' % number).encode('utf-8')

def WLFloat(number):
    yield ('{0:f}'.format(number)).encode('utf-8')

def WLDecimal(number):
    yield ('{0:f}'.format(number)).encode('utf-8')

def WLRational(number):
    return yield_with_separators(
        (WLInteger(number.numerator), WLInteger(number.denominator)),
        separator = b' / ',
        first = b'(',
        last = b')'
    )

def WLComplex(number):
    return yield_with_separators(
        (WLFloat(number.real), WLFloat(number.imag)),
        separator = b' + I*',
        first = b'(',
        last = b')'
    )

#text / bytes

def WLString(string):
    return to_string(string)

def WLBytes(bytes):
    return WLFunction(
        WLSymbol('ByteArray'), (
            WLString(base64.b64encode(bytes).decode('utf-8')),
        )
    )

def WLRule(lhs, rhs):
    return yield_with_separators(
        (lhs, rhs),
        separator = b' -> '
    )

def WLRuleDelayed(lhs, rhs):
    return yield_with_separators(
        (lhs, rhs),
        separator = b' :> '
    )

def WLAssociation(keyvalue):
    return yield_with_separators((
            WLRule(key, value)
            for key, value in keyvalue
        ),
        first = b'<|',
        last  = b'|>'
    )

def WLList(iterable):
    return yield_with_separators(
        iterable,
        first = b'{',
        last  = b'}'
    )