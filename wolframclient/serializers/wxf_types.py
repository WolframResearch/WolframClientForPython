# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain

import wolframclient.serializers.wxfencoder.wxfexpr as wxfexpr

#symbols

def WLSymbol(name):
    yield wxfexpr.WXFExprSymbol(name)

def WLFunction(head, args):
    #args is always a tuple
    return chain(
        (wxfexpr.WXFExprFunction(len(args)), ),
        head,
        chain.from_iterable(args)
    )

#numeric

def WLInteger(number):
    yield wxfexpr.WXFExprInteger(number)

def WLFloat(number):
    yield wxfexpr.WXFExprReal(number)

def WLDecimal(number):
    yield wxfexpr.WXFExprBigReal('{0:f}'.format(number))

def WLRational(number):
    return WLFunction(
        WLSymbol('Rational'), (
            WLInteger(number.numerator),
            WLInteger(number.denominator)
        )
    )

def WLComplex(number):
    return WLFunction(
        WLSymbol('Complex'), (
            WLFloat(number.real),
            WLFloat(number.imag),
        )
    )

#text / bytes

def WLString(string):
    yield wxfexpr.WXFExprString(string)

def WLBytes(bytes):
    yield wxfexpr.WXFExprBinaryString(bytes)

def WLRule(lhs, rhs):
    return WLFunction(WLSymbol('Rule'), (lhs, rhs))

def WLRuleDelayed(lhs, rhs):
    return WLFunction(WLSymbol('RuleDelayed'), (lhs, rhs))

def WLAssociation(keyvalue):
    #the normalizer is always sending an generator key, value
    keyvalue = tuple(keyvalue)
    return chain(
        (wxfexpr.WXFExprAssociation(len(keyvalue)), ),
        chain.from_iterable(
            chain((wxfexpr.WXFExprRule(), ), key, value)
            for key, value in keyvalue
        )
    )

def WLList(iterable):
    #the normalizer is always sending an generator expr
    return WLFunction(
        WLSymbol('List'),
        tuple(iterable)
    )