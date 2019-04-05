# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal

import wolframclient.serializers.wxfencoder.wxfexpr as wxfexpr
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.utils import py_encode_decimal
from wolframclient.utils import six


class NotEncodedException(Exception):
    """Exception used during encoding to signal that a given Python object has been ignored by a :class:`~wolframclient.serializers.wxfencoder.WXFEncoder`."""


class WXFEncoder(object):
    """Encode a given Python object into a stream of :class:`~wolframclient.serializers.wxfencoder.wxfexpr.WXFExpr`.

    This class is meant to be subclassed in order to add support for new classes. The encoder does not have to do anything
    since more than one can be attached to a given :class:`~wolframclient.serializers.wxfencoder.wxfexprprovider.WXFExprProvider`.
    The library provides a default encoder that should cover basic needs, more or less json types, and that should be useful in any case.

    To implement a new encoder one needs to sub-class :class:`~wolframclient.serializers.wxfencoder.wxfencoder.WXFEncoder`
    and implements method :func:`~wolframclient.serializers.wxfencoder.wxfencoder.WXFEncoder.encode`. Encode is a generator function
    that takes a given Python object and instances of :class:`~wolframclient.serializers.wxfencoder.wxfexpr.WXFExpr`. If it returns before
    yielding anything a :class:`~wolframclient.serializers.wxfencoder.wxfencoder.NotEncodedException` is raised to signal that
    the encoder is not supporting the given object, and that the encoding must be delegating to the next encoder (if any).

    Sometimes it is useful to start a new serialization using the provider, re-entrant call, especially when dealing with non-atomic
    :class:`~wolframclient.serializers.wxfencoder.wxfexpr.WXFExpr`, such as ``Function`` or ``Association``.
    To do so one must call ``self.serialize`` on the target object and yield the results (yield from in PY3).
    """

    def __init__(self):
        self._provider = None

    def encode(self, o):
        """The method to implement in sub-classes."""
        raise NotImplementedError

    def serialize(self, o):
        """Re-entrant method used to serialize part of a Python object.

        Example: when serializing a custom class ``foo[{'k1'->1,'k2'->2}]``, the user
        defined encoder for class foo could encode it as a function with head 'foo' and a dict.
        The generator would be something similar to::

            yield WXFFunction(3)
            yield WXFSymbol('foo')
            yield from self.serialize({'k1'->1,'k2'->2})

        Using a re-entrant call (line 3) allows the dictionnary to be encoded as a new expr;
        assuming :class:`~wolframclient.serializers.wxfencoder.WXFDefaultEncoder`
        is also registered as a provider, the dict will get encoded as an association.

        It also enables transformation mechanism, say apply list to all iterable object and
        pass the result to the provider.
        """
        for sub in self._provider.provide_wxfexpr(o):
            yield sub

    NOT_PROVIDED = object()

    def _encode(self, o):
        """Is called by the provider."""
        value = WXFEncoder.NOT_PROVIDED
        for value in self.encode(o):
            yield value
        if value is WXFEncoder.NOT_PROVIDED:
            raise NotEncodedException


class DefaultWXFEncoder(WXFEncoder):
    """The most straight forward serialization of python expressions to their Wolfram Language counterpart.

    This class is meant to represent basically JSON like objects, and is intended to be used in all providers.
    As such it should only deal with obvious convertion.

    e.g: :class:`int` to Wolfram Language ``Integer``, but iterators are not supported but user defined implementation
    can be added easily.
    """

    def encode(self, pythonExpr):
        """Encode most common Python types to their Wolfram Language counterpart."""
        if isinstance(pythonExpr, six.binary_type):
            yield wxfexpr.WXFExprBinaryString(pythonExpr)
        elif isinstance(pythonExpr, six.string_types):
            yield wxfexpr.WXFExprString(pythonExpr)
        elif isinstance(pythonExpr, WLSymbol):
            yield wxfexpr.WXFExprSymbol(pythonExpr.name)
        elif isinstance(pythonExpr, WLFunction):
            yield wxfexpr.WXFExprFunction(len(pythonExpr))
            for sub in self.encode(pythonExpr.head):
                yield sub
            for arg in pythonExpr.args:
                for sub in self.encode(arg):
                    yield sub
        elif isinstance(pythonExpr, six.integer_types):
            yield wxfexpr.WXFExprInteger(pythonExpr)
        elif isinstance(pythonExpr, list):
            yield wxfexpr.WXFExprFunction(len(pythonExpr))
            yield wxfexpr.WXFExprSymbol('List')
            for pyArg in iter(pythonExpr):
                for wxf_expr in self.serialize(pyArg):
                    yield wxf_expr
        elif isinstance(pythonExpr, dict):
            yield wxfexpr.WXFExprAssociation(len(pythonExpr))
            for key, value in pythonExpr.items():
                yield wxfexpr.WXFExprRule()
                for wxf_expr in self.serialize(key):
                    yield wxf_expr
                for wxf_expr in self.serialize(value):
                    yield wxf_expr
        elif pythonExpr is True:
            yield wxfexpr.WXFExprSymbol('True')
        elif pythonExpr is False:
            yield wxfexpr.WXFExprSymbol('False')
        elif pythonExpr is None:
            yield wxfexpr.WXFExprSymbol('None')
        elif isinstance(pythonExpr, float):
            yield wxfexpr.WXFExprReal(pythonExpr)
        elif isinstance(pythonExpr, complex):
            yield wxfexpr.WXFExprFunction(2)
            yield wxfexpr.WXFExprSymbol('Complex')
            yield wxfexpr.WXFExprReal(pythonExpr.real)
            yield wxfexpr.WXFExprReal(pythonExpr.imag)
        elif isinstance(pythonExpr, wxfexpr.WXFExpr):
            yield pythonExpr
        elif isinstance(pythonExpr, decimal.Decimal):
            yield wxfexpr.WXFExprBigReal(py_encode_decimal(pythonExpr))
