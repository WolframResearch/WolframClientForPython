###########
API: Guide
###########

.. toctree::
   :maxdepth: 4

The library provides features through the classes and functions listed on this page. There are grouped by functionality and cover topics ranging from basic usage to most advanced features.

Wolfram Language Expression Representation
===========================================


.. autofunction:: wolframclient.language.wl
    :noindex:

.. autofunction:: wolframclient.language.System
    :noindex:

.. autofunction:: wolframclient.language.Global
    :noindex:

.. autofunction:: wolframclient.language.expression.WLSymbolFactory
    :noindex:


Serialization
==============

.. autofunction:: wolframclient.serializers.export
    :noindex:

Formats
--------

*InputForm* is the default format and is the most readable one::

    >>> export([1, 2, 3], target_format = "wl")
    '{1, 2, 3}'

Serialized output can be imported in a kernel using `ToExpression`.

*WXF* is also available as an efficient binary representation of Wolfram Language expressions::

    >>> export([1, 2, 3], target_format = "wxf")
    '8:f\x03s\x04ListC\x01C\x02C\x03'

The *WXF* format supports compression using zlib, the compression is disabled by default::

    >>> export([1, 2, 3], target_format = "wxf", compress = True)
    '8C:x\x9cKc.f\xf1\xc9,.qftfrf\x06\x00\x1b\xf8\x03L'

Serialized output can be imported in a kernel using :wl:`BinaryDeserialize`.

Supported Types
---------------

Built-in Data Types
^^^^^^^^^^^^^^^^^^^

Built-in data structures are all supported: :class:`list`, :class:`set`, :class:`frozenset`, :class:`dict`.

Example::

    >>> export({"list": [1, 2, 3], "set": set([1, 2, 2, 4]), "frozenset": frozenset([1, 2, 2, 4]), "dict": dict(a = 2)})
    '<|"list" -> {1, 2, 3}, "set" -> {1, 2, 4}, "frozenset" -> {1, 2, 4}, "dict" -> <|"a" -> 2|>|>'

Any class that has an `__iter__` method is converted to a Wolfram Language :wl:`List`::

    >>> export((i + 2 for i in range(10)))
    '"{2, 3, 4, 5, 6, 7, 8, 9, 10, 11}"'

Python generators are also serialized as :wl:`List`::

    >>> def gen():
    ...     yield 1
    ...     yield 2
    ... 
    >>> export(gen())
    '{1, 2}'

.. note ::

    Python generators should be used preferably when serializing big data, to avoid running out of memory.

To preserve ordering in associations use :class:`collections.OrderedDict`::

    >>> from collections import OrderedDict
    >>> export(OrderedDict([(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')]))
    '<|0 -> "a", 1 -> "b", 2 -> "c", 3 -> "d"|>'

Numeric Types
^^^^^^^^^^^^^

Numeric types are natively supported: :class:`int`, :class:`float`, :class:`complex` and :class:`~decimal.Decimal` serializes to
their Wolfram Language counterpart::

    >>> export({'int': 1, 'float':2.3, 'decimal': decimal.Decimal(1), 'complex': complex(3, 4)})
    '<|"int" -> 1, "float" -> 2.300000, "decimal" -> 1, "complex" -> Complex[3.000000, 4.000000]|>'

IEEE exceptions `infinity` and `NaN` are converted respectively to :wl:`DirectedInfinity` and :wl:`Indeterminate`::

    >>> export({'+inf': float('inf'), '-inf': - float('inf'), 'nan': float('NaN')})
    '<|"+inf" -> DirectedInfinity[1], "-inf" -> DirectedInfinity[-1], "nan" -> Indeterminate|>'

:class:`~fractions.Fraction` serializes to :wl:`Rational`::

    >>> export(fractions.Fraction(2, 3))
    '(2 / 3)'

DateObject Serialization
^^^^^^^^^^^^^^^^^^^^^^^^

:mod:`datetime`, :func:`~datetime.time` and :func:`~datetime.date` are serialized to :wl:`DateObject` and all assume that the timezome to use is the current one at evaluation time::

    >>> import datetime
    >>> now = datetime.datetime.now()
    >>> export([now.time(), now.date(), now])
    '{TimeObject[{16, 1, 19.993822}, TimeZone -> $TimeZone], DateObject[{2018, 3, 16}], DateObject[{2018, 3, 16, 16, 1, 19.993822}, "Instant", "Gregorian", $TimeZone]}'

Specify a timezone in Python using :func:`pytz.timezone` and serialize the date to a :wl:`DateObject`::

    >>> from pytz import timezone
    >>> export(timezone('US/Eastern').localize(datetime.datetime.now()))
    'DateObject[{2018, 3, 16, 16, 4, 17.712409}, "Instant", "Gregorian", "US/Eastern"]'

.. _extensible-serialization:

Extensible mechanism
--------------------

The :mod:`~wolframclient.serializers` module provides mechanisms to extend built-in core functions and to define custom class serializations. One way is to extend :class:`~wolframclient.serializers.serializable.WLSerializable` and override :func:`~wolframclient.serializers.serializable.WLSerializable.to_wl`. An other is to define a normalizer function.

Serializable classes
^^^^^^^^^^^^^^^^^^^^

.. autoclass:: wolframclient.serializers.serializable.WLSerializable
    :noindex:
    :members:


Normalizer
^^^^^^^^^^

The serialization process is built on top of a chain of normalizers. A normalizer is a function that takes one argument and return one python object. Each normalizer can either return a new object or pass the input if they can't deal with its type. Built-in normalizers are applied first and when one consumes the input and returns an other object the chain is applied from the beginning on the new object. In :class:`~wolframclient.serializers.serializable.WLSerializable` we build a new class `MyPythonClass` extending `WLSerializable` that was serialized to Wolfram Language ``MyWolframFunction[...]``. Let's take a new approach based on normalizer to achieve the same result.

The class can be defined as follow::

    class MyPythonClass(object):
        def __init__(self, *arguments):
            self.arguments = arguments

Define a normalizer function::

    def normalizer(o):
        if isinstance(o, MyPythonClass):
            return wl.MyWolframFunction(*o.arguments)
        # don't forget to return the input if we can't deal with the type.
        return o

Serialize the function:

    >>> export(MyPythonClass(1,2), normalizer=normalizer)
    b'MyWolframFunction[1, 2]'

Deserialization
==================

.. autofunction:: wolframclient.deserializers.binary_deserialize
    :noindex:


.. _extensible-deserialization:

Extensible mechanism
--------------------

The :mod:`~wolframclient.deserializers` module provides a simple mechanism to extend the built-in implementation and to define custom deserializations. This is done by subclassing :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer` and override the relevant `consume_*` methods.

.. autoclass:: wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer
    :noindex:
    :members:

.. autoclass:: wolframclient.deserializers.wxf.wxfconsumer.WXFConsumerNumpy
    :noindex:
    :members:

Kernel evaluation
==================

.. autoclass:: wolframclient.evaluation.WolframLanguageSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframLanguageAsyncSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframLanguageFutureSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframKernelPool
    :noindex:
    :members:

.. autofunction:: wolframclient.evaluation.parallel_evaluate
    :noindex:

.. autoclass:: wolframclient.evaluation.WolframResult
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframEvaluationJSONResponse
    :noindex:
    :members:

API call
========

.. autoclass:: wolframclient.evaluation.WolframCloudSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframAPICall
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframAPIResponse
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframCloudSessionAsync
    :noindex:
    :members:


Exceptions
==========

.. autoclass:: wolframclient.exception.WolframLanguageException
    :noindex:
    :members:

.. autoclass:: wolframclient.exception.AuthenticationException
    :noindex:
    :members:

.. autoclass:: wolframclient.exception.WolframEvaluationException
    :noindex:
    :members:

.. autoclass:: wolframclient.exception.RequestException
    :noindex:
    :members:

.. autoclass:: wolframclient.exception.SocketException
    :noindex:
    :members:

