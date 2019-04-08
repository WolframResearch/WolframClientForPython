###########
Guide
###########

.. toctree::
   :maxdepth: 4

The Wolfram Client Library is structured in submodules all located in :mod:`wolframclient`:

* :mod:`~wolframclient.evaluation` provides convenient methods to evaluate Wolfram Language expressions directly from Python. There are many ways to evaluate code, including evaluation by a local kernel, direct evaluation by a public or private Wolfram Cloud or calling a deployed API.

* :mod:`~wolframclient.language` provides a Python representation of Wolfram Language symbols and functions.

* :mod:`~wolframclient.serializers` provides serialization methods to various formats such as string :wl:`InputForm` and binary :wl:`WXF` format.

* :mod:`~wolframclient.deserializers` contains a parser for :wl:`WXF`.

* :mod:`~wolframclient.exception` regroups the exceptions and errors that the library may raise.


Expression Representation
===========================================


.. autodata:: wolframclient.language.wl
    :noindex:

.. autodata:: wolframclient.language.wlexpr
    :noindex:

.. autodata:: wolframclient.language.System
    :noindex:

.. autodata:: wolframclient.language.Global
    :noindex:

.. autoclass:: wolframclient.language.expression.WLSymbolFactory
    :noindex:


Serialization
==============

.. autofunction:: wolframclient.serializers.export
    :noindex:

Formats
--------

:wl:`InputForm` is the default format and is the most readable one::

    >>> export([1, 2, 3], target_format = "wl")
    '{1, 2, 3}'

Serialized output can be imported in a kernel using :wl:`ToExpression`.

:wl:`WXF` is also available as an efficient binary representation of Wolfram Language expressions::

    >>> export([1, 2, 3], target_format = "wxf")
    '8:f\x03s\x04ListC\x01C\x02C\x03'

The :wl:`WXF` format supports compression using *zlib*; the compression is disabled by default::

    >>> export([1, 2, 3], target_format = "wxf", compress = True)
    '8C:x\x9cKc.f\xf1\xc9,.qftfrf\x06\x00\x1b\xf8\x03L'

Serialized output can be imported in a kernel using :wl:`BinaryDeserialize`.

Supported Types
---------------

Built-in Data Types
^^^^^^^^^^^^^^^^^^^

Built-in data structures are all supported :class:`list`, :class:`set`, :class:`frozenset` and :class:`dict`.

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

    Python generators should preferably be used when serializing big data to avoid running out of memory.

To preserve ordering in associations use :class:`collections.OrderedDict`::

    >>> from collections import OrderedDict
    >>> export(OrderedDict([(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')]))
    '<|0 -> "a", 1 -> "b", 2 -> "c", 3 -> "d"|>'

Numeric Types
^^^^^^^^^^^^^

Numeric types are natively supported; :class:`int`, :class:`float`, :class:`complex` and :class:`~decimal.Decimal` serialize to
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

:mod:`datetime`, :class:`~datetime.time` and :class:`~datetime.date` are serialized to :wl:`DateObject` and all assume that the time zone to use is the current one at evaluation time::

    >>> import datetime
    >>> now = datetime.datetime.now()
    >>> export([now.time(), now.date(), now])
    '{TimeObject[{16, 1, 19.993822}, TimeZone -> $TimeZone], DateObject[{2018, 3, 16}], DateObject[{2018, 3, 16, 16, 1, 19.993822}, "Instant", "Gregorian", $TimeZone]}'

:class:`~datetime.timedelta` is serialized to :wl:`Quantity`::

    >>> export(datetime.timedelta(seconds = 340))

Specify a time zone in Python using :func:`pytz.timezone` and serialize the date to a :wl:`DateObject`::

    >>> from pytz import timezone
    >>> export(timezone('US/Eastern').localize(datetime.datetime.now()))
    'DateObject[{2018, 3, 16, 16, 4, 17.712409}, "Instant", "Gregorian", "US/Eastern"]'

.. _extensible-serialization:

Extensible Mechanism
--------------------

The :mod:`~wolframclient.serializers` module provides mechanisms to extend built-in core functions and to define custom class serializations. There are three ways to extend serialization:

* Extend :class:`~wolframclient.serializers.serializable.WLSerializable` and override its :meth:`~wolframclient.serializers.serializable.WLSerializable.to_wl` method.
* Call :func:`~wolframclient.serializers.export` with `normalizer` set to a normalizer function. This function will be applied to each object prior to the serialization process.
* Declare a type encoder.

Serializable Classes
^^^^^^^^^^^^^^^^^^^^

.. autoclass:: wolframclient.serializers.serializable.WLSerializable
    :noindex:
    :members:


Normalizer
^^^^^^^^^^

A normalizer is a function that takes one argument and returns one python object. It can either return a new object or pass the input if it can't deal with that type.

Define a class::

    class MyPythonClass(object):
        def __init__(self, *arguments):
            self.arguments = arguments

Define a normalizer function::

    from wolframclient.language import wl
    from wolframclient.serializers import export
    
    def normalizer(o):
        if isinstance(o, MyPythonClass):
            return wl.MyWolframFunction(*o.arguments)
        # don't forget to return the input if we can't deal with the type.
        return o

Serialize an instance of :data:`MyPythonClass` using the normalizer function defined previously::

    >>> export(MyPythonClass(1,2), normalizer=normalizer)
    b'MyWolframFunction[1, 2]'

Encoder
^^^^^^^^

The serialization of a Python object relies on encoder functions. Each encoder is attached to a set of Python types. Encoders are generators of bytes. The library defines encoders for most built-in Python types and for the core components of some popular libraries such as PIL :data:`Image`, NumPy arrays and Pandas :data:`Series`.

.. autodata:: wolframclient.serializers.encoder.wolfram_encoder
    :noindex:

Deserialization
==================

.. autodata:: wolframclient.deserializers.binary_deserialize
    :noindex:


Evaluating Expressions
======================

.. autoclass:: wolframclient.evaluation.WolframLanguageSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframLanguageAsyncSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframEvaluatorPool
    :noindex:
    :members:

.. autofunction:: wolframclient.evaluation.parallel_evaluate
    :noindex:

.. autoclass:: wolframclient.evaluation.WolframResult
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframAPIResponse
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframAPIResponseAsync
    :noindex:
    :members:

Cloud API
=========

.. autoclass:: wolframclient.evaluation.WolframCloudSession
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframAPICall
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframAPIResponse
    :noindex:
    :members:

.. autoclass:: wolframclient.evaluation.WolframCloudAsyncSession
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

