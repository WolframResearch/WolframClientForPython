# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import math

from wolframclient.language.expression import (WLFunction, WLInputExpression,
                                               WLSymbol)
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.serializers.utils import safe_len
from wolframclient.utils import six
from wolframclient.utils.datastructures import Association
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.encoding import force_bytes
from wolframclient.utils.functional import map

encoder = Dispatch()

def _to_key_value(func, serializer, o):
    return func(((serializer.encode(key), serializer.encode(value))
                 for key, value in o.items()),
                length=safe_len(o))


@encoder.dispatch(bool)
def encode_booleans(serializer, o):
    return serializer.serialize_symbol(force_bytes(o))


@encoder.dispatch(six.none_type)
def encode_none(serializer, o):
    """ None in Python *'is frequently used to represent the absence of a value'*
    https://docs.python.org/3/library/constants.html#None

    :wlcode:`Null` in the Wolfram Language *'is a symbol used to indicate the absence of an expression or a result'*,
    whereas :wlcode:`None` *'is a setting used for certain options.'*.
    Ref: http://reference.wolfram.com/language/ref/Null.html and http://reference.wolfram.com/language/ref/None.html

    Python :data:`None` is similar to :wlcode:`Null` when it means absence of value, like in::

        def foo():
            pass

        foo() # None

    .. code-block :: wl

        foo[] := (nothing;)
        foo[] // InputForm  (* Null *)

    Both are usually suppressed similarly, :data:`None` and :wlcode:`Null` are not displayed, whereas `[None]` and
    :wlcode:`{Null}` are.

    On the other hand Python :data:`None` is similar to Wolfram Language :wlcode:`None` when it comes to option values,
    like in::

        def my_function(*args, option: None):
            if option is None:
                # ....

    Obviously the mapping of :data:`None` is not straight forward. The options are:

        1. map :wlcode:`Null` and :wlcode:`None` to Python :data:`None`.
        2. map :wlcode:`Null` to Python :data:`None`.
        3. map :wlcode:`None` to Python :data:`None`.

    1. One symbol no more round trips. In the Wolfram Language :wlcode:`None` has a specific meaning and is used and
    tested explicitly, so it has to be :wlcode:`Null` that no more round trips. :wlcode:`Null` is never really used as a
    specific value, but more as a specific output meaning no output. From Python, testing if a WL function has returned
    something is consistent with Python functions. From the Wolfram Language, Python functions not returning will return
    :wlcode:`None` and therefore their output inconsistent. When the result is part of a bigger expression, checking for
    one value (:wlcode:`Null`) or the other (:wlcode:`None`) is roughly equivalent, especially
    as long as the library returns the same symbol.

    2. Python :data:`None` is mapped to :wlcode:`Null` and :wlcode:`None` to :data:`WLSymbol('None')`. It create a UX
    issue, in the sense that :data:`wl.None` is not available because :data:`None` is reserved keyword. Testing if
    functions have returned is consistent and straight forward in both languages. We lose the opportunity to
    preserve the name *None*, which can cause confusion. :wlcode:`Null` can easily be represented in Python even though
    that's certainly less useful than a straight forward representation of :wlcode:`None` with :data:`None`.

    3. Functions that do not return in Python, now return :wlcode:`None` symbol. The result is not suppressed
    consistently in both languages. Similarly, on the Python side, a Wolfram Language function result must be compared
    to `wl.Null` to check if the function returned anything.

    Having inconsistencies between functions that return nothing is not an small issue, and could lead to all sort of
    hack and work around solutions to prevent them. That's why we prioritize option 2 over the others.
    """
    return serializer.serialize_symbol(b'Null')


if six.PY2:
    @encoder.dispatch(str)
    def encode_bytes(serializer, o):
        return serializer.serialize_bytes(o)


    @encoder.dispatch((bytearray, six.buffer_types))
    def encode_bytes(serializer, o):
        return serializer.serialize_bytes(o, as_byte_array=True)
else:
    @encoder.dispatch((six.binary_type, bytearray, six.buffer_types))
    def encode_bytes(serializer, o):
        return serializer.serialize_bytes(o)


@encoder.dispatch(six.text_type)
def encode_text(serializer, o):
    return serializer.serialize_string(o)


@encoder.dispatch(dict)
def encode_dict(serializer, o):
    return _to_key_value(serializer.serialize_mapping, serializer, o)


@encoder.dispatch(six.integer_types)
def encode_int(serializer, o):
    return serializer.serialize_int(o)


@encoder.dispatch(float)
def encode_float(serializer, o):

    if math.isinf(o):
        return serializer.serialize_function(
            serializer.serialize_symbol(b"DirectedInfinity"),
            (serializer.serialize_int(o < 0 and -1 or 1), ))

    if math.isnan(o):
        return serializer.serialize_symbol(b"Indeterminate")

    return serializer.serialize_float(o)


@encoder.dispatch(complex)
def encode_complex(serializer, o):
    return serializer.serialize_complex(o)


@encoder.dispatch(six.iterable_types)
def encode_iter(serializer, o):
    return serializer.serialize_iterable(
        map(serializer.encode, o), length=safe_len(o))


#STARTING WL CONSTRUCTS


@encoder.dispatch(WLSymbol)
def encode_symbol(serializer, o):
    return serializer.serialize_symbol(o.name)


@encoder.dispatch(WLFunction)
def encode_function(serializer, o):
    return serializer.serialize_function(
        serializer.encode(o.head),
        map(serializer.encode, o.args),
        length=len(o.args))


@encoder.dispatch(WLInputExpression)
def encode_inputexpr(serializer, o):
    return serializer.serialize_input_form(o.input)


@encoder.dispatch(WLSerializable)
def encode_serializable(serializer, o):
    return serializer.encode(o.to_wl())


@encoder.dispatch(Association)
def encode_association(serializer, o):
    return _to_key_value(serializer.serialize_association, serializer, o)
