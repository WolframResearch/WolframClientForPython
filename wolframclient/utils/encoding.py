from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import map

force_text = Dispatch()


@force_text.dispatch(six.buffer_types)
def encode(s, *args, **opts):
    return force_text(force_bytes(s, *args, **opts), *args, **opts)


@force_text.dispatch(six.text_type)
def encode(s, encoding="utf-8", errors="strict"):
    return s


@force_text.dispatch(six.binary_type, replace_existing=True)
def encode(s, encoding="utf-8", errors="strict"):
    return s.decode(encoding, errors)


if not six.PY2:

    @force_text.dispatch(object)
    def encode(s, encoding="utf-8", errors="strict"):
        return six.text_type(s)


else:

    @force_text.dispatch(object)
    def encode(s, encoding="utf-8", errors="strict"):
        if hasattr(s, "__unicode__"):
            return six.text_type(s)
        else:
            return six.text_type(bytes(s), encoding, errors)


force_bytes = Dispatch()


@force_bytes.dispatch(six.string_types)
def encode(s, encoding="utf-8", errors="strict"):
    return s.encode(encoding, errors)


@force_bytes.dispatch(six.binary_type, replace_existing=True)
def encode(s, encoding="utf-8", errors="strict"):
    return s


@force_bytes.dispatch(six.buffer_types, replace_existing=True)
def encode(s, encoding="utf-8", errors="strict"):
    return six.binary_type(s)


if six.PY2:

    @force_bytes.dispatch(memoryview, replace_existing=True)
    def encode(s, encoding="utf-8", errors="strict"):
        return s.tobytes()


if not six.PY2:

    def encode_default(s, encoding="utf-8", errors="strict"):
        return six.text_type(s).encode(encoding)


else:

    def encode_default(s, encoding="utf-8", errors="strict"):
        return six.binary_type(s)


@force_bytes.dispatch(object)
def encode(s, encoding="utf-8", errors="strict"):
    try:
        return encode_default(s, encoding, errors)
    except UnicodeEncodeError:
        if isinstance(s, Exception):
            # An Exception subclass containing non-ASCII data that doesn't
            # know how to print itself properly. We shouldn't raise a
            # further exception.
            return b" ".join(force_bytes(arg, encoding, errors=errors) for arg in s)
        return six.text_type(s).encode(encoding, errors)


def safe_force_text(obj):
    try:
        return force_text(obj, errors="ignore")
    except Exception as e:
        return "<unprintable obj: %s>" % e


# this function is supposed to be the most efficient byte concatenation that can be archived in python
# used by the serializers

# join seems to be the winner
# https://gist.github.com/smcl/7462529818bb77baad32727a9e5ff44c
# https://blog.mclemon.io/python-efficient-string-concatenation-in-python-2016-edition

if six.PY2:

    def concatenate_bytes(iterable):
        return b"".join(map(six.binary_type, iterable))


else:
    concatenate_bytes = b"".join
