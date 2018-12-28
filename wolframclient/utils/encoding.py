# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.functional import map


def force_text(s, encoding='utf-8', errors='strict'):
    """
    Similar to smart_text, except that lazy instances are resolved to
    strings, rather than kept as lazy objects.

    If strings_only is True, don't convert (some) non-string-like objects.
    """
    # Handle the common case first for performance reasons.
    if isinstance(s, six.text_type):
        return s
    if not isinstance(s, six.string_types):
        if six.PY3:
            if isinstance(s, bytes):
                s = six.text_type(s, encoding, errors)
            else:
                s = six.text_type(s)
        elif hasattr(s, '__unicode__'):
            s = six.text_type(s)
        else:
            s = six.text_type(bytes(s), encoding, errors)
    else:
        # Note: We use .decode() here, instead of six.text_type(s, encoding,
        # errors), so that if s is a SafeBytes, it ends up being a
        # SafeText at the end.
        s = s.decode(encoding, errors)
    return s


def force_bytes(s, encoding='utf-8', errors='strict'):
    """
    If strings_only is True, don't convert (some) non-string-like objects.
    """
    # Handle the common case first for performance reasons.
    if isinstance(s, bytes):
        return s

    if isinstance(s, six.buffer_types):
        return bytes(s)

    if not isinstance(s, six.string_types):
        try:
            if six.PY3:
                return six.text_type(s).encode(encoding)
            else:
                return bytes(s)
        except UnicodeEncodeError:
            if isinstance(s, Exception):
                # An Exception subclass containing non-ASCII data that doesn't
                # know how to print itself properly. We shouldn't raise a
                # further exception.
                return b' '.join(
                    force_bytes(arg, encoding, errors=errors) for arg in s)
            return six.text_type(s).encode(encoding, errors)
    else:
        return s.encode(encoding, errors)


def safe_force_text(obj):
    try:
        return force_text(obj, errors='ignore')
    except Exception as e:
        return '<unprintable obj: %s>' % e


#this function is supposed to be the most efficient byte concatenation that can be archived in python
#used by the serializers

#join seems to be the winner
#https://gist.github.com/smcl/7462529818bb77baad32727a9e5ff44c
#https://blog.mclemon.io/python-efficient-string-concatenation-in-python-2016-edition

if six.PY2:

    def concatenate_bytes(iterable):
        return b''.join(map(six.binary_type, iterable))
else:
    concatenate_bytes = b''.join
