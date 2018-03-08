# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wxfserializer.utils import six

import re

INT8_MAX = 1 << 7
INT8_MIN = -(1 << 7)
INT16_MAX = 1 << 15
INT16_MIN = -(1 << 15)
INT32_MAX = 1 << 31
INT32_MIN = -(1 << 31)
INT64_MAX = 1 << 63
INT64_MIN = -(1 << 63)

def force_text(s, encoding='utf-8', errors='strict'):
    """
    Similar to smart_text, except that lazy instances are resolved to
    strings, rather than kept as lazy objects.

    If strings_only is True, don't convert (some) non-string-like objects.
    """
    # Handle the common case first for performance reasons.
    if issubclass(type(s), six.text_type):
        return s
    if not issubclass(type(s), six.string_types):
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

class VersionParser(object):
    ''' Parse a string version of the form {{major}}.{{minor}}.* to extract the major and minor values '''

    def __init__(self, version):
        if not re.match(r'\d[.]\d+[.].*', version):
            raise ValueError("Version string is not valid.")
        values = version.split('.')
        self.version = version
        self.major = int(values[0])
        self.minor = int(values[1])