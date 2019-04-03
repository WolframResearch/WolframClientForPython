# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import datetime
import decimal
import platform
import sys
import types
from itertools import chain

#stripped version of SIX

PY2 = sys.version_info[0] == 2
PY3 = sys.version_info[0] == 3
PY_35 = sys.version_info >= (3, 5)
PY_36 = sys.version_info >= (3, 6)
PY_37 = sys.version_info >= (3, 7)

WINDOWS = platform.system() == 'Windows'
LINUX = platform.system() == 'Linux'
MACOS = platform.system() == 'Darwin'

JYTHON = sys.platform.startswith('java')

if PY3:
    string_types = str,
    integer_types = int,
    class_types = type,
    text_type = str
    binary_type = bytes
    none_type = type(None)

    import io
    StringIO = io.StringIO
    BytesIO = io.BytesIO

    memoryview = memoryview
    buffer_types = (bytes, bytearray, memoryview)

else:
    string_types = basestring,
    integer_types = (int, long)
    class_types = (type, types.ClassType)
    text_type = unicode
    binary_type = str
    none_type = types.NoneType

    import StringIO
    StringIO = BytesIO = StringIO.StringIO

    # memoryview and buffer are not strictly equivalent, but should be fine for
    # django core usage (mainly BinaryField). However, Jython doesn't support
    # buffer (see http://bugs.jython.org/issue1521), so we have to be careful.
    if JYTHON:
        memoryview = memoryview
    else:
        memoryview = buffer
    buffer_types = (bytearray, memoryview, buffer)

iterable_types = (list, tuple, set, frozenset, types.GeneratorType, chain)

protected_types = tuple(
    chain(string_types, integer_types,
          (float, decimal.Decimal, datetime.date, datetime.datetime,
           datetime.time, bool, none_type)))
