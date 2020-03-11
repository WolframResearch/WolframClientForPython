from __future__ import absolute_import, print_function, unicode_literals

import itertools
import platform
import sys
import types

# stripped version of SIX

PY2 = sys.version_info[0] == 2
PY3 = sys.version_info[0] == 3
PY_35 = sys.version_info >= (3, 5)
PY_36 = sys.version_info >= (3, 6)
PY_37 = sys.version_info >= (3, 7)
PY_38 = sys.version_info >= (3, 8)

WINDOWS = platform.system() == "Windows"
LINUX = platform.system() == "Linux"
MACOS = platform.system() == "Darwin"

JYTHON = sys.platform.startswith("java")

if PY3:
    string_types = (str,)
    integer_types = (int,)
    class_types = (type,)
    text_type = str
    binary_type = bytes
    none_type = type(None)

    import io

    StringIO = io.StringIO
    BytesIO = io.BytesIO

    buffer_types = (bytes, bytearray, memoryview)

else:
    string_types = (basestring,)
    integer_types = (int, long)
    class_types = (type, types.ClassType)
    text_type = unicode
    binary_type = str
    none_type = types.NoneType

    import StringIO

    StringIO = BytesIO = StringIO.StringIO

    buffer_types = (bytearray, memoryview, buffer)

iterable_types = [
    list,
    tuple,
    set,
    frozenset,
    types.GeneratorType,
    itertools.chain,
    itertools.groupby,
]
if not PY2:
    iterable_types.extend((map, range))
