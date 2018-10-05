# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.serializable import WLSerializable
from wolframclient.utils.importutils import API
"""
"""

__all__ = ['export', 'WLSerializable']

DEFAULT_FORMAT = 'wl'

available_formats = API(
    wl='wolframclient.serializers.wl.WLSerializer',
    wxf='wolframclient.serializers.wxf.WXFSerializer',
)


def export(data, stream=None, target_format=DEFAULT_FORMAT, **options):
    """Serialize input `data` to a target format.

    Input `data` can be one of the native Python types supported directly including:
    :class:`list`, :class:`dict`, etc. Or, a serializable python object, i.e extending
    :class:`~wolframclient.serializers.serializable.WLSerializable`.
    The default format is string *InputForm*::

        >>> export(wl.Range(3))
        'Range[3]'

    Specify the target format to be WXF::

        >>> export([1,2,3], target_format='wxf')
        '8:f\x03s\x04ListC\x01C\x02C\x03'

    .. note :: WXF is a binary format for serializing Wolfram Language expression. Consult
            the documentation for a full format description: https://reference.wolfram.com/language/tutorial/WXFFormatDescription.html

    If a string is provided as second argument, it is interpreted as a file path, and serialized form is
    written directly to the file. The file is opened and closed automatically::

        >>> export([1, 2, 3], 'file.wl')
        'file.wl'

    An output stream can also be provided, in which case the output bytes are written to it.

    Finally any object that implements a `write` method, like :class:`file`, :class:`io.BytesIO`
    or :class:`io.StringIO` is a valid value for the `stream` option::

        >>> with open('file.wl', 'wb') as f:
        ...     export([1, 2, 3], f)
        ...
        <open file 'file.wl', mode 'wb' at 0x10a4f01e0>

    """
    if not target_format in available_formats:
        raise ValueError('Invalid export format %s. Choices are: %s' %
                         (target_format, ', '.join(available_formats.keys())))
    return available_formats[target_format](**options).export(
        data, stream=stream)
