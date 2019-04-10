# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.serializable import WLSerializable
from wolframclient.serializers.encoder import wolfram_encoder
from wolframclient.utils.importutils import API

__all__ = ['export', 'WLSerializable', 'wolfram_encoder']

DEFAULT_FORMAT = 'wl'

available_formats = API(
    wl='wolframclient.serializers.wl.WLSerializer',
    wxf='wolframclient.serializers.wxf.WXFSerializer',
)


def export(data, stream=None, target_format=DEFAULT_FORMAT, **options):
    """ Serialize input `data` to a target format.

    Input `data` can be any supported Python type, including :class:`list`, :class:`dict` or any serializable Python
    object.
    
    Serializable python objects are class extending :class:`~wolframclient.serializers.serializable.WLSerializable` and
    types declared in an encoder.

    The default format is :wl:`InputForm` string::

        >>> export(wl.Range(3))
        b'Range[3]'

    Specify WXF format by setting `target_format`::

        >>> export([1,2,3], target_format='wxf')
        b'8:f\x03s\x04ListC\x01C\x02C\x03'

    .. note :: WXF is a binary format for serializing Wolfram Language expression. Consult the
        `format specifications <https://reference.wolfram.com/language/tutorial/WXFFormatDescription.html>`_ for in
        depth format description.

    WXF byte arrays are deserialized with :func:`~wolframclient.deserializers.binary_deserialize`::

        >>> wxf = export([1,2,3], target_format='wxf')
        >>> binary_deserialize(wxf)
        [1, 2, 3]

    If `stream` is specified with a string, it is interpreted as a file path and the serialized form is written directly
    to the specified file. The file is opened and closed automatically::

        >>> export([1, 2, 3], stream='file.wl')
        'file.wl'

    If `stream` is specified with an output stream, the serialization bytes are written to it.

    Any object that implements a `write` method, e.g. :data:`file`, :class:`io.BytesIO` or :class:`io.StringIO`, is a
    valid value for the `stream` named parameter::

        >>> with open('file.wl', 'wb') as f:
        ...     export([1, 2, 3], stream=f)
        ...
        <open file 'file.wl', mode 'wb' at 0x10a4f01e0>

    """
    if not target_format in available_formats:
        raise ValueError('Invalid export format %s. Choices are: %s' %
                         (target_format, ', '.join(available_formats.keys())))
    return available_formats[target_format](**options).export(
        data, stream=stream)
