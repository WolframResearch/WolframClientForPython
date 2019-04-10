# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys

from wolframclient.language import wl
from wolframclient.utils import six
from wolframclient.utils.api import PIL, numpy
from wolframclient.utils.dispatch import Dispatch

encoder = Dispatch()
""" Serialize a given :class:`PIL.Image` into a Wolfram Language image.

    This method first tries to extract the data and relevant information about the image,
    and reconstruct it using :wl:`Image` constructor. This is the most efficient way to
    proceed, but is not guaranteed to work all the time. Only some pixel representations
    are supported, fortunatelly the most common ones.

    When the internal PIL representation does not correspond to one of the Wolfram Language,
    the image is converted to its format, if specified, or ultimately to PNG. This may fail,
    in which case an exception is raised and there is nothing more we can do.

    In theory we could represent any image, but due to :func:`~PIL.Image.convert()` behavior 
    we can't. This function is not converting, but naively casts to a given type without rescaling.
    e.g. int32 values above 255 converted to bytes become 255. We can't cast properly from 'I' mode
    since some format are using 'I' for say 'I;16' (int16) and the rawmode is not always accessible.
    
    See bug in convert: https://github.com/python-pillow/Pillow/issues/3159
"""

MODE_MAPPING = {
    # mode  : (type, colorspace, interleaving)
    "1": ("Bit", None, True),
    "L": ("Byte", "Grayscale", True),
    "RGB": ("Byte", "RGB", True),
    "CMYK": ("Byte", "CMYK", True),
    "LAB": ("Byte", "LAB", True),
    "F": ("Real32", None, True),
    "RGBA": ("Byte", "RGB", True),
    "HSV": ("Byte", "HSB", True)
}
SYS_IS_LE = sys.byteorder == 'little'


def normalize_array(array):
    if array.dtype == numpy.dtype('bool'):
        array = array.astype('<u1')
    return array

@encoder.dispatch(PIL.Image)
def encode_image(serializer, img):
    # some PIL mode are directly mapped to WL ones. Best case fast (de)serialization.
    try:
        if img.mode in MODE_MAPPING:
            wl_data_type, colorspace, interleaving = MODE_MAPPING[img.mode]
            return serializer.encode(
                wl.Image(
                    normalize_array(numpy.array(img)),
                    wl_data_type,
                    ColorSpace=colorspace or wl.Automatic,
                    Interleaving=interleaving))
    except ImportError:
        pass
    # try to use format and import/export, may fail during save() and raise exception.
    stream = six.BytesIO()
    img_format = img.format or "PNG"
    try:
        img.save(stream, format=img_format)
    except KeyError:
        raise NotImplementedError('Format %s is not supported.' % img_format)
    return serializer.serialize_function(
        serializer.serialize_symbol(b'ImportByteArray'),
        (serializer.serialize_bytes(stream.getvalue(), as_byte_array = True),
         serializer.serialize_string(img_format)))
