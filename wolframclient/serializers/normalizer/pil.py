# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from PIL import Image

from wolframclient.language.expression import wl
from wolframclient.utils import six

def update_dispatch(dispatch):
    
    @dispatch.multi(Image.Image)
    def normalizer(self, im):
        stream = six.BytesIO()
        im.save(stream, format = 'png')
        return wl.ImportByteArray(stream.getvalue(), 'PNG')