# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
import PIL.Image


def update_dispatch(dispatch):
    @dispatch.multi(PIL.Image.Image)
    def normalizer(self, im):
        stream = six.BytesIO()
        im.save(stream, "PNG")

        return self.serialize_function(
            self.serialize_symbol(b'ImportByteArray'),
            (self.serialize_bytes(stream.getvalue()),
             self.serialize_string('PNG')))
