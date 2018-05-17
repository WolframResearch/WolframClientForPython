# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals, with_statement

from wolframclient.serializers.wxfencoder.streaming import ZipCompressedWriter
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase

import random
import zlib

if six.PY3:
    def _bytes(value):
        return bytes((value,))
else:
    def _bytes(value):
        return chr(value)

class TestCase(BaseTestCase):
    def test_compress(self):
        stream = six.BytesIO()
        with ZipCompressedWriter(stream) as z_writer:
            z_writer.write(b'abc')
        zipped = b"x\x9cKLJ\x06\x00\x02M\x01'"
        self.assertSequenceEqual(stream.getvalue(), zipped)

    def test_multi_write_compress(self):
        byte_list = [random.randint(0, 255) for i in range(10000)]
        data = six.binary_type(bytearray(byte_list))
        stream = six.BytesIO()
        with ZipCompressedWriter(stream) as z_writer:
            for i in byte_list:
                z_writer.write(_bytes(i))
        zipped = zlib.compress(data)
        self.assertSequenceEqual(stream.getvalue(), zipped)