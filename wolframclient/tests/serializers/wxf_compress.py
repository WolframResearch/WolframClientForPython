# -*- coding: utf-8 -*-

from __future__ import (absolute_import, print_function, unicode_literals,
                        with_statement)

import random
import zlib

from wolframclient.serializers.wxfencoder.streaming import (
    ExactSizeReader, ZipCompressedReader, ZipCompressedWriter)
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase

if six.PY2:

    def _bytes(value):
        return chr(value)
else:

    def _bytes(value):
        return bytes((value, ))


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

    def test_uncompress(self):
        byte_list = [random.randint(0, 255) for i in range(10000)]
        data = six.binary_type(bytearray(byte_list))
        zipped = zlib.compress(data)
        total = len(zipped)
        num_of_chunk = 20
        chunk_size = total // num_of_chunk
        in_buffer = six.BytesIO(zipped)
        reader = ZipCompressedReader(in_buffer)
        buff = six.BytesIO()
        for i in range(num_of_chunk):
            buff.write(reader.read(chunk_size))
            self.assertEqual(buff.getvalue(), data[:(i + 1) * chunk_size])

        buff.write(reader.read())
        self.assertEqual(buff.getvalue(), data)
        self.assertEqual(reader.read(), b'')

    def test_uncompress_exact_len(self):
        byte_list = [random.randint(0, 255) for i in range(10000)]
        data = six.binary_type(bytearray(byte_list))
        zipped = zlib.compress(data)
        total = len(zipped)
        num_of_chunk = 20
        chunk_size = total // num_of_chunk
        in_buffer = six.BytesIO(zipped)
        reader = ExactSizeReader(ZipCompressedReader(in_buffer))
        buff = six.BytesIO()
        for i in range(num_of_chunk):
            buff.write(reader.read(chunk_size))
            self.assertEqual(buff.getvalue(), data[:(i + 1) * chunk_size])

        buff.write(reader.read())
        self.assertEqual(buff.getvalue(), data)

    def test_uncompress_exact_len_err(self):
        data = six.binary_type(bytearray(range(100)))
        zipped = zlib.compress(data)
        total = len(zipped)
        reader = ExactSizeReader(ZipCompressedReader(six.BytesIO(zipped)))
        with self.assertRaises(EOFError):
            reader.read(size=total + 1)
