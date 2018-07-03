# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.six import BytesIO
from wolframclient.utils.encoding import force_bytes
from wolframclient.utils.api import zlib

class ZipCompressedWriter(object):

    __slots__ = '_compressor', '_writer'

    def __init__(self, writer):
        """ Write zip compressed data to a given buffer writer. """
        self._compressor = zlib.compressobj()
        self._writer = writer

    def flush(self):
        """ Must be called when closing or destroying the object."""
        self._writer.write(self._compressor.flush())

    def write(self, data):
        """ Write the compression of `data` to the underlying buffer writer. """
        b = force_bytes(data)
        self._writer.write(self._compressor.compress(b))

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.flush()


CHUNK_SIZE = 8192

class ZipCompressedRead(object):
    
    __slots__ = '_compressor', '_reader', '_buffer', '_index'

    def __init__(self, reader):
        """ Write zip compressed data to a given buffer writer. """
        self._compressor = zlib.decompressobj()
        self._reader = reader

    def read(self, size=-1, exact_size=False):
        """Read from a compressed stream of bytes and return the inflated byte sequence.

        Parameter `size` specifies the amount of byte returned at best. 
        
        Parameter `exact_size` indicates that number of bytes in `size` must be exactly returned.

        If `size` is set to -1 then the reader is read entirely.
        """
        if size < 0:
            expected_out_size = 0
            chunk_size = -1
            # True would be inconsistent.
            exact_size = False
        else:
            expected_out_size = size
            chunk_size = CHUNK_SIZE
        out_data = BytesIO()
        out_len = 0
        while True:
            # sometimes some bytes are left over. We have to send them first to zlib.
            if self._compressor.unconsumed_tail != b'':
                data_in = self._compressor.unconsumed_tail
            else:
                # read more data from input reader.
                data_in = self._reader.read(expected_out_size - out_len)
                if data_in == b'':
                    if exact_size:
                        raise EOFError('Not enough data to decompress.')
                    else:
                        break
            # uncompress a new chunk
            chunk = self._compressor.decompress(data_in, expected_out_size)
            # increment output len.
            out_len = out_len + len(chunk)
            # write to buffer
            out_data.write(chunk)
            # check requested size against output length.
            if out_len == expected_out_size:
                break
        return out_data.getvalue()
