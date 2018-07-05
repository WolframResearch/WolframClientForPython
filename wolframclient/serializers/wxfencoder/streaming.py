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

class ExactSizeReader(object):
    """ Read exactly the amount of bytes requested and fails otherwise."""
    def __init__(self, reader):
        self._reader = reader

    def read(self, size=-1):
        """Read from an underlying readable object.
        
        If size is negative the data is read until EOF.
        If it is 0 then b'' is returned.
        Otherwise the exact amount of bytes is read from the source. More than
        one call may be necessary.
        """
        # negative value read until EOF and 0 returns b''. Both remain unchanged.
        if size <= 0:
            return self._reader.read(size)
        data = self._reader.read(size)
        # fast path, most of the time the request amount of bytes is returned.
        if len(data) == size:
            return data
        # need an intermediary buffer
        data = BytesIO(data)
        out_len = len(data)
        while out_len < size:
            chunk = self._reader.read(size - out_len)
            if chunk == b'':
                raise EOFError('Not enough data to read.')
            data.write(chunk)
            out_len = out_len + len(chunk)
        return data.getvalue()

class ZipCompressedReader(object):
    __slots__ = '_compressor', '_reader', '_buffer', '_index'

    CHUNK_SIZE = 8192

    def __init__(self, reader):
        """ Write zip compressed data to a given buffer writer."""
        self._compressor = zlib.decompressobj()
        self._reader = reader

    def read(self, size=-1):
        """Read from a compressed stream of bytes and return the inflated byte sequence.

        Parameter `size` specifies the amount of bytes to return. If `size` is set to -1 
        then the reader is read entirely, otherwise exact number of bytes is returned.

        .. note :: 
            Contrary to some python buffer implementation this method raises an exception when there is not enough data available.
        """
        if size < 0:
            expected_out_size = 0
            chunk_size = -1
            # True would be inconsistent.
            exact_size = False
        else:
            expected_out_size = size
            chunk_size = ZipCompressedReader.CHUNK_SIZE
        out_data = BytesIO()
        out_len = 0
        while True:
            # sometimes some bytes are left over. We have to send them first to zlib.
            if self._compressor.unconsumed_tail != b'':
                data_in = self._compressor.unconsumed_tail
            else:
                # read more data from input reader. Read in chunk since we can't guess how
                # big the inflated result is.
                data_in = self._reader.read(chunk_size)
                if data_in == b'':
                    raise EOFError('Not enough data to decompress.')
            # decompress a new chunk
            chunk = self._compressor.decompress(
                data_in, expected_out_size - out_len)
            # increment output len.
            out_len = out_len + len(chunk)
            # write to buffer
            out_data.write(chunk)
            # check requested size against output length.
            if out_len == expected_out_size:
                break
        return out_data.getvalue()
