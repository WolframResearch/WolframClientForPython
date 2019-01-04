# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.api import zlib
from wolframclient.utils.encoding import force_bytes


class ZipCompressedWriter(object):
    def __init__(self, writer):
        """ Write zip compressed data to a given buffer writer. """
        self._compressor = zlib.compressobj()
        self._writer = writer

    def flush(self):
        """ Must be called when closing or destroying the object."""
        self._writer.write(self._compressor.flush())

    def write(self, data):
        """ Write the compression of `data` to the underlying buffer writer. """
        self._writer.write(self._compressor.compress(force_bytes(data)))

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
        data = self._reader.read(size)
        # Negative values read until EOF and 0 returns b''. Both remain unchanged.
        # Also a fast path when the requested amount of bytes is returned in one go.
        if size <= 0 or len(data) == size:
            return data
        # need an intermediary buffer
        out_len = len(data)
        data = six.BytesIO(data)
        while out_len < size:
            chunk = self._reader.read(size - out_len)
            if chunk == b'':
                raise EOFError('Not enough data to read.')
            data.write(chunk)
            out_len = out_len + len(chunk)
        return data.getvalue()


class ZipCompressedReader(object):
    """A buffer implementation reading zip compressed data from a source buffer and returning uncompressed data.

    This class is instantiated from a reader, any object implementing a :meth:`~io.BufferedIOBase.read` method.
    """
    CHUNK_SIZE = 8192

    def __init__(self, reader):
        """ Read zip compressed data from a given buffer reader."""
        self._compressor = zlib.decompressobj()
        self._reader = reader

    def read(self, size=-1):
        """Read from a compressed stream of bytes and return the inflated byte sequence.

        Parameter `size` specifies the amount of bytes to return at most. If `size` is set to -1
        then the reader is read until EOF is reached.
        """
        if size is None or size < 0:
            chunk_size = -1
            size = -1
        else:
            chunk_size = ZipCompressedReader.CHUNK_SIZE
        out_data = six.BytesIO()
        out_len = 0
        while True:
            # first step find try to find some data to uncompress.
            # sometimes some bytes are left over. We have to send them first to zlib.
            if self._compressor.unconsumed_tail != b'':
                data_in = self._compressor.unconsumed_tail
            else:
                # read more data from input reader. Read in chunk since we can't guess how
                # big the inflated result is.
                data_in = self._reader.read(chunk_size)
                # no more data is available.
                if data_in == b'':
                    break
            # second step, decompress the new chunk
            if size > 0:
                chunk = self._compressor.decompress(data_in, size - out_len)
            else:
                chunk = self._compressor.decompress(data_in)
            # increment output len.
            out_len = out_len + len(chunk)
            # write to buffer
            out_data.write(chunk)
            # check requested size against output length.
            if size > 0 and out_len == size:
                break
        return out_data.getvalue()
