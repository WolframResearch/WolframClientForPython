# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.encoding import force_bytes
from wolframclient.utils.importutils import API

zlib = API(compressobj = 'zlib.compressobj')

class ZipCompressedWriter(object):

    __slots__ = '_compressor', '_writer'

    def __init__(self, writer):
        ''' Write zip compressed data to a given buffer writer. '''
        self._compressor = zlib.compressobj()
        self._writer = writer

    def flush(self):
        ''' Must be called when closing or destroying the object.'''
        self._writer.write(self._compressor.flush())

    def write(self, data):
        ''' Write the compression of `data` to the underlying buffer writer. '''
        self._writer.write(self._compressor.compress(force_bytes(data)))

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.flush()