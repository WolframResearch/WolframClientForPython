# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import wl_types
from wolframclient.serializers.base import FormatSerializer

class WLSerializer(FormatSerializer):

    types = wl_types

    def __init__(self, normalizer = None, indent = None):
        super(WLSerializer, self).__init__(normalizer = normalizer)
        self.indent = indent

    def dump(self, data, stream):
        for payload in self.normalize(data):
            stream.write(payload)
        return stream