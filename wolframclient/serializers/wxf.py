# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import wxf_types
from wolframclient.serializers.base import FormatSerializer
from wolframclient.serializers.wxfencoder.serializer import WXFExprSerializer

class WXFSerializer(FormatSerializer):

    types = wxf_types

    def __init__(self, normalizer = None, compress = False, enforce = False, **opts):
        super(WXFSerializer, self).__init__(normalizer = normalizer, **opts)
        self.compress = compress
        self.enforce  = enforce

    def dump(self, data, stream):
        serializer = WXFExprSerializer(stream, enforce=self.enforce, compress = self.compress)
        serializer.serialize(self.normalize(data))
        return stream