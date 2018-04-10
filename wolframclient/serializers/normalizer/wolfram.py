# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.serializable import WLSerializable

def update_dispatch(dispatch):

    @dispatch.multi(WLSymbol)
    def normalizer(self, o):
        return self.serialize_symbol(o.name)

    @dispatch.multi(WLFunction)
    def normalizer(self, o):
        return self.serialize_function(
            self.normalize(o.head),
            tuple(self.normalize(arg) for arg in o.args)
        )

    @dispatch.multi(WLSerializable)
    def normalizer(self, o):
        return self.normalize(o.to_wl())