# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLFunction, WLSymbol, WLInputExpression
from wolframclient.serializers.normalizer.builtin import safe_key
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.serializers.utils import safe_len
from wolframclient.utils.datastructures import Association


def update_dispatch(dispatch):
    @dispatch.multi(WLSymbol)
    def normalizer(self, o):
        return self.serialize_symbol(o.name)

    @dispatch.multi(WLFunction)
    def normalizer(self, o):
        return self.serialize_function(
            self.normalize(o.head), tuple(
                self.normalize(arg) for arg in o.args))

    @dispatch.multi(WLInputExpression)
    def normalizer(self, o):
        return self.serialize_input_form(o.input)

    @dispatch.multi(WLSerializable)
    def normalizer(self, o):
        return self.normalize(o.to_wl())

    @dispatch.multi(Association)
    def normalizer(self, o):
        return self.serialize_association(
            ((self.normalize(safe_key(key)), self.normalize(value))
             for key, value in o.items()),
            length=safe_len(o))
