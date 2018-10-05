# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import fractions


def update_dispatch(dispatch):
    @dispatch.multi(fractions.Fraction)
    def normalizer(self, o):
        return self.serialize_fraction(o)
