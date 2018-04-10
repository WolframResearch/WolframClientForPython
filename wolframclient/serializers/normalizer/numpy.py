# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import numpy

def update_dispatch(dispatch):
    
    @dispatch.multi(numpy.ndarray)
    def normalizer(self, o):
        return self.serialize_string("NDARRAY")