# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

__all__ = ['WLSerializable']

class WLSerializable:

    def to_wl(self):
        raise NotImplementedError('class %s must implement a to_wl method' % self.__class__.__name__)