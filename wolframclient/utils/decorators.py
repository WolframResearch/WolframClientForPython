# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.datastructures import Association
from wolframclient.utils.functional import composition

def decorate(*func):
    comp = composition(*func)
    def multipass(fn):
        def caller(*args, **opts):
            return comp(fn(*args, **opts))
        return caller
    return multipass

to_tuple = decorate(tuple)
to_dict  = decorate(Association)