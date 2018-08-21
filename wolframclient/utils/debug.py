# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from decimal import Decimal

from functools import wraps

import time

if hasattr(time, 'perf_counter'):
    def timed(function):
        def inner(*args, **opts):
            t = time.perf_counter()
            value = function(*args, **opts)
            return time.perf_counter() - t, value
        return inner
else:
    def timed(function):
        def inner(*args, **opts):
            time.perf_counter()
            t = time.time()
            value = function(*args, **opts)
            return time.time() - t, value
        return inner

def echo(x):
    print(x)
    return x

def print_elapsed_time(viewfunc):
    @wraps(viewfunc)
    def inner(*args, **kw):
        t = time.time()
        res = viewfunc(*args, **kw)
        print("Done %s: %s sec" % (inner.__name__, Decimal(time.time() - t).quantize(Decimal("0.000000"))))
        return res
    return inner
