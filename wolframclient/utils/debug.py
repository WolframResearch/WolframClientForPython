# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from decimal import Decimal
from functools import wraps

from wolframclient.utils.api import time


def timed(function):
    def inner(*args, **opts):
        t = time.perf_counter()
        value = function(*args, **opts)
        return time.perf_counter() - t, value

    return inner


def echo(x):
    print(x)
    return x


def timed_repeated(N=30):
    def repeated(function):
        def inner(*args, **opts):
            return repeated_timing(function, *args, N=N, **opts)

        return inner

    return repeated


def repeated_timing(function, *args, **opts):
    N = opts.pop('N', 30)
    timed_func = timed(function)
    timers = []
    for _ in range(N):
        timer, res = timed_func(*args, **opts)
        timers.append(timer)
    timers = sorted(timers)
    min = int(.1 * N)
    max = int(.9 * N)
    trimmed_timers = timers[min:max]
    return sum(trimmed_timers) / len(trimmed_timers), res


def print_elapsed_time(viewfunc):
    @wraps(viewfunc)
    def inner(*args, **kw):
        t = time.perf_counter()
        res = viewfunc(*args, **kw)
        print("Done %s: %s sec" % (inner.__name__,
                                   Decimal(time.perf_counter() - t).quantize(
                                       Decimal("0.000000"))))
        return res

    return inner
