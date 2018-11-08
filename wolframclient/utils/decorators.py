# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import functools

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
to_dict = decorate(Association)


def synchronized(lock):
    """Synchronized call to the wrapped function using the provided `lock`."""

    def wrap(fn):
        @functools.wraps(fn)
        def caller(*args, **kwargs):
            with lock:
                return fn(*args, **kwargs)

        return caller

    return wrap


class cached_property(object):
    """
    Decorator that converts a method with a single self argument into a
    property cached on the instance.

    Optional ``name`` argument allows you to make cached properties of other
    methods. (e.g.  url = cached_property(get_absolute_url, name='url') )
    """

    def __init__(self, func, name=None):
        self.func = func
        self.__doc__ = getattr(func, '__doc__')
        self.name = name or func.__name__

    def __get__(self, instance, cls=None):
        if instance is None:
            return self
        res = instance.__dict__[self.name] = self.func(instance)
        return res
