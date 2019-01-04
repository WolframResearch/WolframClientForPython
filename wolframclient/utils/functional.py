# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import inspect
from functools import reduce
from itertools import chain, islice

from wolframclient.utils import six

if six.PY2:

    def map(f, iterable):
        return (f(e) for e in iterable)
else:
    map = map


def first(iterable, default=None):
    try:
        return next(iter(iterable))
    except StopIteration:
        return default


def last(iterable, default=None):
    try:
        return iterable[-1]
    except IndexError:
        return default


def identity(x):
    return x


def composition(*functions):
    return reduce(lambda f, g: lambda *args, **kw: f(g(*args, **kw)),
                  reversed(functions or (identity, )))


def is_iterable(obj, exclude_list=six.string_types):
    if isinstance(obj, exclude_list):
        return False
    return not inspect.isclass(obj) and hasattr(obj, '__iter__')


def to_iterable(obj, exclude_list=six.string_types):
    if isinstance(obj, exclude_list):
        return obj,
    try:
        return iter(obj)
    except TypeError:
        return obj,


def iterate(*args):
    return chain.from_iterable(map(to_iterable, args))


def flatten(*args):
    for arg in args:
        if is_iterable(arg):
            for sub in arg:
                for el in flatten(sub):
                    yield el
        else:
            yield arg


def riffle(iterable, separator):
    iterable = iter(iterable)
    try:
        yield next(iterable)
        for el in iterable:
            yield separator
            yield el
    except StopIteration:
        pass


def partition(iterable, n):
    """ Yield successive n-sized chunks from l. """
    iterable = iter(iterable)
    res = tuple(islice(iterable, n))
    while len(res) != 0:
        yield res
        res = tuple(islice(iterable, n))
