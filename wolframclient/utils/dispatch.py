# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from collections import defaultdict
from itertools import product

from wolframclient.utils.functional import force_tuple

#code borrowed by Guido in person.
#https://www.artima.com/weblogs/viewpost.jsp?thread=101605

UNDEFINED = object()


class Dispatch(object):
    def __init__(self):
        self.dispatchmap = defaultdict(dict)
        self.defaultmap = dict()
        self.proxymap = dict()

    def get_key(self, function):
        return function.__name__

    def get_proxy(self, function):
        key = self.get_key(function)
        try:
            return self.proxymap[key]
        except KeyError:
            self.proxymap[key] = self.create_proxy(function)
            return self.proxymap[key]

    def create_proxy(self, function):
        def inner(*args, **opts):
            return self.resolve(function, *args)(*args, **opts)

        return inner

    def multi(self, *types):
        def register(function):

            key = (self.get_key(function), len(types))

            for expanded in product(*map(force_tuple, types)):
                if expanded in self.dispatchmap[key]:
                    raise TypeError("duplicate registration %s" % (expanded, ))

                self.dispatchmap[key][expanded] = function

            return self.get_proxy(function)

        return register

    def default(self):
        def register(function):

            key = self.get_key(function)

            if key in self.defaultmap:
                raise TypeError("duplicate registration for default")

            self.defaultmap[key] = function

            return self.get_proxy(function)

        return register

    def resolve(self, source, *args):

        key = (self.get_key(source), len(args))
        types = tuple(arg.__class__ for arg in args)

        function = self.dispatchmap[key].get(types, UNDEFINED)

        if function is UNDEFINED:
            #populating cache
            for targets, function in self.dispatchmap[key].items():
                if len(targets) == len(types):
                    if all(issubclass(a, b) for a, b in zip(types, targets)):
                        self.dispatchmap[key][types] = function
                        return function

            default = self.defaultmap.get(self.get_key(source), UNDEFINED)

            if default is not UNDEFINED:
                self.dispatchmap[key][types] = default

                return default
            raise TypeError("no match")
        return function


class ClassDispatch(Dispatch):
    def create_proxy(self, function):
        def inner(obj, *args, **opts):
            return self.resolve(function, *args)(obj, *args, **opts)

        return inner
