# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import warnings
from collections import defaultdict
from itertools import product, chain

from wolframclient.utils.functional import force_tuple, flatten

#original idea by Guido in person.
#https://www.artima.com/weblogs/viewpost.jsp?thread=101605

UNDEFINED = object()

def default_function(*args, **opts):
    raise ValueError('Unable to handle args')

class Dispatch(object):
    """ A method dispatcher class allowing for multiple implementations of functions specified by their name.
    Each implementation is associated to a set of input types.
    
    Imprementations are registered with the annotation :meth:`~wolframclient.utils.dispatch.Dispatch.multi`.

    The function implementation must be called using :meth:`~wolframclient.utils.dispatch.Dispatch.create_proxy`.
    It resolves the type mapping, if need be, and caches the result, then applies the implementation to
    the input argument.
        
    If the types is not mapped to a specific function, i.e. it is not found in dispatchmap,
    and if a default method is set, the tuple type is associated to this default to speedup next
    invocation. This imply that the mapping will not be checked anymore.

    Where there is a type hierarchy, all possible combinations are checked in order, following MRO,
    and argument order. Finding the best matching mapping can be a costly operation, performed once. 
        
    Example: call :meth:`~wolframclient.utils.dispatch.Dispatch.create_proxy` with two arguments of types (:class:`collections.OrderedDict`, :class:`dict`) 
    (:class:`~collections.OrderedDict` inherits from :class:`dict`). The type combinations are checked as follows:

        - (OrderedDict, dict)
        - (OrderedDict, object)
        - (dict, dict)
        - (dict, object)

    Once the mapping is determined, it is cached for later use.
    """
    def __init__(self):
        self.dispatchmap = dict()
        self.unregister()

    def dispatch(self, *types):
        """ Annotate a function and map it to a given set of type(s).

        Multiple mappings for a given function must share the same name as defined by :meth:`~wolframclient.utils.dispatch.Dispatch.get_key`.
        
        Declare an implementation to use on :data:`bytearray` input::

            @dispatcher.multi(bytearray)
            def my_func(...)

        Function with many arguments are specified with a list of types. Declare an implementation for two arguments of type 
        :data:`str` and :data:`int`::

            @dispatcher.multi([str, int])
            def my_func(...)

        A tuple can be used as a type to specify alternative choices for a given parameter. 
        Declare a function used for both :data:`bytes` and :data:`bytearray`::

            @dispatcher.multi((bytes, bytearray))
            def my_func(...)
        
        Implementation must be unique. Registering the same combinaison of types will raise an error.
        """

        def register(func):
            return self.register(func, *types)

        return register

    def clear(self):
        """ Removes the local cache """
        self.cache = dict()

    def update(self, dispatch, update_default = False):
        if isinstance(dispatch, Dispatch):
            for t, function in dispatch.dispatchmap.items():
                self.register(function, t)
            if update_default and dispatch.default_function:
                self.register_default(dispatch.default_function)
            self.clear()
        else:
            raise ValueError('%s is not an instance of Dispatch' % dispatch)
            
    def register(self, function, *types):
        if not types:
            if self.default_function:
                raise TypeError("Dispatch already has a default function registred.")  
            self.default_function = function
            self.clear()
            return self.default_function

        for t in flatten(*types):
            if t in self.dispatchmap:
                raise TypeError("Duplicated registration for input type(s): %s" % (t, ))  
            self.dispatchmap[t] = function

        self.clear()
        return function

    def unregister(self, *types):
        if not types:
            self.default_function = None
        else:
            for t in flatten(types):
                try:
                    del self.dispatchmap[t]
                except KeyError:
                    pass
        self.clear()

    def __call__(self, arg, *args, **opts):
        return self.resolve(arg)(arg, *args, **opts)

    def resolve(self, arg):
        try:
            return self.cache[arg.__class__]
        except KeyError:
            for t in arg.__class__.__mro__:
                try:
                    self.cache[t] = self.dispatchmap[t]
                    return self.cache[t]
                except KeyError:
                    pass

            self.cache[arg.__class__] = self.default_function or default_function
            return self.cache[arg.__class__]

    def as_method(self):
        def method(instance, arg, *args, **opts):
            return self.resolve(arg)(instance, arg, *args, **opts)
        return method