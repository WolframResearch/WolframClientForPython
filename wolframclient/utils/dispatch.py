# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import inspect

from wolframclient.utils.functional import flatten

#original idea by Guido in person.
#https://www.artima.com/weblogs/viewpost.jsp?thread=101605

UNDEFINED = object()

class Dispatch(object):
    """ A method dispatcher class allowing for multiple implementations of a function. Each implementation is associated to a given of input type.
    
    Imprementations are registered with the annotation :meth:`~wolframclient.utils.dispatch.Dispatch.dispatch`.

    The Dispatch class is callable, it behaves as a function that uses the implementation corresponding to the input parameter.
        
    When a mapping is 
    If the types is not mapped to a specific function, and if a default method is set, the tuple type is associated to this default to speedup next
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
        self.clear()

    def dispatch(self, *args, **opts):
        """ Annotate a function and map it to a given set of type(s).
        
        Declare an implementation to use on :data:`bytearray` input::

            @dispatcher.dispatch(bytearray)
            def my_func(...)

        The default implementation is associated with :data:`object`. Set a default::

            @dispatcher.dispatch(object)
            def my_default_func(...)

        A tuple can be used as input to associate more than one type with a function. 
        Declare a function used for both :data:`bytes` and :data:`bytearray`::

            @dispatcher.dispatch((bytes, bytearray))
            def my_func(...)

        Implementation must be unique. Registering the same combinaison of types will raise an error.
        """

        def register(func):
            return self.register(func, *args, **opts)

        return register

    def update(self, dispatch, force = False):
        """ Update current mapping with the one from `dispatch`. """
        if isinstance(dispatch, Dispatch):
            dispatchmapping = dispatch.dispatchdict
        elif isinstance(dispatch, dict):
            dispatchmapping = dispatch
        else:
            raise ValueError('%s is not an instance of Dispatch' % dispatch)
        for t, function in dispatchmapping.items():
            self.register(function, t, force = force)

    def validate_types(self, types):
        for t in frozenset(flatten(types)):
            if not inspect.isclass(t):
                raise ValueError('%s is not a class' % t)
            yield t

    def register(self, function, types = object, force = False):
        if not callable(function):
            raise ValueError('Function %s is not callable' % function)

        self.reset_cached_mapping()

        for t in self.validate_types(types):
            if not force and t in self.dispatchdict:
                raise TypeError(
                    "Duplicated registration for input type(s): %s" % (t, ))
            else:
                self.dispatchdict[t] = function

        return function

    def unregister(self, types = object):
        """ Remove implementations associated to types. """

        self.reset_cached_mapping()

        for t in self.validate_types(types):
            try:
                del self.dispatchdict[t]
            except KeyError:
                pass

    def clear(self):
        """ Reset the dispatcher to its initial state. """
        self.dispatchdict = dict()
        self.cached_mapping = dict()

    def reset_cached_mapping(self):
        self.cached_mapping = dict()

    def __call__(self, arg, *args, **opts):
        return self.resolve(arg)(arg, *args, **opts)

    def resolve(self, arg):
        for t in arg.__class__.__mro__:
            try:
                return self.cached_mapping[t]
            except KeyError:
                impl = self.dispatchdict.get(t, None)
                if impl:
                    self.cached_mapping[t] = impl
                    return impl

        return self.default_function

    def default_function(self, *args, **opts):
        raise ValueError('Unable to handle args')

    def as_method(self):
        """ Return the dispatch as a class method. 
        
        If :data:`myMethod` is the function dispatched on input :data:`arg`, it enables:: 
            
            self.myMethod(arg, *args, **kwargs)

        """

        def method(instance, arg, *args, **opts):
            return self.resolve(arg)(instance, arg, *args, **opts)

        return method
