# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import inspect

from wolframclient.utils.functional import flatten

#original idea by Guido in person.
#https://www.artima.com/weblogs/viewpost.jsp?thread=101605


class Dispatch(object):
    """ A method dispatcher class allowing for multiple implementations of a function. Each implementation is associated to a specific input type.
    
    Implementations are registered with the annotation :meth:`~wolframclient.utils.dispatch.Dispatch.dispatch`.

    The Dispatch class is callable, it behaves as a function that uses the implementation corresponding to the input parameter.

    When a type is a subtype, the type and its parents are checked in the order given by :data:`__mro__` (method resolution order). 
        
    *Example:* method :meth:`~wolframclient.utils.dispatch.Dispatch.resolve` applied to an instance of :class:`collections.OrderedDict`,
    check for the first implementation to match with :class:`collections.OrderedDict`, then with :class:`dict`, and ultimately to :data:`object`.

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

        Implementation must be unique. By default, registering the same combination of types will raise an error.
        Set `replace_existing` to :data:`True` to update the current mapping.
        Or, set `keep_existing` to :data:`True` to ignore duplicate registration and keep the existing mapping.
        """

        def register(func):
            return self.register(func, *args, **opts)

        return register

    def update(self, dispatch, **opts):
        """ Update current mapping with the one from `dispatch`.
        
        `dispatch` can be a Dispatch instance or a :class:`dict`.
        `**opts` are passed to :meth:`~wolframclient.utils.dispatch.Dispatch.register`
         """
        if isinstance(dispatch, Dispatch):
            dispatchmapping = dispatch.dispatch_dict
        elif isinstance(dispatch, dict):
            dispatchmapping = dispatch
        else:
            raise ValueError('%s is not an instance of Dispatch' % dispatch)
        for t, function in dispatchmapping.items():
            self.register(function, t, **opts)

    def validate_types(self, types):
        for t in frozenset(flatten(types)):
            if not inspect.isclass(t):
                raise ValueError('%s is not a class' % t)
            yield t

    def register(self,
                 function,
                 types=object,
                 keep_existing=False,
                 replace_existing=False):
        """ Equivalent to annotation :meth:`~wolframclient.utils.dispatch.Dispatch.dispatch` but as 
        a function.
        """
        if not callable(function):
            raise ValueError('Function %s is not callable' % function)
        if keep_existing and replace_existing:
            raise ValueError(
                'Option values keep_existing and replace_existing cannot be both True.'
            )

        self.clear_cache()

        for t in self.validate_types(types):
            if replace_existing:
                self.dispatch_dict[t] = function
            elif t in self.dispatch_dict:
                if not keep_existing:
                    raise TypeError(
                        "Duplicated registration for input type(s): %s" %
                        (t, ))
            else:
                self.dispatch_dict[t] = function

        return function

    def unregister(self, types=object):
        """ Remove implementations associated with types. """

        self.clear_cache()

        for t in self.validate_types(types):
            try:
                del self.dispatch_dict[t]
            except KeyError:
                pass

    def clear(self):
        """ Reset the dispatcher to its initial state. """
        self.dispatch_dict = dict()
        self.dispatch_dict_cache = dict()

    def clear_cache(self):
        if self.dispatch_dict_cache:
            self.dispatch_dict_cache = dict()

    def resolve(self, arg):
        """ Return the implementation better matching the type the argument type. """
        for t in arg.__class__.__mro__:
            try:
                return self.dispatch_dict_cache[t]
            except KeyError:
                impl = self.dispatch_dict.get(t, None)
                if impl:
                    self.dispatch_dict_cache[t] = impl
                    return impl

        return self.default_function

    def default_function(self, *args, **opts):
        """ Ultimately called when no type was found. """
        raise ValueError('Unable to handle args')

    def __call__(self, arg, *args, **opts):
        return self.resolve(arg)(arg, *args, **opts)

    def as_method(self):
        """ Return the dispatch as a class method. 
        
        Create a new dispatcher::

            dispatch = Dispatcher()

        Use the dispatcher as a class method::

            class MyClass(object):
                myMethod = dispatch.as_method()

        Call the class method::
            
            o = MyClass()
            o.myMethod(arg, *args, **kwargs)

        """

        def method(instance, arg, *args, **opts):
            return self.resolve(arg)(instance, arg, *args, **opts)

        return method
