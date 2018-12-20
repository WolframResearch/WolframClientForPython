# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import warnings
from collections import defaultdict
from itertools import product, chain

from wolframclient.utils.functional import force_tuple, chain_indexed

#original idea by Guido in person.
#https://www.artima.com/weblogs/viewpost.jsp?thread=101605

UNDEFINED = object()


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
        """ Annotate a function so that its actual implementation is a function found using the mapping
        table and input parameter types. 
        """
        def inner(*args, **opts):
            return self._resolve(function, *args)(*args, **opts)

        return inner

    def multi(self, types):
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
        def register(function):
            if isinstance(types, tuple):
                length = 1
            else:
                length = len(types)
            key = (self.get_key(function), length)
            for expanded in product(*map(force_tuple, types)):
                if expanded in self.dispatchmap[key]:
                    if force:
                        warnings.warn("duplicate registration for input type(s): %s" % (expanded, ), UserWarning)
                    else:
                        raise TypeError("duplicate registration for input type(s): %s" % (expanded, ))

                self.dispatchmap[key][expanded] = function

            return self.get_proxy(function)

        return register

    def default(self):
        """ Annotate a function to be the default implementation.

        There must be one and only one default, which can also be set in the constructor.
        """
        def register(function):

            key = self.get_key(function)

            if key in self.defaultmap:
                raise TypeError("duplicate registration for default")

            self.defaultmap[key] = function

            return self.get_proxy(function)

        return register

    def _resolve(self, source, *args):
        """ Return the implementation associated to given method and type of args. """

        key = (self.get_key(source), len(args))
        types = tuple(arg.__class__ for arg in args)
        # check if the types are associated to a given function:
        function = self.dispatchmap[key].get(types, UNDEFINED)

        if function is UNDEFINED:
            # Need to build all the possible tuples using subclasses and order them properly by MRO,
            # and parameter index.
            # This can quickly become fairly big because of the cartesian products.
            tuple_args = product(*map(force_tuple, types))
            #populating cache
            dispatch = self.dispatchmap[key]
            for tuple_type in chain_indexed(*map(lambda t : self.all_type_combinations(*t), tuple_args)):
                function = dispatch.get(tuple_type, None)
                if function:
                    dispatch[types] = function
                    return function
                
            default = self.defaultmap.get(self.get_key(source), UNDEFINED)
            if default is not UNDEFINED:
                self.dispatchmap[key][types] = default
                return default
            raise TypeError("No type match for arguments: %s", (args,))
        return function

    @staticmethod
    def all_type_combinations(*types):
        """ From a given set of types yield all the combinations of types and parent types.

        The tuple made of sollely `object` is not returned, because it's the default implementation, which should be unique.
        """
        if len(types) == 0:
            return
        mro = list(map(lambda x : x.__mro__, types[:-1]))
        mro.append(types[-1].__mro__[:-1])
        for elem in product(*mro):
            yield elem

class ClassDispatch(Dispatch):
    """ Multi method implementation where the first element is of a given type, known ahead of time,
    and as such can be ignored both during dispatch and resolve.

    Named input parameters are not used to resolve which implementation to use, but are passed to the
    function.
    """
    def create_proxy(self, function):
        def inner(obj, *args, **opts):
            return self._resolve(function, *args)(obj, *args, **opts)

        return inner
