# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import inspect
import sys
import functools
import logging
import warnings
import pkg_resources
from collections import defaultdict
from importlib import import_module

from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import composition, iterate
from wolframclient.utils.importutils import safe_import_string
from wolframclient.utils import six
from wolframclient.utils.api import multiprocessing

logger = logging.getLogger(__name__)

__all__ = ['wolfram_encoder', 'Encoder']

class _NoForceContext(object):
    """ Disable the possiblity to force update the mapping of types to implementation, for a given scope. """
    def __init__(self, encoder):
        self.encoder = encoder
    
    def __enter__(self):
        self.encoder._disable_force_update = True
        return self

    def __exit__(self, type, value, traceback):
        self.encoder._disable_force_update = False


class WolframEncoder(Dispatch):
    """ Multi method implementation targeting Wolfram encoder functions. The first element is of a given type, 
    known ahead of time, it's a serializer instance, and as such can be ignored both during dispatch and resolve.

    Only the second parameter, the object to encode, is used during dispatch. A tuple can be provided if one method
    is supporting more than one type.

    Named input parameters are not used to resolve which implementation to use, but are passed to the
    function.
    """

    def __init__(self):
        super().__init__()
        self._disable_force_update = False

    def create_proxy(self, encoder):
        def inner(serializer, o, **opts):
            return self._resolve(encoder, o)(serializer, o, **opts)

        return inner

    def register(self, *types, force=False):
        """ Annotation to declare a given function as an Wolfram encoder for a given list of types. 

        An encoder is a generator with two arguments: a serializer instance, and an object of a type matching 
        one of the type in `types`. It returns bytes. 

        Define an encoder applied to input of type `bytes` or `bytearray`::

            @wolfram_encoder.register(bytes, bytearray)
            def func(serializer, o):
                raise NotImplementedError

        Serializer is expected to be a :class:`~wolframclient.serializers.base.FormatSerializer`.
        """
        for t in types:
            if not inspect.isclass(t):
                raise ValueError('Invalid type specification. %s is not a class.' % (t,))
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('New Wolfram encoder for types: %s', types)

        if self._disable_force_update and force:
            raise ValueError('Cannot use force to override %s encoder from a plugin.' % (types,))

        def wrap(fn):
            @functools.wraps(fn)
            @self.multi(types, force=force)
            def encode(serializer, o):
                return fn(serializer, o)
            return encode
        return wrap
    
    def plugin_context(self):
        return _NoForceContext(self)


wolfram_encoder = WolframEncoder()
""" Instance of :class:`~wolframclient.serializers.encoder.WolframEncoder` used by default during serialization. """

# for now, this method name is fixed and must match the one in the wolfram_encoder wrapper.
@wolfram_encoder.default()
def encode(serializer, o):
    if not inspect.isclass(o) and hasattr(o, '__iter__'):
        return serializer.serialize_iterable(serializer.encode(value) for value in o)
    if serializer.allow_external_objects:
        return serializer.serialize_external_object(o)

    raise NotImplementedError(
        'Cannot serialize object of class %s' % o.__class__)


class DispatchUpdater(object):
    def __init__(self, dispatch):
        self.registry = defaultdict(list)
        self.modules = set()
        self.plugins_registry = defaultdict(list)
        self.dispatch = dispatch

    def register_modules(self, **handlers):
        for module, _handlers in handlers.items():
            self.modules.add(module)
            self.registry[module].extend(iterate(_handlers))

    def register_plugins(self, name='wolframclient_serializers_encoder'):
        if logger.isEnabledFor(logging.INFO):
            logger.info('Registering Wolfram encoders plugins associated to entrypoint %s.' % name)
        for entry_point in pkg_resources.iter_entry_points(group=name):
            self.plugins_registry[entry_point.name].extend(entry_point.module_name)

    def _update_dispatch(self):
        if self.modules:
            installed_modules = sys.modules.keys()
            for module in self.modules.intersection(installed_modules):
                for handler in self.registry[module]:
                    import_module(handler)

                del self.registry[module]
                self.modules.remove(module)
            
    def _update_plugins(self):
        if self.plugins_registry:
            with wolfram_encoder.plugin_context():
                for plugins_name, handler in self.plugins_registry.items():
                    handler = ''.join(handler)
                    try:
                        import_module(handler)
                    except TypeError as e:
                        warnings.warn('Failed to load encoder associated to plugins %s. The following error occured while loading %s: %s' % 
                            (plugins_name, handler, e), UserWarning)
                self.plugins_registry = defaultdict(list)


    if not six.JYTHON:
        # global lock to avoid multiple dispatcher updating in multithreaded programs.
        _lock = multiprocessing.Lock()
        
        def update_dispatch(self):
            with self._lock:
                self._update_dispatch()
                self._update_plugins()
        
    else:
        def update_dispatch(self):
            self._update_dispatch()
            self._update_plugins()


wolfram_encoder_updater = DispatchUpdater(wolfram_encoder)
wolfram_encoder_updater.register_modules(

    #builtin libraries
    sys='wolframclient.serializers.encoder.builtin',
    decimal='wolframclient.serializers.encoder.decimal',
    datetime='wolframclient.serializers.encoder.datetime',
    fractions='wolframclient.serializers.encoder.fractions',

    #wolfram language support
    wolframclient=
    'wolframclient.serializers.encoder.wolfram',

    #third party libraries
    numpy='wolframclient.serializers.encoder.numpy',
    PIL='wolframclient.serializers.encoder.pil',
)

wolfram_encoder_updater.register_plugins()

class Encoder(object):
    """ A generic class exposing an :meth:`~wolframclient.serializers.encode.Encoder.encode`
    method applying an optional normalizer function, followed the most relevant encoding available 
    for a given type.
    """

    default_encoder = encode
    default_updater = wolfram_encoder_updater

    def __init__(self,
                 normalizer=None,
                 allow_external_objects=False,
                 target_kernel_version=None,
                 **kwargs):
        self.encode = self.chain_normalizer(normalizer)
        self.allow_external_objects = allow_external_objects
        self.target_kernel_version = target_kernel_version or 11.3
        self._properties = kwargs

    def chain_normalizer(self, func):
        self.default_updater.update_dispatch()

        return composition(*map(safe_import_string,
                                iterate(func or (), self.default_encoder)))

    def get_property(self, key, d=None):
        return self._properties.get(key, d)