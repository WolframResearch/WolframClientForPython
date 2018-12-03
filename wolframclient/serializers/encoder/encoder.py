# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import inspect
import sys
import functools
import logging
import warnings
from collections import defaultdict
from importlib import import_module

from wolframclient.utils.dispatch import ClassDispatch
from wolframclient.utils.functional import composition, iterate
from wolframclient.utils.importutils import safe_import_string
from wolframclient.utils import six
from wolframclient.utils.api import multiprocessing

logger = logging.getLogger(__name__)


dispatch = ClassDispatch()

def wolfram_encoder(*types):
    """ Annotation to declare a given function as an Wolfram encoder for a given list of types. 

    An encoder is a generator with two arguments: a serializer instance, and an object of a type matching 
    one of the type in `types`. It returns bytes. 

    Define an encoder applied to input of type `bytes` or `bytearray`::

        @wolfram_encoder(bytes, bytearray)
        def func(serializer, o):
            raise NotImplementedError

    Serializer is expected to be a :class:`~wolframclient.serializers.base.FormatSerializer`.
    """
    if logger.isEnabledFor(logging.INFO):
        logger.info('New Wolfram encoder for types: %s', types)
    def wrap(fn):
        @functools.wraps(fn)
        @dispatch.multi(types)
        def encode(serializer, o):
            return fn(serializer, o)
        return encode
    return wrap

# for now, this method name is fixed and must match the one in the wolfram_encoder wrapper.
@dispatch.default()
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
        self.dispatch = dispatch

    def register_modules(self, **handlers):
        for module, handlers in handlers.items():
            self.modules.add(module)
            self.registry[module].extend(iterate(handlers))

    def _update_dispatch(self):
        if self.modules:
            for module in self.modules.intersection(sys.modules.keys()):
                for handler in self.registry[module]:
                    import_module(handler)

                del self.registry[module]
                self.modules.remove(module)

    if not six.JYTHON:
        # global lock to avoid multiple dispatcher updating in multithreaded programs.
        _lock = multiprocessing.Lock()

        def update_dispatch(self):
            with self._lock:
                self._update_dispatch()
    else:

        def update_dispatch(self):
            self._update_dispatch()


updater = DispatchUpdater(dispatch)
updater.register_modules(

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


class Encoder(object):
    """ A generic class exposing an :meth:`~wolframclient.serializers.encode.Encoder.encode`
    method applying an optional normalizer function, followed the most relevant encoding available 
    for a given type.
    """

    default_encoder = encode
    default_updater = updater

    def __init__(self,
                 normalizer=None,
                 allow_external_objects=False,
                 target_kernel_version=None):
        self.encode = self.chain_normalizer(normalizer)
        self.allow_external_objects = allow_external_objects
        self.target_kernel_version = target_kernel_version or 11.3

    def chain_normalizer(self, func):

        self.default_updater.update_dispatch()

        return composition(*map(safe_import_string,
                                iterate(func or (), self.default_encoder)))
