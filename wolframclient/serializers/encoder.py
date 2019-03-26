# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import sys
from collections import defaultdict

import pkg_resources

from wolframclient.serializers.utils import safe_len
from wolframclient.utils import six
from wolframclient.utils.api import multiprocessing
from wolframclient.utils.dispatch import Dispatch
from wolframclient.utils.functional import (composition, is_iterable, iterate,
                                            map)
from wolframclient.utils.importutils import safe_import_string

logger = logging.getLogger(__name__)

__all__ = ['wolfram_encoder', 'Encoder']

wolfram_encoder = Dispatch()
wolfram_encoder.__doc__ = """ 
    Mapping between Python types and encoders used during serializations.

    This instance of :class:`~wolframclient.utils.dispatch.Dispatch` is used in :func:`~wolframclient.serializers.export`
    to serialize Python expressions and produce a stream of bytes.

    **Register new encoders:**


    The annotation :meth:`~wolframclient.utils.dispatch.Dispatch.dispatch` applied to a function, defines an encoder, and associates 
    it to the types passed as argument of the annotation.

    Define a new class::

        class MyPythonClass(object):
            def __init__(self, *arguments):
                self.arguments = arguments

    Specify its encoder::

        from wolframclient.serializers import wolfram_encoder
        from wolframclient.language import wl
        from wolframclient.serializers import export

        @wolfram_encoder.dispatch(MyPythonClass)
        def my_encoder(serializer, o):
            return serializer.encode(wl.MyWolframFunction(*o.arguments))

    Serialize an expression::

        >>> export(MyPythonClass(1,2))
        b'MyWolframFunction[1, 2]'

    Alternatively, apply :meth:`~wolframclient.utils.dispatch.Dispatch.register` to a function and its associated 
    type(s) achieves the same result.

    It is not possible to associate two encoders with the same type, but it's possible to remove a mapping. First, 
    unregister the previous encoder::

        wolfram_encoder.unregister(MyPythonClass)

    And register it again with :meth:`~wolframclient.utils.dispatch.Dispatch.register`::

        wolfram_encoder.register(my_encoder, MyPythonClass) 

    **Update with a dispatcher:**


    An other way to extend supported types is to create a new :class:`~wolframclient.utils.dispatch.Dispatch`, map 
    various types and encoders, and ultimatelly update :data:`wolfram_encoder` using 
    :meth:`~wolframclient.utils.dispatch.Dispatch.update`.

    Create a new dispatcher, and register :data:`MyPythonClass`::

        from wolframclient.utils.dispatch import Dispatch

        dispatch = Dispatch()
        dispatch.register(my_encoder, MyPythonClass)

    Update the main encoder with the new dispatch instance::

        wolfram_encoder.update(dispatch)

    Serialize an expression::

        >>> export(MyPythonClass(1,2))
        b'MyWolframFunction[1, 2]'

    **Define plugins:**


    The library supports an entry point dedicated to new encoders: `wolframclient_serializers_encoder`. The library use 
    this entry point to loads plugins at runtime, as separated libraries.
    For more information about entry points, refer to the documentation page: 
    https://packaging.python.org/specifications/entry-points/

    The plugin name must be unique and the value must reference a dispatcher instance. This instance is loaded and used 
    to update :data:`wolfram_encoder`. A plugin is a simple way to distribute encoders as a separe library. 
    
    One type must have a unique encoder associated to it, as a consequence, two plugins registering an encoder for the 
    same type are incompatible. it is strongly advised to create one plugin for each existing Python library. 
    e.g.: have one plugin dedicated to Numpy, and one to Pandas which makes heavy use of Numpy arrays.
    """


@wolfram_encoder.dispatch(object)
def encode(serializer, o):
    if is_iterable(o):
        return serializer.serialize_iterable(
            map(serializer.encode, o), length=safe_len(o))
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
            logger.info(
                'Registering Wolfram encoders plugins associated with entrypoint %s.'
                % name)
        for entry_point in pkg_resources.iter_entry_points(group=name):
            self.plugins_registry[entry_point.name].extend(
                entry_point.module_name)

    def _update_dispatch(self):
        if self.modules:
            installed_modules = sys.modules.keys()
            for module in self.modules.intersection(installed_modules):
                for handler in self.registry[module]:
                    self.dispatch.update(
                        safe_import_string(handler), keep_existing=True)

                del self.registry[module]
                self.modules.remove(module)

    def _update_plugins(self):
        if self.plugins_registry:
            for plugins_name, handler in self.plugins_registry.items():
                handler = ''.join(handler)
                try:
                    self.dispatch.update(safe_import_string(handler))
                except TypeError as e:
                    logger.fatal(
                        'Failed to load encoder associated to plugins %s.' %
                        plugins_name)
                    raise e
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
    sys='wolframclient.serializers.encoders.builtin.encoder',
    decimal='wolframclient.serializers.encoders.decimal.encoder',
    datetime='wolframclient.serializers.encoders.datetime.encoder',
    fractions='wolframclient.serializers.encoders.fractions.encoder',

    #wolfram language support
    wolframclient='wolframclient.serializers.encoders.wolfram.encoder',
    numpy='wolframclient.serializers.encoders.numpy.encoder',
    pandas='wolframclient.serializers.encoders.pandas.encoder',
    PIL=('wolframclient.serializers.encoders.pil.encoder',
         'wolframclient.serializers.encoders.numpy.encoder'),
)

wolfram_encoder_updater.register_plugins()


class Encoder(object):
    """ A generic class exposing an :meth:`~wolframclient.serializers.encode.Encoder.encode`
    method applying an optional normalizer function, followed the most relevant encoding available 
    for a given type.

    Arbitrary named parameters passed during initialization are later accessible with 
    :meth:`~wolframclient.serializers.encode.Encoder.get_property`. 
    """

    default_encoder = wolfram_encoder.as_method()
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
        """ Return the value of the named parameter passed during initialization.

        Set `d` to the default value if key was not present.
        """
        return self._properties.get(key, d)
