# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from collections import defaultdict

from wolframclient.utils.dispatch import ClassDispatch
from wolframclient.utils.functional import composition, iterate
from wolframclient.utils.importutils import safe_import_string
from wolframclient.utils import six
import inspect
import sys
from wolframclient.utils.api import multiprocessing

dispatch = ClassDispatch()


@dispatch.default()
def normalizer(self, o):
    if not inspect.isclass(o) and hasattr(o, '__iter__'):
        return self.serialize_iterable(self.normalize(value) for value in o)
    if self.allow_external_objects:
        return self.serialize_external_object(o)

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
                    safe_import_string(handler)(self.dispatch)

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
    sys='wolframclient.serializers.normalizer.builtin.update_dispatch',
    decimal='wolframclient.serializers.normalizer.decimal.update_dispatch',
    datetime='wolframclient.serializers.normalizer.datetime.update_dispatch',
    fractions='wolframclient.serializers.normalizer.fractions.update_dispatch',

    #wolfram language support
    wolframclient=
    'wolframclient.serializers.normalizer.wolfram.update_dispatch',

    #third party libraries
    numpy='wolframclient.serializers.normalizer.numpy.update_dispatch',
    PIL='wolframclient.serializers.normalizer.pil.update_dispatch',
)


class Normalizer(object):

    default_normalizer = normalizer
    default_updater = updater

    def __init__(self,
                 normalizer=None,
                 allow_external_objects=False,
                 target_kernel_version=None):
        self.normalize = self.chain_normalizers(normalizer)
        self.allow_external_objects = allow_external_objects
        self.target_kernel_version = target_kernel_version or 11.3

    def chain_normalizers(self, func):

        self.default_updater.update_dispatch()

        return composition(*map(safe_import_string,
                                iterate(func or (), self.default_normalizer)))
