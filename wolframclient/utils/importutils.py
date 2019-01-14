# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import os
from importlib import import_module

from wolframclient.utils import six


def module_path(module, *args):
    if isinstance(module, six.string_types):
        try:
            module = import_module(module)
        except ImportError:
            return None
    return os.path.join(
        os.path.dirname(os.path.realpath(module.__file__)), *args)


def import_string(dotted_path):
    """
    Import a dotted module path and return the attribute/class designated by the
    last name in the path. Raise ImportError if the import failed.
    """

    if not isinstance(dotted_path, six.string_types):
        return dotted_path

    try:
        module_path, class_name = dotted_path.rsplit('.', 1)
    except ValueError:
        raise ImportError("%s doesn't look like a module path" % dotted_path)

    module = import_module(module_path)

    if class_name == '__module__':
        return module

    try:
        return getattr(module, class_name)
    except AttributeError:
        raise ImportError('Module "%s" does not define a "%s" attribute/class'
                          % (module_path, class_name))


def safe_import_string(f):
    if isinstance(f, (list, tuple)):
        for path in f:
            try:
                return import_string(path)
            except ImportError:
                pass
        raise ImportError('Cannot import %s' % (f, ))
    if isinstance(f, six.string_types):
        return import_string(f)
    return f


def safe_import_string_and_call(f, *args, **kw):
    return safe_import_string(f)(*args, **kw)


class API(object):
    def __init__(self, importer=safe_import_string, **mapping):
        self.__dict__['importer'] = importer
        self.__dict__['mapping'] = mapping
        self.__dict__['imports'] = {}

    def __getattr__(self, value):
        key = self.__dict__['mapping'][value]
        try:
            return self.__dict__['imports'][key]
        except KeyError:
            self.__dict__['imports'][key] = self.__dict__['importer'](key)
            return self.__dict__['imports'][key]

    def __getitem__(self, key):
        try:
            return getattr(self, key)
        except AttributeError:
            raise KeyError('unregistred module %s' % key)

    def __len__(self):
        return len(self.__dict__['mapping'])

    def __bool__(self):
        return bool(self.__dict__['mapping'])

    def keys(self):
        return iter(self)

    def values(self):
        for key in self:
            yield self[key]

    def items(self):
        return zip(self.keys(), self.values())

    def __repr__(self):
        return '<%s %s>' % (self.__class__.__name__, ", ".join(sorted(self)))

    def __dir__(self):
        return list(self)

    def __iter__(self):
        return iter(self.__dict__['mapping'].keys())
