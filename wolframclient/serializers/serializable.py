# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

__all__ = ['WLSerializable']


class WLSerializable:
    """A class that can be serialized using :func:`~wolframclient.serializers.export`.

    Custom serialization of a class is done by subclassing this class::

        from wolframclient.serializers.serializable import WLSerializable
        from wolframclient.language import wl
        from wolframclient.serializers import export

        class MyPythonClass(WLSerializable):
            def __init__(self, *arguments):
                self.arguments = arguments

            def to_wl(self):
                return wl.MyWolframFunction(*self.arguments)

    Serialize :data:`MyPythonClass` using :func:`~wolframclient.serializers.export`::

        >>> export(MyPythonClass('foo', 'bar'))
        b'MyWolframFunction["foo", "bar"]'

    Serialization is applied recursively; arguments are also serialized::

        >>> export(MyPythonClass(1, 2, MyPythonClass(2, 3)))
        'MyWolframFunction[1, 2, MyWolframFunction[2, 3]]'

    """

    def to_wl(self):
        """ Return the serialized form of a given Python class.
        
        The returned value must be a combination of serializable types.
        """
        raise NotImplementedError(
            'class %s must implement a to_wl method' % self.__class__.__name__)
