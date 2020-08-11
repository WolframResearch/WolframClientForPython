from __future__ import absolute_import, print_function, unicode_literals

from collections import OrderedDict


class Association(OrderedDict):
    """ A :class:`~collections.OrderedDict` that serializes to an Association"""

    def __repr__(self):
        return dict.__repr__(self)


def _fail(self, *args, **opts):
    raise TypeError("{0} does not support item assignment".format(self.__class__.__name__))


class immutabledict(dict):
    """
    hashable dict implementation, suitable for use as a key into
    other dicts.

        >>> h1 = immutabledict({"apples": 1, "bananas":2})
        >>> h2 = immutabledict({"bananas": 3, "mangoes": 5})
        >>> h1+h2
        immutabledict(apples=1, bananas=3, mangoes=5)
        >>> d1 = {}
        >>> d1[h1] = "salad"
        >>> d1[h1]
        'salad'
        >>> d1[h2]
        Traceback (most recent call last):
        ...
        KeyError: immutabledict(bananas=3, mangoes=5)

    based on answers from
       http://stackoverflow.com/questions/1151658/python-hashable-dicts

    """

    def __hash__(self):
        return hash(tuple(self.items()))

    __setitem__ = _fail
    __delitem__ = _fail
    clear = _fail
    pop = _fail
    popitem = _fail
    setdefault = _fail
    update = _fail

    def __add__(self, right):
        result = hashdict(self)
        dict.update(result, right)
        return result


class Settings(dict):
    """
    Dictionary subclass enabling attribute lookup/assignment of keys/values.

    For example::

        >>> m = Settings({'foo': 'bar'})
        >>> m.foo
        'bar'
        >>> m.foo = 'not bar'
        >>> m['foo']
        'not bar'

    ``Settings`` objects also provide ``.first()`` which acts like
    ``.get()`` but accepts multiple keys as arguments, and returns the value of
    the first hit, e.g.::

        >>> m = Settings({'foo': 'bar', 'biz': 'baz'})
        >>> m.first('wrong', 'incorrect', 'foo', 'biz')
        'bar'

    """

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            # to conform with __getattr__ spec
            raise AttributeError(key)

    def __setattr__(self, key, value):
        self[key] = value
