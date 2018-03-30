# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.encoding import force_text

class WLExpressionMeta(object):
    
    def __bool__(self):
        return True

    def __call__(self, *args, **opts):
        return WLFunction(self, *args, **opts)

class WLSymbol(WLExpressionMeta):

    __slots__ = 'name'

    def __init__(self, name):

        if isinstance(name, six.binary_type):
            self.name = force_text(name)
        elif isinstance(name, six.text_type):
            self.name = name
        else:
            raise ValueError('Symbol name should be %s not %s. You provided: %s' % (
                six.text_type.__name__,
                name.__class__.__name__,
                name
            ))

    def __len__(self):
        return 0 #consistent with Length(x)

    def __eq__(self, other):
        return isinstance(other, WLSymbol) and self.name == other.name

    def __repr__(self):
        return self.name

class WLFunction(WLExpressionMeta):

    #reminder: use slots to reduce memory usage:
    #https://stackoverflow.com/questions/472000/usage-of-slots
    #https://www.codementor.io/satwikkansal/python-practices-for-efficient-code-performance-memory-and-usability-aze6oiq65

    __slots__ = 'head', 'args'

    def __init__(self, head, *args, **opts):
        self.head = head

        if opts:
            self.args = tuple(chain(args, (wl.Rule(k, v) for k, v in opts.items())))
        else:
            self.args = args

    def __len__(self):
        return len(self.args)

    def __repr__(self):
        return '%s[<< %s >>]' % (repr(self.head), len(self))

class ExpressionFactory(object):

    def __init__(self, context = None):
        self.context = context

    def __getattr__(self, attr):
        if self.context:
            return WLSymbol('%s`%s' % (self.context, attr))
        return WLSymbol(attr)

wl     = ExpressionFactory()
system = ExpressionFactory('System')