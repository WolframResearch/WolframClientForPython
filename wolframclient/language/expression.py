# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.encoding import force_text

class Expression(object):

    #reminder: use slots to reduce memory usage:
    #https://stackoverflow.com/questions/472000/usage-of-slots
    #https://www.codementor.io/satwikkansal/python-practices-for-efficient-code-performance-memory-and-usability-aze6oiq65

    __slots__ = 'head', 'args', 'context'

    def __init__(self, head, context = None):
        assert isinstance(head, six.string_types)
        self.head    = head
        self.context = context
        self.args    = []

    def fully_qualified_symbol(self, head = None):
        if self.context:
            return '%s`%s' % (self.context, head or self.head)
        return head or self.head

    def __eq__(self, other):
        return isinstance(other, Expression) and (self.head, self.args) == (other.head, other.args)

    def __call__(self, *args, **kw):
        self.args.append((args, kw))
        return self

    def __repr__(self):
        return '%s%s' % (
            force_text(self.head),
            ''.join('[...]' for k in self.args)
        )

    def evaluate(self, evaluator):
        return evaluator.evaluate(self)

class ExpressionFactory(object):

    def __init__(self, context = None):
        self.context = context

    def __getattr__(self, attr):
        return Expression(attr, context = self.context)

wl     = ExpressionFactory()
system = ExpressionFactory('System')