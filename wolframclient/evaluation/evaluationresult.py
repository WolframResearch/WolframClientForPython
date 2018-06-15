# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals


class WolframEvaluationResult(object):
    __slots__ = 'success', 'failure', 'expr'

    def __init__(self, expr=None, failure=None):
        self.success = expr is not None
        self.failure = failure
        self.expr = expr

    def result(self):
        if self.success:
            return self.expr
        else:
            return self.failure

    def __repr__(self):
        if self.success:
            return '{}<success={}, expr={}>'.format(self.__class__.__name__, self.success, self.expr)
        else:
            return '{}<success={}, failure={}>'.format(self.__class__.__name__, self.success, self.failure)
