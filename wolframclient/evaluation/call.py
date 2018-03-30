# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

class WolframCall(object):
    def __init__(self, inputs, output):
        self.inputs = inputs
        self.output = output

    def perform(self, input):
        raise NotImplementedError()


class WolframAPICall(WolframCall):
    def __init__(self, exec_context, api, result_type=None):
        self.exec_context = exec_context
        self.api = api
        self.result_type = result_type
        self.result = None
        self._success = None

    def perform(self, input={}):
        self.result = self.exec_context.execute(self.api, input)
        
    @property
    def success(self):
        return self.result.success


class CallResult(object):
    def __init__(self):
        self.meta = {
            'caller' : None,
            'success' : None,
            'done' : False,
            'target' : None
        }
        
