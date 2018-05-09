# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import wl
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.utils.datastructures import Association
from wolframclient.utils.encoding import safe_force_text

class WolframLanguageException(WLSerializable, Exception):

    def __init__(self, payload, exec_info = None):

        self.payload = payload

        if exec_info:
            self.set_traceback(*exec_info)
        else:
            self.set_traceback(None, None, None)

    def failure_tag(self):
        return "PythonError"

    def failure_template(self):
        return safe_force_text(self.payload)

    def failure_parameters(self):
        return {}

    def failure_code(self):
        if isinstance(self.payload, Exception):
            return safe_force_text(self.payload.__class__.__name__)

    def set_traceback(self, exc_type, exc_value, tb):
        self.exc_type, self.exc_value, self.tb = exc_type, exc_value, tb

    def to_wl(self, **opts):
        return wl.Failure(
            self.failure_tag(),
            #the failure code can contain code that needs evaluation
            #we need to use Association to prevent WXF from constructing an association
            Association(self.failure_meta())
        )

    def show_traceback(self):
        return True

    def failure_meta(self):

        template, parameters, code = self.failure_template(), self.failure_parameters(), self.failure_code()

        if template:
            yield "MessageTemplate",   template
            yield "MessageParameters", parameters

        if code:
            yield "FailureCode", code

        if self.show_traceback() and self.tb:

            from wolframclient.language.traceback import serialize_traceback

            yield "Traceback", serialize_traceback(
                self.exc_type,
                self.exc_value,
                self.tb,
            )