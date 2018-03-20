# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import wl
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.utils.decorators import to_dict
from wolframclient.utils.encoding import safe_force_text

class WolframLanguageExceptionBase(WLSerializable, Exception):

    def __init__(self):
        raise NotImplementedError

    def failure_tag(self):
        raise NotImplementedError

    def failure_template(self):
        raise NotImplementedError

    def failure_parameters(self):
        return {}

    def set_traceback(self, exc_type, exc_value, tb):
        self.exc_type, self.exc_value, self.tb = exc_type, exc_value, tb

    def to_wl(self, **opts):
        return wl.Failure(
            self.failure_tag(),
            wl.Association(*(
                wl.RuleDelayed(key, value)
                for key, value in self.failure_meta().items()
            ))
        )

    def show_traceback(self):
        return True

    @to_dict
    def failure_meta(self):

        template, parameters = self.failure_template(), self.failure_parameters()

        if template:
            yield "MessageTemplate",   template

        if parameters:
            yield "MessageParameters", parameters

        if self.show_traceback() and self.tb:

            from wolframclient.language.traceback import serialize_traceback

            yield "Traceback", serialize_traceback(self.exc_type, self.exc_value, self.tb)

class WolframLanguageExceptionFromPython(WolframLanguageExceptionBase):

    def __init__(self, exception, exec_info = None, show_traceback = True, failure_tag = None):

        self.exception = exception

        self._show_traceback   = show_traceback
        self._failure_tag      = failure_tag

        if exec_info:
            self.set_traceback(*exec_info)
        else:
            self.set_traceback(None, None, None)

    def show_traceback(self):
        return bool(self._show_traceback)

    def failure_tag(self):
        return safe_force_text(self._failure_tag or self.exception.__class__.__name__)

    def failure_template(self):
        return safe_force_text(self.exception)