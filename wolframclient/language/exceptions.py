# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language import wl
from wolframclient.serializers.serializable import WLSerializable
from wolframclient.utils.decorators import to_dict
from wolframclient.utils.encoding import safe_force_text


class WolframLanguageException(WLSerializable, Exception):
    ''' The most generic exception raised by the Wolfram Client Library.

    This class is :class:`~wolframclient.serializers.serializable.WLSerializable` and will automatically serialize to a
    failure box when evaluated in Wolfram Desktop.
    '''

    def __init__(self, payload, exec_info=None):

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

    def to_wl(self, *args, **opts):
        return wl.Failure(
            self.failure_tag(),
            wl.Association(*(wl.RuleDelayed(key, value)
                             for key, value in self.failure_meta().items())))

    def show_traceback(self):
        return True

    @to_dict
    def failure_meta(self):

        template, parameters, code = self.failure_template(
        ), self.failure_parameters(), self.failure_code()

        if template:
            yield "MessageTemplate", template
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

    def __repr__(self):
        return '%s: %s' % (self.__class__.__name__, self.failure_template())
