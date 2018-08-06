# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.exceptions import WolframLanguageException

class RequestException(WolframLanguageException):
    """Error in HTTP request."""

    __slots__ = 'response'

    def __init__(self, response, msg=None):
        self.response = response
        self.msg = msg

class AuthenticationException(RequestException):
    """Error in authentication request."""

    pass

class EvaluationException(RequestException):
    """Error while evaluating an expression."""

    pass

class WolframKernelException(WolframLanguageException):
    """Error while interacting with a Wolfram Kernel."""

    pass

class SocketException(WolframLanguageException):
    """Error while operating on socket."""

    pass

class WolframParserException(WolframLanguageException):
    """Error while deserializing WXF bytes."""
    pass

__all__ = ['WolframLanguageException',
           'RequestException',
           'AuthenticationException',
           'EvaluationException',
           'WolframKernelException',
           'SocketException',
           'WolframParserException']