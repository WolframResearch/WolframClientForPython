# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.exceptions import WolframLanguageException
from wolframclient.logger.utils import str_trim

class RequestException(WolframLanguageException):
    """Error in HTTP request."""

    __slots__ = 'response'

    def __init__(self, response, msg=None):
        self.response = response
        self.msg = msg

    def __str__(self):
        if self.response is not None and hasattr(self.response, 'status'):
            status = '<%s>: ' % self.response.status
        else:
            status = ''
        return '%s%s' % (status, self.msg)

    
class AuthenticationException(RequestException):
    """Error in authentication request."""
    pass

class WolframKernelException(WolframLanguageException):
    """Error while interacting with a Wolfram Kernel."""
    pass

class WolframEvaluationException(WolframLanguageException):
    """Error after an evaluation raising messages."""
    def __init__(self, error, result=None, messages=[]):
        self.error = error
        self.result = result
        if isinstance(messages, list):
            self.messages = messages
        else:
            self.messages = [messages]

    def __str__(self):
        return self.error

    def __repr__(self):
        return 'WolframEvaluationException<error=%s, expr=%s, messages=%i>:' % (self.error, str_trim(self.result), len(self.messages))

class SocketException(WolframLanguageException):
    """Error while operating on socket."""

    pass

class WolframParserException(WolframLanguageException):
    """Error while deserializing WXF bytes."""
    pass

__all__ = ['WolframLanguageException',
           'RequestException',
           'AuthenticationException',
           'WolframKernelException',
           'SocketException',
           'WolframParserException']
