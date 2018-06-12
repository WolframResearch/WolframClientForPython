# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.language.exceptions import WolframLanguageException


class RequestException(Exception):
    ''' Error in HTTP request.'''
    __slots__ = 'response'

    def __init__(self, response, msg=None):
        self.response = response
        self.msg = msg

class AuthenticationException(RequestException):
    ''' Error in authentication request.'''
    pass

class EvaluationException(RequestException):
    ''' Error while evaluating an expression.'''
    pass

__all__ = ['RequestException',
           'AuthenticationException',
           'EvaluationException']
