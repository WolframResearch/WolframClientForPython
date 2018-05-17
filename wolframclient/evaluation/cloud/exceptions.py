# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

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
    ''' Error while calling /evaluations API.'''
    pass

XAuthNotConfigured = AuthenticationException(
    'XAuth is not configured. Missing xauth consumer key and/or secret.')

class InputException(ValueError):
    ''' Error in input parameters or encoding'''
    pass

class OutputException(ValueError):
    ''' Error in output parameters or decoding'''
    pass
class EncoderException(Exception):
    ''' Error while encoding data.'''
    def __init__(self, msg, cause=None):
        super(EncoderException, self).__init__(msg)
        self.cause = cause

class DecoderException(Exception):
    ''' Error while decoding data.'''
    def __init__(self, msg, cause=None):
        super(DecoderException, self).__init__(msg)
        self.cause = cause