from __future__ import absolute_import, print_function, unicode_literals

class RequestException(Exception):
    ''' Error in HTTP request.'''
    __slots__ = 'response'

    def __init__(self, response):
        self.response = response

class AuthenticationException(RequestException):
    ''' Error in authentication request.'''
    pass

XAuthNotConfigured = AuthenticationException(
    'XAuth is not configured. Missing xauth consumer key and/or secret.')


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


class ConfigurationException(Exception):
    ''' Configuration error. '''
    pass

class ConfigurationWarning(Warning):
    ''' Configuration warning, happening when something is likely to be wrong. '''
    pass


