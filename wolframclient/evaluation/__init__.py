# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud import SecuredAuthenticationKey, UserIDPassword, WolframAPICall, WolframCloudSession, WolframCloudSessionAsync, WolframServer
from wolframclient.evaluation.result import WolframAPIResponse, WolframEvaluationJSONResponse, WolframKernelEvaluationResult, WolframResult
from wolframclient.utils.six import JYTHON

if JYTHON:
    __all__ = [
        'WolframServer', 'WolframCloudSession', 'WolframCloudSessionAsync',
        'WolframAPICall', 'SecuredAuthenticationKey', 'UserIDPassword',
        'WolframResult', 'WolframKernelEvaluationResult', 'WolframAPIResponse',
        'WolframEvaluationJSONResponse'
    ]
else:
    from wolframclient.evaluation.kernel import WolframLanguageSession
    __all__ = [
        'WolframAPICall', 'WolframServer', 'WolframCloudSession',
        'WolframCloudSessionAsync', 'WolframAPICall',
        'SecuredAuthenticationKey', 'UserIDPassword', 'WolframLanguageSession',
        'WolframResult', 'WolframKernelEvaluationResult', 'WolframAPIResponse',
        'WolframEvaluationJSONResponse'
    ]
    try:
        __all__.append('WolframKernelPool')
        __all__.append('WolframLanguageAsyncSession')
    except:
        pass
