# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.evaluation.cloud import SecuredAuthenticationKey, UserIDPassword, WolframCloudSession, WolframCloudSessionAsync, WolframServer
from wolframclient.evaluation.result import WolframAPIResponse, WolframEvaluationJSONResponse, WolframKernelEvaluationResult, WolframResult
from wolframclient.utils.six import JYTHON

if JYTHON:
    __all__ = [
        'WolframServer',
        'WolframCloudSession', 'WolframCloudSessionAsync',
        'SecuredAuthenticationKey',
        'UserIDPassword',
        'WolframResult',
        'WolframKernelEvaluationResult',
        'WolframAPIResponse',
        'WolframEvaluationJSONResponse'
        ]
else:
    from wolframclient.evaluation.call import WolframAPICall, WolframCall
    from wolframclient.evaluation.kernel import WolframLanguageSession
    __all__ = [
        'WolframCall', 'WolframAPICall',
        'WolframServer',
        'WolframCloudSession', 'WolframCloudSessionAsync',
        'SecuredAuthenticationKey',
        'UserIDPassword',
        'WolframLanguageSession',
        'WolframResult',
        'WolframKernelEvaluationResult',
        'WolframAPIResponse',
        'WolframEvaluationJSONResponse'
    ]
