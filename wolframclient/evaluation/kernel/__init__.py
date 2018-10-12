# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.kernel.kernelsession import WolframLanguageSession
from wolframclient.evaluation.kernel.futuresession import WolframLanguageFutureSession

__all__ = ['WolframLanguageSession', 'WolframLanguageFutureSession']

try:
    from wolframclient.evaluation.kernel.asyncsession import WolframLanguageAsyncSession
    from wolframclient.evaluation.kernel.kernelpool import WolframKernelPool, parallel_evaluate
    __all__.append(WolframKernelPool.__name__)
    __all__.append(WolframLanguageAsyncSession.__name__)
    __all__.append(parallel_evaluate.__name__)
except Exception as e:
    pass
