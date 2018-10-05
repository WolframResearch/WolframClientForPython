# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.kernel.kernelsession import WolframLanguageSession
from wolframclient.evaluation.kernel.futuresession import WolframLanguageFutureSession

__all__ = ['WolframLanguageSession', 'WolframLanguageFutureSession']

try:
    __all__.append('WolframLanguageAsyncSession')
    __all__.append('WolframKernelPool')
except Exception as e:
    pass
