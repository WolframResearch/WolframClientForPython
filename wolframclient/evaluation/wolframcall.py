# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.evaluation.kernel import WolframLanguageSession
from wolframclient.evaluation.cloud import WolframCloudSession
from wolframclient.exception import WolframKernelException, AuthenticationException
from wolframclient.language.exceptions import wl
from wolframclient.utils.six import PY3

__all__ = ['WolframCall', 'WolframAPICall']


class WolframCall(object):
    """Primary interface to evaluate Wolfram Language code.

    The wolfram call can be evaluated on various targets.
    * a kernel, when `input` is an instance of
    :class:`wolframclient.evaluation.WolframLanguageSession`
    * a wolfram cloud when `input` is an instance of
    :class:`wolframclient.evaluation.WolframCloudSession`

    The expression to evaluate can have various forms, it can
    be a string expression, or a instance of
    :class:`wolframclient.serializers.WLSerializable` in which
    case it is serialized prior to being evaluated.

    Examples::
        >>> with WolframLanguageSession() as wl_session:
        ...     call = WolframCall(wl_session, '1+1')
        ...     call.perform()
        ...
        b'2'

        >>> with WolframCloudSession() as cloud_session:
        ...     call = WolframCall(cloud_session, '1+1')
        ...     call.perform()
        ...
        b'2'

    """
    
    def __init__(self, target, input):
        self.target = target
        self.input = input

    def perform(self):
        # normalize input.
        if isinstance(self.input, (tuple, list)):
            self.input = wl.Construct(*self.input)
        else:
            try:
                self.input = self.input.read()
            except AttributeError:
                pass
        # ensure session is ready to evaluate expressions.
        if isinstance(self.target, WolframLanguageSession):
            if not self.target.started:
                raise WolframKernelException(
                    'Wolfram language session is not started.')
        elif isinstance(self.target, WolframCloudSession):
            if not self.target.authorized:
                raise AuthenticationException('Cloud session not authorized.')
        else:
            raise ValueError('Unknow target type %s' %
                             input.__class__.__name__)

        return self.target.evaluate(self.input).result()

    def perform_async(self):
        return self.target.evaluate_async(self.input)


class WolframAPICall(object):
    def __init__(self, target, api, input):
        if not isinstance(target, WolframCloudSession):
            ValueError('WolframAPICall must be initialized with an WolframCloudSession instance.')
        self.target = target
        self.input = input

    def perform(self):
        self.target.call()
