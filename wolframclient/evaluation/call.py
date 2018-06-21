# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.evaluation.kernel import WolframLanguageSession
from wolframclient.evaluation.cloud import WolframCloudSession
from wolframclient.exception import WolframKernelException, AuthenticationException
from wolframclient.language.exceptions import wl
from wolframclient.utils.six import PY3

__all__ = ['WolframCall']


class WolframCall(object):
    """Primary interface to evaluate Wolfram Language code.

    The wolfram call can seamlessly evaluate on various targets:

    * a kernel, when initialized with an instance of :class:`WolframLanguageSession<wolframclient.evaluation.WolframLanguageSession>`
    * a wolfram cloud when initialized with an instance of :class:`WolframCloudSession<wolframclient.evaluation.WolframCloudSession>`

    The expression to evaluate can have various forms, it can be a string expression, or a instance of
    :class:`WLSerializable<wolframclient.serializers.WLSerializable>` in which case it is serialized prior to being evaluated.

    Examples of the similar evaluations on different targets::

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

    When `input` is a file, its content is read and send to the kernel.

    """
    
    def __init__(self, target, input):
        self.target = target
        self.input = input

    def perform(self):
        """Send the input to the specified target for evaluation and return the result."""
        self._normalize_input()
        self._ensure_target_ready()
        return self.target.evaluate(self.input).result()

    def perform_async(self):
        """Asynchronously send the input to the specified target for evaluation and return a future object.
        
        .. warning::
            Asynchronous evaluation is only available for `Python 3.2` and above.

        """
        self._normalize_input()
        self._ensure_target_ready()
        return self.target.evaluate_async(self.input)

    def _normalize_input(self):
        # Normalize input. If it's readable, do it.
        try:
            self.input = self.input.read()
        except AttributeError:
            pass
    
    def _ensure_target_ready(self):
        # ensure session is ready to evaluate expressions.
        if isinstance(self.target, WolframLanguageSession):
            if not self.target.started:
                raise WolframKernelException(
                    'Wolfram language session is not started.')
        elif isinstance(self.target, WolframCloudSession):
            if not self.target.authorized:
                raise AuthenticationException('Cloud session not authorized.')
        else:
            raise ValueError('Unknow target type %s' % input.__class__.__name__)
