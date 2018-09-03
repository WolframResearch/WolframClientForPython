# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud import WolframCloudSession, WolframCloudSessionAsync
from wolframclient.utils import six
from wolframclient.evaluation.kernel import WolframLanguageSession
from wolframclient.exception import AuthenticationException, WolframKernelException

__all__ = ['WolframCall', 'WolframAPICall']

class WolframCall(object):
    """Primary interface to evaluate Wolfram Language code.

    The wolfram call can seamlessly evaluate on various targets:

    * a kernel, when initialized with an instance of :class:`~wolframclient.evaluation.WolframLanguageSession`
    * a wolfram cloud when initialized with an instance of :class:`~wolframclient.evaluation.WolframCloudSession`

    The expression to evaluate can have various forms, it can be a string expression, or a instance of
    :class:`~wolframclient.serializers.WLSerializable` in which case it is serialized prior to being evaluated.

    Examples of the similar evaluations on different targets::

        >>> with WolframLanguageSession() as wl_session:
        ...     call = WolframCall(wl_session, '1+1')
        ...     call.perform()
        ...
        b'2'

        >>> cloud_session = WolframCloudSession()
        >>> call = WolframCall(cloud_session, '1+1')
        >>> call.perform()
        b'2'

    When `input` is a file, its content is read and send to the kernel.

    """

    def __init__(self, target, input):
        self.target = target
        self.input = input

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
            raise ValueError('Unknow target type %s' % self.input.__class__.__name__)

    def perform(self):
        """Send the input to the specified target for evaluation and return the result."""
        self._normalize_input()
        self._ensure_target_ready()
        return self.target.evaluate(self.input)

    def perform_wrap(self):
        """Send the input to the specified target for evaluation and return the result as a 
        class holding the result and the evaluation meta-data.
        
        The class instance depends on the target evaluator."""
        self._normalize_input()
        self._ensure_target_ready()
        return self.target.evaluate_wrap(self.input)

    def perform_async(self):
        """Asynchronously send the input to the specified target for evaluation and return a future object.

        .. warning::
            Asynchronous evaluation is only available for `Python 3.2` and above.

        """
        self._normalize_input()
        self._ensure_target_ready()
        return self.target.evaluate_async(self.input)

class WolframAPICall(object):
    """Perform an API call to a given target.

    The API call is actually performed when :func:`~wolframclient.evaluation.WolframAPICall.perform`
    is called.

    Parameters can be added using one of the various functions that this class exposes. They
    can be of many types including: string, files, WL serializable python objects, binary data with arbitrary
    content-type (e.g: *image/png*).
    """

    def __init__(self, target, api, permission_key=None):
        self.target = target
        self.api = api
        self.parameters = {}
        self.files = {}
        self.permission_key = permission_key
        self.multipart = False

    def add_parameter(self, name, value):
        """Add a new API input parameter from a serialization python object."""
        self.parameters[name] = value
        return self
    
    def add_file_parameter(self, name, fp, content_type=None):
        """Add a new API input parameter from a file pointer `fp`"""
        if content_type is None:
            self.files[name] = fp
        else:
            self.files[name] = ('tmp_%s' % name, fp, content_type)
        return self

    def add_binary_parameter(self, name, data, content_type='application/octet-stream'):
        """Add a new API input parameter as a blob of binary data."""
        if not isinstance(data, six.binary_type):
            raise TypeError('Input data by bytes.')
        self.files[name] = ('tmp_%s' % name, data, content_type)
        return self

    def add_image_data_parameter(self, name, image_data, content_type='image/png'):
        """Add a new API image input parameter from binary data.

        If the data in `image_data` does not represent an image in the `PNG` format, the
        optional parameter `content_type` must be set accordingly to the appropriate content
        type.
        e.g: *image/jpeg*, *image/gif*, etc.
        """
        if not isinstance(data, six.binary_type):
            raise TypeError('Input data must by bytes.')
        self.files[name] = ('tmp_image_%s' % name, data, content_type)
        return self

    def perform(self, **kwargs):
        """Make the API call, return the result."""
        return self.target.call(self.api,
                                  input_parameters=self.parameters,
                                  files=self.files,
                                  permissions_key=self.permission_key, **kwargs)

    def __repr__(self):
        return 'WolframAPICall<api=%s>' % (self.api,)

    def __str__(self):
        return repr(self)
        
