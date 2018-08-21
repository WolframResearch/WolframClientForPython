.. toctree::
   :maxdepth: 4



Introduction to the Wolfram Client Library
*******************************************

The Wolfram Client Library is structured in sub-modules all located in :mod:`wolframclient`:

* :mod:`~wolframclient.evaluation` provides convenient methods to evaluate Wolfram Language expressions directly from Python. There are many ways to evaluate code including: evaluation by a local kernel, direct evaluation by a public or private Wolfram Cloud, calling deployed API.

* :mod:`~wolframclient.serializers` provides serialization methods to various formats such as string :wl:`InputForm` and binary `WXF` format.

* :mod:`~wolframclient.deserializers` contains a parser for `WXF`.

* :mod:`~wolframclient.language` provides Python representation of Wolfram Language symbols and functions.

* :mod:`~wolframclient.exception` regroups the exceptions and errors that the library may raise.


Wolfram Cloud interactions
==============================

Cloud interaction requires to be properly authenticated to a Wolfram Cloud using your **Wolfram ID** and password. To create one visit https://account.wolfram.com/auth/create.

Cloud evaluation
-------------------------------

A one-shot evaluation on the Wolfram public cloud only requires to initiate an authenticated session, e.g. using a Wolfram ID and password::

    from wolframclient.evaluation import UserIDPassword, WolframCloudSession
    userID = UserIDPassword('MyWolframID', 'password')
    session = WolframCloudSession(authentication=userID)
    session.evaluate('Range[3]')

Cloud functions
------------------

From an authenticated cloud session it is possible to build a cloud function, to later uses it with various parameters::

    wl_str_reverse = session.cloud_function('StringReverse')
    wl_str_reverse("hello")
    wl_str_reverse("world.")


Functions may accept more than one input parameters. Define a cloud function that applies :wl:`Join` and call it from Python on multiple lists::

    wl_join = session.cloud_function('Join[##] &')
    wl_join([0,1], ["a", "b"], [2, "c"])

API
---------------

Deploy Wolfram Language API
++++++++++++++++++++++++++++++

From the Wolfram Language it is possible to deploy arbitrary code and expose it through an API.

Using the Wolfram Language, connect to the Wolfram Cloud:

.. code-block :: wl

    CloudConnect["MyWolframID", "myPassword"]

Deploy an API accepting a list and returning the result of the function `MinMax` applied on it:

.. code-block :: wl

    CloudDeploy[
        APIFunction[{"list" -> RepeatingElement[Expression]},
            MinMax[#list] &
        ],
        CloudObject["api/private/minmax"]
    ]

Call API
+++++++++++

Note that the API was deployed without any particular permissions, and as such is a private `CloudObject` only usable by its owner.

Use the previously authenticated session to call the API from Python::

    api = ('MyWolframID', 'api/private/minmax')
    result = session.call(api, {'list' : [[1, 2, 3], [-1, -2, -3]]})
    if result.success:
        print('API call successfully returned:', result.output)
    else:
        print('API call failed:', result.failure)

Use WolframAPICall
------------------

:class:`~wolframclient.evaluation.WolframAPICall` provides a convenient interface to API. The above example becomes::
    
    from wolframclient.evaluation import WolframAPICall

    call = WolframAPICall(session, ('MyWolframID', 'api/private/minmax'))
    call.add_parameter('list', [[1, 2, 3], [-1, -2, -3]])
    result = call.perform()
    result.get()

Some convenient functions deals with specific content type and files. It is particularly useful when dealing with images. Deploy an API that takes an image and returns its dimensions:

.. code-block :: wl

    CloudDeploy[
        APIFunction[{"image" -> "Image"},
            ImageDimensions[#image] &
        ],
        CloudObject["api/private/imagedimensions"]
    ]

Call the API using a PNG file `/path/to/example.png`::

    >>> api_call = WolframAPICall(session, ('MyWolframID', 'api/private/imagedimensions'))
    >>> api_call.add_file_parameter('image', '/path/to/example.png')
    >>> result = api_call.perform().get()
    b'{320, 240}'

Wolfram Language evaluation
==============================

Local kernel
---------------

Wolfram Language session :class:`~wolframclient.evaluation.WolframLanguageSession` is initialized with a *WolframKernel* executable specified by its path. A started session enables local evaluation of Wolfram Language code directly in Python.::

    from wolframclient.evaluation import WolframLanguageSession

    try:
        session = WolframLanguageSession('/path/to/kernel-executable')
        session.start()
        session.evaluate('Range[3]')
    finally:
        session.terminate()

A best practice for using :class:`~wolframclient.evaluation.WolframLanguageSession` is use a try/finally block to explicitly close the session after it is used. Alternatively a `with` can achieve the same result.::

    from wolframclient.evaluation import WolframLanguageSession
    
    with WolframLanguageSession('/path/to/kernel-executable') as session:
        session.evaluate('Range[3]')

.. note ::
    Typical location of the *WolframKernel* executable depends on the operating system. The relative path from your installation directory should be:
    
    * On `MacOS`: `Contents/MacOS/WolframKernel`
    * On `Windows`: `WolframKernel.exe`
    * On Linux: `Files/Executables/WolframKernel`

Wolfram Call
------------------
An other approach is to rely on :class:`~wolframclient.evaluation.WolframCall`. This class abstract away the evaluator (:class:`~wolframclient.evaluation.WolframLanguageSession`, or :class:`~wolframclient.evaluation.WolframCloudSession` and enable smooth transition from local evaluation to cloud evaluation::

    from wolframclient.evaluation import WolframLanguageSession
    from wolframclient.evaluation import WolframCall
    
    with WolframLanguageSession('/path/to/kernel-executable') as session:
        call = WolframCall(session, 'Range[3]')
        result = call.perform()
        result.get()

In the above example the variable `session` can be seamlessly replaced by a cloud session instance.::

    from wolframclient.evaluation import WolframCloudSession
    from wolframclient.evaluation import WolframCall
    
    userID = UserIDPassword('MyWolframID', 'password')
    session = WolframCloudSession(authentication=userID)
    call = WolframCall(session, 'Range[3]')
    result = call.perform()
    result.get()

Serialization
=============

This library is intended to provide a way to serialize python expression to Wolfram Language string `InputForm` and `WXF` string of bytes. The library was designed to be extensible so that any arbitrary Python object can be serialized with the addition of custom encoder(s). The serialization module was tested with three interpreters: 
- Python 2.7, 
- Python 3.6.4, 
- JYTHON.


Serialize
----------

This module provides an high level abstraction to represent and serialize arbitrary Wolfram Language expressions.
The function :func:`~wolframclient.serializers.export` can serialize a variety of standard Python objects, such as :class:`list`, or :class:`dict`, and provides extensible mechanism for custom classes::

    >>> from wolframclient.serializers import export
    >>> export([1,2,3])
    b'{1, 2, 3}'

Wolfram language expressions are conveniently represented using :class:`wolframclient.language.wl`::

    >>> from wolframclient.language import wl
    >>> wl.Quantity(12, "Hours")
    Quantity[<< 2 >>]

Resulting Python objects are serializable to string `InputForm`:: 

    >>> export(wl.Select(wl.PrimeQ, [1,2,3]))
    b'Select[PrimeQ, {1, 2, 3}]'

WXF format is also supported::

    >>> export(wl.Select(wl.PrimeQ, [1,2,3]), target_format='wxf')
    b'8:f\x02s\x06Selects\x06PrimeQf\x03s\x04ListC\x01C\x02C\x03'    

If a string is provided as second argument then the serialized output is directly written to the file::

    >>> export([1, 2, 3], 'file.wl')
    'file.wl'

Any object that implements a :func:`write` method, like :class:`file`, :py:class:`io.BytesIO` or :py:class:`io.StringIO` is a valid `stream` value::

    with open('file.wl', 'wb') as f:
        export([1, 2, 3], f)

Deserialize
-----------

The library can parse a WXF binary input and return Python objects from it.
The function :func:`~wolframclient.deserializers.binary_deserialize` can deserialize any WXF input into standard Python objects and, eventually Numpy arrays. Note that Numpy is not mandatory as long as no numeric array is encountered.

    >>> from wolframclient.serializers import export
    >>> from wolframclient.deserializers import binary_deserialize
    >>> my_list = [1,2,3]
    >>> wxf = export(my_list, target_format='wxf')
    >>> binary_deserialize(wxf)
    [1, 2, 3]

Any object that implements a :func:`read` method, following the buffer specifications is a valid input. 

Export a `dict` using the WXF format::

    >>> export({'key1' : 1, 'key2' : 2}, target_format='wxf', stream='/path/to/file.wxf')
    '/path/to/file.wxf'

Import it as a Python object::

    >>> with open('/path/to/file.wxf') as fp:
    ...     binary_deserialize(fp)
    ...
    {'key1': 1, 'key2': 2}
