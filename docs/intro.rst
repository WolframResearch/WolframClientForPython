.. toctree::
   :maxdepth: 4



Introduction to the Wolfram Client Library
*******************************************

The Wolfram Client Library is structured in sub-modules all located in :mod:`wolframclient`:

* :mod:`~wolframclient.evaluation` provides convenient methods to evaluate Wolfram Language expressions directly from Python. There are many ways to evaluate code including: evaluation by a local kernel, direct evaluation by a public or private Wolfram Cloud, calling deployed API.

* :mod:`~wolframclient.serializers` provides serialization methods to various formats such as string :wl:`InputForm` and binary :wl:`WXF` format.

* :mod:`~wolframclient.deserializers` contains a parser for :wl:`WXF`.

* :mod:`~wolframclient.language` provides Python representation of Wolfram Language symbols and functions.

* :mod:`~wolframclient.exception` regroups the exceptions and errors that the library may raise.


Wolfram Cloud interactions
==============================

Cloud interaction requires to be properly authenticated to a Wolfram Cloud using your **Wolfram ID** and password. To create one visit https://account.wolfram.com/auth/create. 

.. _ref-auth:

Authenticate
-------------

Begin by importing from the :mod:`~wolframclient.evaluation` module, the classes :class:`~wolframclient.evaluation.UserIDPassword` and :class:`~wolframclient.evaluation.WolframCloudSession`.

    >>> from wolframclient.evaluation import UserIDPassword, WolframCloudSession

Create a new instance of :class:`~wolframclient.evaluation.UserIDPassword` with your Wolfram ID and password::

    >>> userID = UserIDPassword('MyWolframID', 'password')

Using `userID`, start a new authenticated cloud session:: 

    >>> session = WolframCloudSession(authentication=userID)
    >>> session.authorized
    True

In the following sections the authenticated session initialized above is simply referred by its variable name `session`.

Cloud evaluation
-------------------------------

A one-shot evaluation on the Wolfram public cloud requires to initiate an :ref:`authenticated session<ref-auth>`, and call its :meth:`~wolframclient.evaluation.WolframCloudSession.evaluate` method::

    >>> session.evaluate('Range[3]')
    WolframEvaluationJSONResponse<success=True, expression={1, 2, 3}>

Even if the authenticated session is a persistent object, each evaluation occurs independently, similarly to :wl`:CloudEvaluate`. It means that it's not the appropriate tools to work with variables, and functions.

Define a basic function `f`::

    >>> result = session.evaluate('f[x_]:=x+1')
    >>> result.get()
    'Null'

Apply `f` to `1`, but `f` is no more defined, thus getting an unevaluated result::

    >>> result = session.evaluate('f[1]')
    >>> result.get()
    'f[1]'

Cloud functions
------------------

From an :ref:`authenticated session<ref-auth>` it is possible to build a cloud function, to later use it with various parameters. Create a cloud function::

    >>> wl_str_reverse = session.cloud_function('StringReverse')

Apply it to a different strings::

    >>> wl_str_reverse("hello")
    WolframEvaluationJSONResponse<success=True, expression="olleh">

Retrieve the evaluation result with :meth:`~wolframclient.evaluation.WolframResult.get`:

    >>> wl_str_reverse("world.").get()
    '".dlrow"'

Functions may accept more than one input parameters. Define a cloud function that applies :wl:`Join` and call it from Python on multiple lists::

    >>> wl_join = session.cloud_function('Join[##] &')
    >>> wl_join([0,1], ["a", "b"], [2, "c"])
    '{0, 1, "a", "b", 2, "c"}'

API
---------------

.. _ref-deployAPI:

Deploy Wolfram Language API
++++++++++++++++++++++++++++++

From the Wolfram Language, it is possible to deploy arbitrary code and to expose it through an API.

Using the Wolfram Language, connect to the Wolfram Cloud using your Wolfram ID and password:

.. code-block :: wl

    CloudConnect["MyWolframID", "myPassword"]

Create an :wl:`APIFunction` that takes an integer and returns its squared value:

.. code-block :: wl

    api = APIFunction[{"x" -> "Integer"},
        #x^2 &
    ]

.. note::
    By default, :wl:`APIFunction` formats output as string :wl:`InputForm` which is not always suited for interoperability with Python. For better inter-operability JSON is preferable. :wl:`WXF` is an other versatile option.

Deploy the API as a cloud object named `api/private/xsquared`:

.. code-block :: wl

    CloudDeploy[api, CloudObject["api/private/xsquared"]]

The API was deployed with default permissions, and as such is a private :wl:`CloudObject` only usable by its owner.


API call from Python
++++++++++++++++++++++

Once again we need an :ref:`authenticated session<ref-auth>` to call the API from Python. API are specified with 2-value tuple made of the owner ID (your Wolfram ID) and the name of the :wl:`CloudObject` used to deploy::

    >>> api = ('MyWolframID', 'api/private/xsquared')

The API sole input is `"list"`. In general API values are specified as a :class:`dict` where keys are parameters' name. Python :class:`list` are automatically converted to Wolfram Language :wl:`List` and thus are valid API input values. 

Call the API::

    >>> result = session.call(api, {'x' : 4})

Check that the API call succeeded::

    >>> result.success
    True

Get the result as an string::

    >>> result.get()
    b'16'

Parse it as an :class:`int`::

    >>> int(result.get())
    16

Use WolframAPICall
------------------

:class:`~wolframclient.evaluation.WolframAPICall` provides a convenient interface to call API. Using the :ref:`previously deployed API <ref-deployAPI>`, and the :ref:`authenticated session<ref-auth>`, instanciate a new :class:`~wolframclient.evaluation.WolframAPICall`::
    
    >>> from wolframclient.evaluation import WolframAPICall
    >>> call = WolframAPICall(session, ('MyWolframID', 'api/private/xsquared'))

Add an input parameter::

    >>> call.add_parameter('x', 4)
    WolframAPICall<api=('MyWolframID', 'api/private/xsquared')>

Perform the call::

    >>> result = call.perform()

Get the result::

    >>> result.get()
    b'16'

The class :class:`~wolframclient.evaluation.WolframAPICall` exposes some helper functions to deal with specific content type and files. It is particularly useful when using image inputs. 

Deploy an API that takes an image and returns its :wl:`ImageDimensions` as a JSON array:

.. code-block :: wl

    CloudDeploy[
        APIFunction[{"image" -> "Image"},
            ImageDimensions[#image] &,
            "JSON"
        ],
        CloudObject["api/private/imagedimensions"]
    ]

Create a :class:`~wolframclient.evaluation.WolframAPICall` targeting the new API::

    >>> api_call = WolframAPICall(session, ('MyWolframID', 'api/private/imagedimensions'))

Add a new file parameter. File parameters have a name and their values must be an opened file object as returned by :func:`open`. Call the API using a image stored in `/path/to/example/image.png`::

    >>> with open('/path/to/example/image.png', 'rb') as fp:
    ...     api_call.add_file_parameter('image', fp)
    ...     result = api_call.perform()
    ...
    WolframAPICall<api=('dorianb', 'api/private/imagedimensions')>

.. note ::
    It's important to make the call while the file object is opened, i.e. inside the `with` statement.

Parse the JSON API response::

    >>> import json
    >>> json.loads(result.get())
    [320, 240]

Wolfram Language evaluation
==============================

.. _ref-localkernel:

Local kernel
---------------

Wolfram Language session :class:`~wolframclient.evaluation.WolframLanguageSession` is initialized with a *WolframKernel* executable specified by its path. A session enables local evaluation of Wolfram Language code directly in Python.

.. note ::
    Typical location of the *WolframKernel* executable depends on the operating system. The relative path from your installation directory should be:
    
    * On `MacOS`: `Contents/MacOS/WolframKernel`
    * On `Windows`: `WolframKernel.exe`
    * On Linux: `Files/Executables/WolframKernel`

    It is advised to first try to execute the WolframKernel executable once from your terminal.

Import :class:`~wolframclient.evaluation.WolframLanguageSession`::
    
    >>> from wolframclient.evaluation import WolframLanguageSession

:class:`~wolframclient.evaluation.WolframLanguageSession` must be terminated either: inside a `try/finally` block, by explicitly closing the session after it is used, or, alternatively, in a `with` block that achieves the same result automatically::

    >>> with WolframLanguageSession('/path/to/kernel-executable') as session:
    ...     session.evaluate('Range[3]')
    ...
    WolframKernelEvaluationResult<success=True, result=cba>

Wolfram Call
------------------

An other approach is to rely on :class:`~wolframclient.evaluation.WolframCall`. This class abstracts away the evaluator type (:class:`~wolframclient.evaluation.WolframLanguageSession` or :class:`~wolframclient.evaluation.WolframCloudSession`) and enables seamless transitions from local evaluation to cloud evaluation.

First import :class:`~wolframclient.evaluation.WolframLanguageSession` and :class:`~wolframclient.evaluation.WolframCall`::

    >>> from wolframclient.evaluation import WolframLanguageSession
    >>> from wolframclient.evaluation import WolframCall
    
Using an :ref:`initialized Wolfram language session<ref-localkernel>`, it is possible to instanciate a new :class:`~wolframclient.evaluation.WolframCall` instance, and perform an evaluation::

    >>> with WolframLanguageSession('/path/to/kernel-executable') as session:
    ...     call = WolframCall(session, 'StringReverse["abc"]')
    ...     result = call.perform()
    ...     result.get()
    ...
    'cba'

In the above example the variable `session` can be seamlessly replaced by an :ref:`initialized cloud session<ref-auth>`::

    >>> from wolframclient.evaluation import WolframCloudSession
    >>> from wolframclient.evaluation import WolframCall

    >>> userID = UserIDPassword('MyWolframID', 'password')
    >>> session = WolframCloudSession(authentication=userID)
    >>> call = WolframCall(session, 'StringReverse["abc"]')
    >>> result = call.perform()
    >>> result.get()
    '"cba"'

Serialization
=============

This library is intended to provide a way to serialize python expression to Wolfram Language string :wl:`InputForm` and :wl:`WXF` string of bytes. The functionality was designed to be extensible, so that any arbitrary Python object can be serialized with the addition of custom encoders. The serialization module was tested with three interpreters:

- Python 2.7, 
- Python 3.6.4, 
- JYTHON.

Serialize
----------

The modules :mod:`~wolframclient.serializers` and :mod:`~wolframclient.language` provide tools to represent and serialize arbitrary Wolfram Language expressions.
The function :func:`~wolframclient.serializers.export` can serialize a variety of standard Python objects, such as :class:`list`, or :class:`dict`.

Import the function::

    >>> from wolframclient.serializers import export

Serialize a Python list of integer into an Wolfram Language :wl:`InputForm` string representation::

    >>> export([1,2,3])
    b'{1, 2, 3}'

Wolfram language expressions are conveniently represented using :class:`~wolframclient.language.wl`.

Import the function::
    
    >>> from wolframclient.language import wl

Build a Python object representing a :wl:`Quantity`::

    >>> wl.Quantity(12, "Hours")
    Quantity[<< 2 >>]

The :func:`~wolframclient.serializers.export` function can serialize Python objects built with :class:`~wolframclient.language.wl`::

    >>> export(wl.Quantity(12, "Hours"))
    b'Quantity[12, "Hours"]'

Expressions can be nested, and mixed with serializable Python types::

    >>> export(wl.Select(wl.PrimeQ, [1,2,3]))
    b'Select[PrimeQ, {1, 2, 3}]'

The :wl:`WXF` format is also supported. It is a binary format, thus not always human readable, but is the most efficient way to exchange Wolfram Language expressions. Specify a `target_format` argument to serialize the previous expression to WXF::

    >>> export(wl.Select(wl.PrimeQ, [1,2,3]), target_format='wxf')
    b'8:f\x02s\x06Selects\x06PrimeQf\x03s\x04ListC\x01C\x02C\x03'

If a string is provided as second argument, the serialized output is directly written to the file.

Represent a Wolfram Language :wl:`ListPlot` of the first 25 :wl:`Prime` numbers::

    wl_expr = wl.ListPlot(wl.Prime(wl.Range(25)))

Serialize it to :wl:`WXF` and print the resulting bytes to the file `/path/to/file.wxf`::

    >>> export(wl_expr, '/path/to/file.wxf', target_format='wxf')
    '/path/to/file.wxf'

Using the Wolfram Desktop, import the file:

.. code-block:: wl

    Import["/path/to/file.wxf"]

.. image :: examples/svg/listplotPrime25.svg
    :align: center
    :alt: ListPlot graphic. If this image does not display, it might be that your browser does not support the SVG image format.

The library also provides extensible serialization mechanism for custom Python classes. Refer to the :ref:`API guide page<extensible-serialization>` detailed explanations and to the :doc:`examples page<examples>` for some use cases.

Deserialize
-----------

The library can parse :wl:`WXF` binary inputs and return Python objects from it.
The function :func:`~wolframclient.deserializers.binary_deserialize` can deserialize any :wl:`WXF` input into standard Python objects and, eventually `NumPy <http://www.numpy.org/>`_ arrays. Note that the `NumPy <http://www.numpy.org/>`_ library is not mandatory so long as no numeric array is encountered.

Export a Python list of integers to :wl:`WXF`::

    >>> from wolframclient.serializers import export
    >>> my_list = [1,2,3]
    >>> wxf = export(my_list, target_format='wxf')

Import the :func:`~wolframclient.deserializers.binary_deserialize` function::

    >>> from wolframclient.deserializers import binary_deserialize

Deserialize the bytes::

    >>> binary_deserialize(wxf)
    [1, 2, 3]

Export a `dict` using the WXF format::

    >>> export({'name' : 'Alice', 'age' : 37}, target_format='wxf', stream='/path/to/file.wxf')
    '/path/to/file.wxf'

Import it as a Python object::

    >>> with open('/path/to/file.wxf', 'rb') as fp:
    ...     binary_deserialize(fp)
    ...
    {'key1': 1, 'key2': 2}

.. note ::
    Make sure to :func:`open` WXF files in binary mode **'b'** to avoid encoding issues.