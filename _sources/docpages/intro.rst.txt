.. toctree::
   :maxdepth: 4



Introduction to the Wolfram Client Library
*******************************************

The Wolfram Client Library is structured in submodules all located in :mod:`wolframclient`:

* :mod:`~wolframclient.evaluation` provides convenient methods to evaluate Wolfram Language expressions directly from Python. There are many ways to evaluate code including: evaluation by a local kernel, direct evaluation by a public or private Wolfram Cloud, calling a deployed API.

* :mod:`~wolframclient.serializers` provides serialization methods to various formats such as string :wl:`InputForm` and binary :wl:`WXF` format.

* :mod:`~wolframclient.deserializers` contains a parser for :wl:`WXF`.

* :mod:`~wolframclient.language` provides a Python representation of Wolfram Language symbols and functions.

* :mod:`~wolframclient.exception` regroups the exceptions and errors that the library may raise.


.. _ref-expressions:

Wolfram Language Expression Representation
==========================================

The library exposes many kinds of interactions with the Wolfram Language, many of which require representation of Wolfram Language expressions as Python objects. A straightforward way to construct Python objects representing expressions is to call attributes of :func:`~wolframclient.language.wl`.

Import the factory::

    >>> from wolframclient.language import wl

Represent the Wolfram Language symbol :wl:`Now`::

    >>> wl.Now
    Now

More functions are represented in a similar fashion::

    >>> wl.Select(wl.PrimeQ, wl.Range(5))
    Select[PrimeQ, Range[5]]

Option are defined using named parameters. :wl:`ArrayPad` accepts option :wl:`Padding`::

    >>> wl.ArrayPad([[0]], 1, Padding=1)
    ArrayPad[[[0]], 1, Rule[Padding, 1]]

.. note :: 
    for more details about the Python representation of Wolfram Language expressions, refer to :ref:`the advanced usage section<adv-expression-representation>`.

Wolfram Language Evaluation
==============================

.. _ref-localkernel:

Local Kernel
---------------

The Wolfram Language session :class:`~wolframclient.evaluation.WolframLanguageSession` is initialized with a *WolframKernel* executable specified by its path. A session enables local evaluation of Wolfram Language code directly from Python.

.. note ::
    the typical location of the *WolframKernel* executable depends on the operating system. The relative path from your installation directory should be:
    
    * On `MacOS`: `Contents/MacOS/WolframKernel`
    * On `Windows`: `WolframKernel.exe`
    * On Linux: `Files/Executables/WolframKernel`

    **It is advised to first try to run the WolframKernel executable once from your terminal.**

Initialization
++++++++++++++

Import :class:`~wolframclient.evaluation.WolframLanguageSession`::
    
    >>> from wolframclient.evaluation import WolframLanguageSession

Create a new session targeting a local *WolframKernel* specified by its path::

    >>> session = WolframLanguageSession('/path/to/kernel-executable')

Note that sessions are also automatically started when the first evaluation occurs.

Expression Evaluation
++++++++++++++++++++++++++

Functions are conveniently represented using :func:`~wolframclient.language.wl`. First import it::

    >>> from wolframclient.language import wl

Evaluate a Wolfram Language function from Python using :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate`::

    >>> session.evaluate(wl.StringReverse('abc'))
    'cba'

.. code-block :: wl

    StringReverse["abc"]


Call the Wolfram Language function :wl:`MinMax` on a Python :class:`list`::

    >>> session.evaluate(wl.MinMax([1, 5, -3, 9]))
    [-3, 9]

.. code-block :: wl

    MinMax[{1, 5, -3, 9}]

Query `WolframAlpha <https://www.wolframalpha.com/>`_ for the distance between the Earth and the Sun using the function :wl:`WolframAlpha`::

    >>> distance = session.evaluate(wl.WolframAlpha("Earth distance from Sun", "Result"))
    distance = Quantity[1.008045994315923, AstronomicalUnit]

.. code-block :: wl

    WolframAlpha["Earth distance from Sun", "Result"]

The Python object stored in the `distance` variable is a Wolfram Language :wl:`Quantity`. Convert the unit to kilometers, looping back the previous result in a new expression evaluation::

    >>> d_km = session.evaluate(wl.UnitConvert(distance, "Kilometers"))
    Quantity[150801534.3173264, Kilometers]

.. code-block :: wl

    dkm = UnitConvert[distance, "Kilometers"]

Finally, retrieve the result as a Python number::

    >>> session.evaluate(wl.QuantityMagnitude(d_km))
    150801534.3173264

.. code-block :: wl
    
    QuantityMagnitude[dkm]

Associations are represented as Python dictionaries and vice versa:

    >>> session.evaluate(wl.AssociationMap(wl.Prime, [1, 3, 5]))
    {1: 2, 3: 5, 5: 11}

.. code-block :: wl

    AssociationMap[Prime, {1, 3, 5}]

Options
+++++++++

Wolfram Language options are passed as Python named arguments (a.k.a. `**kwargs`). As seen previously, :wl:`ArrayPad` accepts an option :wl:`Padding` to specify what padding to use. Pad an array with ones::

    >>> session.evaluate(wl.ArrayPad([[0]], 1, Padding=1))
    [[1, 1, 1], [1, 0, 1], [1, 1, 1]]

.. code-block :: wl

    ArrayPad[{{0}}, 1, Padding->1]

InputForm String Evaluate
+++++++++++++++++++++++++

It is sometimes simpler to input Wolfram Language code as :wl:`InputForm` strings, and let the kernel apply :wl:`ToExpression` to it. Compute the squares of an array of integers::

    >>> session.evaluate('Map[#^2 &, Range[5]]')
    [1, 4, 9, 16, 25]

The library provides a function :func:`~wolframclient.language.wlexpr` to help mix :wl:`InputForm` strings and objects. This is particularly useful to define pure functions. Evaluate an alternative representation of the previous expression::

    >>> from wolframclient.language import wlexpr
    >>> session.evaluate(wl.Map(wlexpr('#^2&'), wl.Range(5)))

Persistence
+++++++++++

Expressions evaluated in a given session are persistent. Define a function, and call it::
    
    >>> session.evaluate('f[x_] := x ^ 2')
    Null
    >>> session.evaluate('f[4]')
    16

Create Python Function
++++++++++++++++++++++

From a Wolfram Language expression, it is possible to create a Python function that directly evaluates when called using :func:`~wolframclient.evaluation.WolframLanguageSession.function`::

    >>> str_reverse = session.function('StringReverse')
    >>> str_reverse('abc', 'def', 'ghi')
    'cba'

Define a Wolfram Language function that takes a list or a sequence of integers and only returns the primes::

    >>> session.evaluate('selectPrimes[integers : List[__Integer]] := Select[integers, PrimeQ]')
    >>> session.evaluate('selectPrimes[integers___Integer] := selectPrimes[{integers}]')
    
Create a Python function from it::

    >>> selectPrimes = session.function('selectPrimes')

Apply the function to a list::

    >>> selectPrimes([1,2,3,4])
    [2, 3]

Apply the function to a sequence of values::

    >>> selectPrimes(2, 3, 4, 5)
    [2, 3, 5]

It also works with an iterator of integers::

    >>> selectPrimes(range(100, 120))
    [101, 103, 107, 109, 113]

The session is no more useful, so terminate it::

    >>> session.terminate()

Session management
+++++++++++++++++++++

:class:`~wolframclient.evaluation.WolframLanguageSession` must be terminated, either by explicitly calling :func:`~wolframclient.evaluation.WolframLanguageSession.terminate` or, preferably, using it in a `with` block that achieves the same result automatically. 

It is highly recommended to initialize a session once and for all to mitigate the initialization cost.

Delegate the handling of the lifecycle of a session using a `with` block::

    >>> with WolframLanguageSession('/path/to/kernel-executable') as wl_session:
    ...     wl_session.StringReverse('abc')
    ...
    'cba'

Alternatively, start a session manually::

    >>> session = WolframLanguageSession('/path/to/kernel-executable')
    >>> session.start()

This is not required, since this operation is automatically performed during the first evaluation. Ensure the session started successfully:

    >>> session.started
    True

Manually terminate the session::

    >>> session.terminate()

.. note::
    nonterminated sessions usually result in orphan kernel processes, which ultimately lead to the inability to spawn any usable instance at all. Typically, this ends up with a WolframKernelException raised after a failure to communicate with the kernel.

.. note :: 
    for in-depth explanations and use cases of local evaluation, refer to :ref:`the advanced usage section<adv-local-evaluation>`.

Wolfram Cloud Interactions
==============================

Most Cloud interactions require proper authentication to the Wolfram Cloud. Authentication from the library is done with a secured authentication key. Secured authentication keys are attached to a Wolfram account. To create one, visit https://account.wolfram.com/auth/create.

.. _ref-auth:

Authenticate
----------------

Generate a Secured Authentication Key
+++++++++++++++++++++++++++++++++++++

Using a Wolfram Desktop, or a the Wolfram Public Cloud, generate a new authentication key called `pythonclientlibrary`:

.. code-block :: wl

    sak = GenerateSecuredAuthenticationKey["pythonclientlibrary"]

Get the key and secret as strings:

.. code-block :: wl

    sak["ConsumerKey"]
    sak["ConsumerSecret"]

Start authenticated cloud session
++++++++++++++++++++++++++++++++++

Begin by importing the classes :class:`~wolframclient.evaluation.SecuredAuthenticationKey` and :class:`~wolframclient.evaluation.WolframCloudSession` from the :mod:`~wolframclient.evaluation` module.

    >>> from wolframclient.evaluation import SecuredAuthenticationKey, WolframCloudSession

Create a new instance of :class:`~wolframclient.evaluation.SecuredAuthenticationKey` with the consumer key and secret strings::

    >>> sak = SecuredAuthenticationKey('my consumer key', 'my consumer secret')

Using `sak`, start a new authenticated cloud session:: 

    >>> session = WolframCloudSession(authentication=sak)
    >>> session.authorized
    True

In the following sections, the authenticated session initialized here is simply referred to by its variable name `session`.

Cloud Evaluation
-------------------------------

One-Shot Evaluation
++++++++++++++++++++++

A one-shot evaluation on the Wolfram Public Cloud requires the initiation of an :ref:`authenticated session<ref-auth>`. 

First import the :ref:`expression factory<ref-expressions>`::

    >>> from wolframclient.language import wl

Using an authenticated session, call a function::

    >>> session.evaluate(wl.Range(3))
    '{1, 2, 3}'

    >>> session.evaluate(wl.StringReverse('abc'))
    '"cba"'

Complex expressions are evaluated with the :meth:`~wolframclient.evaluation.WolframCloudSession.evaluate` method. Return the first five `wl`:Prime` numbers::

    >>> session.evaluate('Map[Prime, Range[5]]')
    '{2, 3, 5, 7, 11}'

Even if the authenticated session is a persistent object, each evaluation occurs independently, similarly to :wl:`CloudEvaluate`. This means that this is not the appropriate tool to work with variables and functions.

Define a function `f`::

    >>> result = session.evaluate('f[x_]:=x+1')
    'Null'

Apply `f` to `1`. However, `f` is no longer defined, thus returning an unevaluated result::

    >>> result = session.evaluate('f[1]')
    'f[1]'

Cloud Functions
------------------

From an :ref:`authenticated session<ref-auth>`, it is possible to build a cloud function to later use it with various parameters. Create a cloud function::

    >>> wl_str_reverse = session.function('StringReverse')

Apply it to a first string::

    >>> wl_str_reverse("hello")
    '"olleh"'

Use the function again with a new argument::

    >>> wl_str_reverse("world.")
    '".dlrow"'

Functions may accept more than one input parameter. Define a cloud function that applies :wl:`Join` on all arguments it is given. Join multiple Python arrays::

    >>> wl_join = session.function('Join[##] &')
    >>> wl_join([0,1], ["a", "b"], [2, "c"])
    '{0, 1, "a", "b", 2, "c"}'

API
---------------

.. _ref-deployAPI:

Deploy a Wolfram Language API
++++++++++++++++++++++++++++++

From the Wolfram Language, it is possible to deploy arbitrary code and expose it through an API.

Using the Wolfram Language, connect to the Wolfram Cloud using your Wolfram ID and password:

.. code-block :: wl

    CloudConnect["MyWolframID", "myPassword"]

Create an :wl:`APIFunction` that takes an integer and returns its squared value:

.. code-block :: wl

    api = APIFunction[{"x" -> "Integer"},
        #x^2 &
    ]

.. note::
    By default, :wl:`APIFunction` formats output as string :wl:`InputForm`, which is not always suited for interoperability with Python. For better interoperability, JSON is preferable. :wl:`WXF` is another versatile option.

Deploy the API as a cloud object named `api/private/xsquared`:

.. code-block :: wl

    CloudDeploy[api, CloudObject["api/private/xsquared"]]

The API was deployed with default permissions, and as such is a private :wl:`CloudObject` only usable by its owner. Provide read access to the newly created secured authentication key called `pythonclientlibrary`:

.. code-block :: wl

    SetPermissions[CloudObject["api/private/xsquared"], SecuredAuthenticationKeys["pythonclientlibrary"] -> {"Read", "Execute"}]


API Call from Python
++++++++++++++++++++++

Once again, an :ref:`authenticated session<ref-auth>` is needed to call the API from Python. APIs are specified with a two-value tuple made up of the owner ID (your Wolfram ID) and the name of the :wl:`CloudObject` used to deploy::

    >>> api = ('MyWolframID', 'api/private/xsquared')

The API's sole input is "`list`". In general, an API value is specified as a :class:`dict`, in which keys are parameters names. A :class:`list` in Python is automatically converted to a Wolfram Language :wl:`List` and is thus a valid API input values. 

Call the API::

    >>> result = session.call(api, {'x' : 4})

Check that the API call succeeded::

    >>> result.success
    True

Get the result as a string::

    >>> result.get()
    b'16'

Parse it as an :class:`int`::

    >>> int(result.get())
    16

Use WolframAPICall
------------------

:class:`~wolframclient.evaluation.WolframAPICall` provides a convenient interface to call an API. Using the :ref:`previously deployed API <ref-deployAPI>` and the :ref:`authenticated session<ref-auth>`, initiate a new :class:`~wolframclient.evaluation.WolframAPICall`::
    
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

The class :class:`~wolframclient.evaluation.WolframAPICall` exposes some helper functions to deal with specific content types and files. It is particularly useful when using image inputs. 

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

Add a new file parameter. File parameters have a name, and their values must be an opened file object as returned by :func:`open`. Call the API using an image stored in `/path/to/example/image.png`::

    >>> with open('/path/to/example/image.png', 'rb') as fp:
    ...     api_call.add_file_parameter('image', fp)
    ...     result = api_call.perform()
    ...
    WolframAPICall<api=('dorianb', 'api/private/imagedimensions')>

.. note ::
    it is important to make the call while the file object is opened, i.e. inside the `with` statement.

Parse the JSON API response::

    >>> import json
    >>> json.loads(result.get())
    [320, 240]


Serialization
=============

This library is intended to provide a way to serialize Python expressions to Wolfram Language :wl:`InputForm` strings and :wl:`WXF` strings of bytes. The functionality was designed to be extensible, so that any arbitrary Python object can be serialized with the addition of custom encoders.

Serialize
----------

The modules :mod:`~wolframclient.serializers` and :mod:`~wolframclient.language` provide tools to represent and serialize arbitrary Wolfram Language expressions.
The function :func:`~wolframclient.serializers.export` can serialize a variety of standard Python objects, such as :class:`list` or :class:`dict`.

Import the function::

    >>> from wolframclient.serializers import export

Serialize a Python list of integers into a Wolfram Language :wl:`InputForm` string representation::

    >>> export([1,2,3])
    b'{1, 2, 3}'

Wolfram Language expressions are conveniently represented using :class:`~wolframclient.language.wl`.

Import the function::
    
    >>> from wolframclient.language import wl

Build a Python object representing a :wl:`Quantity`::

    >>> wl.Quantity(12, "Hours")
    Quantity[<< 2 >>]

The :func:`~wolframclient.serializers.export` function can serialize Python objects built with :class:`~wolframclient.language.wl`::

    >>> export(wl.Quantity(12, "Hours"))
    b'Quantity[12, "Hours"]'

Expressions can be nested and mixed with serializable Python types::

    >>> export(wl.Select(wl.PrimeQ, [1,2,3]))
    b'Select[PrimeQ, {1, 2, 3}]'

The :wl:`WXF` format is also supported. It is a binary format, thus not always human readable, but it is the most efficient way to exchange Wolfram Language expressions. Specify a `target_format` argument to serialize the previous expression to WXF::

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

.. image :: ../examples/svg/listplotPrime25.svg
    :align: center
    :alt: ListPlot graphic. If this image does not display, it might be that your browser does not support the SVG image format.

The library also provides extensible serialization mechanisms for custom Python classes. Refer to the :ref:`API guide page<extensible-serialization>` for detailed explanations and to the :doc:`examples page<advanced_usages>` for some use cases.

Deserialize
-----------

The library can parse :wl:`WXF` binary inputs and return Python objects from it.
The function :func:`~wolframclient.deserializers.binary_deserialize` can deserialize any :wl:`WXF` input into standard Python objects and, eventually, `NumPy <http://www.numpy.org/>`_ arrays. Note that the `NumPy <http://www.numpy.org/>`_ library is not mandatory so long as no numeric array is encountered.

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
    make sure to :func:`open` WXF files in binary mode **'b'** to avoid encoding issues.