.. toctree::
   :maxdepth: 4

##########################################
Advanced Usage and Code Examples
##########################################

.. note ::
    all examples require the setting of the variable **kernel_path** to the path of a local Wolfram kernel.

.. _adv-expression-representation:

****************************
Expression Representation
****************************

Contexts
========

In the Wolfram Language, symbols live in :wl:`Context`, which serves as a namespace mechanism. Built-in functions belong to the ``System``` context. By default, the context called ``Global``` is associated to user-defined functions and new variables. More information about contexts can be found at `tutorial/Contexts <http://reference.wolfram.com/language/tutorial/Contexts.html>`_.

Wolfram Language expressions are conveniently represented in Python using the attributes of the factory :data:`~wolframclient.language.wl`::

    >>> from wolframclient.language import wl
    >>> wl.Range(3)
    Range[3]

These attributes of the factory :data:`~wolframclient.language.wl` do not have a context attached::

    >>> wl.myFunction(1)
    myFunction[1]

The representation of a given expression using its string :wl:`InputForm` is readable but ambiguous, since the context is resolved in the kernel during evaluation, and as such different kernels may return different results. See :wl:`$ContextPath`.

On the other hand, in the :wl:`WXF` format, expressions are represented with their full names. Context must be fully specified for all symbols, the exception being the ``System``` context, which can be omitted. As a consequence, in WXF a symbol with no context is always deserialized as a ``System``` symbol.

The method :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate` accepts two types of input:

* Python strings are treated as a string :wl:`InputForm`; as such, context is resolved during evaluation. User-defined functions are most of the time automatically created with the ``Global``` context. 
* Serializable Python objects are serialized to WXF before evaluation; as such, context must be explicitly specified, except for ``System``` symbols.

System Context
--------------------

In order to explicitly represent a system symbol in Python, the factory :data:`~wolframclient.language.System` can be used. First import it::

    >>> from wolframclient.language import System

Create a Python object representing the built-in function :wl:`Classify`::    

    >>> System.Classify
    System`Classify

Global Context
---------------------

User-defined functions and variables are associated to the ``Global``` context by default. The factory :data:`~wolframclient.language.Global` can be used to represent those symbols::

    >>> from wolframclient.language import Global
    >>> Global.f
    Global`f

Arbitrary Contexts
------------------

The factory :data:`~wolframclient.language.wl` can be used to build symbols with arbitrary context, as well as subcontexts::

    >>> wl.Developer.PackedArrayQ
    Developer`PackedArrayQ

    >>> wl.Global.f
    Global`f

    >>> wl.System.Predict
    System`Predict

    >>> wl.MyContext.MySubContext.myFunction
    MyContext`MySubContext`myFunction


Use Case
----------

Create a new function using :wl:`InputForm` evaluation that takes a list of strings and returns the longer ones. The Wolfram Language function `max` is:

.. code-block :: wl

    max[s:List[__String]] := MaximalBy[s, StringLength]

.. literalinclude:: /examples/python/globalcontext.py
    :linenos:
    :emphasize-lines: 7,13,17

In that code, a simple replacement of `g.max` by `wl.max` shows that kernel no longer evaluates the input.

.. note :: 
    it is important to understand that :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate` applied to a string is equivalent to evaluating :wl:`ToExpression` on top of the string input, and as such some context inference is performed. In this case the `max` function is actually ``Global`max``, whereas when the Python object is passed to :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate`, it is serialized to :wl:`WXF` first, which is strict in terms of symbol context. The only context that can be omitted is the ``System``` context. As a consequence, any symbol without context is attached to the ``System``` context; `max` is thus ``System`max``, which is not defined.


.. _adv-local-evaluation:

****************************
Local Kernel Evaluation
****************************

The following sections provide executable demonstrations of the local kernel evaluation features of the client library.

Evaluation Methods
======================

Synchronous
-----------

First initialize a session::

    from wolframclient.evaluation import WolframLanguageSession

    kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
    session=WolframLanguageSession(kernel_path)
    session.start()

Expressions involving more than one function are usually evaluated with :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate`. Compute an integral::

    >>> session.evaluate('NIntegrate[Sqrt[x^2 + y^2 + z^2], {x, 0, 1}, {y, 0, 1}, {z, 0, 1}]')
    0.9605920064034617

Messages may be issued during evaluation. By default, the above evaluation methods log error messages with severity `warning`. It usually results in the message being printed out. It is also possible to retrieve both the evaluation result and the messages, wrapped in an instance of :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`, by using :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wrap`::

    >>> eval = session.evaluate_wrap('1/0')
    >>> eval.result
    DirectedInfinity[]

Messages are stored as tuples of two elements; the message name and the formatted message. 

    >>> eval.messages
    [('Power::infy', 'Infinite expression Infinity encountered.')]

Asynchronous
------------

Concurrent Future API
^^^^^^^^^^^^^^^^^^^^^

Some computations may take a significant time to finish, and the result might not be required immediately. Asynchronous evaluation is a way to start evaluations on a local kernel using a background task, without blocking the main Python execution. Asynchronous evaluation requires an instance of :class:`~wolframclient.evaluation.WolframLanguageFutureSession` which contains the same method as its synchronous counterpart, except that returned values are wrapped into :class:`~concurrent.futures.Future` objects.

Evaluate an artificially delayed code (using :wl:`Pause`), and print the time elapsed at each step:  

.. literalinclude:: /examples/python/asynchronous1.py
    :linenos:
    :emphasize-lines: 2,8,11,15

The standard output should display:

.. code-block :: text

    Starting an evaluation delayed by 2 seconds.
    After 0.0069s, the code is running in the background, Python execution continues.
    After 2.02s, result was available. Kernel evaluation returned: 2

Coroutine and Asyncio APIs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :mod:`asyncio` provides high-level concurrent code and asynchronous evaluation using coroutines and the `async`/`await` keywords. Asynchronous evaluation based on :mod:`asyncio` requires an instance of :class:`~wolframclient.evaluation.WolframLanguageAsyncSession`, which methods are mostly coroutines.

Define a coroutine `delayed_evaluation` that artificially delays evaluation, using an asyncio sleep coroutine. Use this newly created coroutine to evaluate a first expression, wait for the coroutine to finish and evaluate the second:

.. literalinclude:: /examples/python/asynchronous2.py
    :linenos:
    :emphasize-lines: 2-3,10-12,14,15,18,20,25-26

The timer printed in the standard output indicates that the total evaluation took roughly two seconds, which is the expected value for sequential evaluations:

.. code-block :: text

    Starting two tasks sequentially.
    After 2.04s, both evaluations finished returning: [1, 2, 3], 6

When coroutines can be evaluated independently one from each other, it is convenient to run them in parallel. Start two independent coroutines in a concurrent fashion, and wait for the result:

.. literalinclude:: /examples/python/asynchronous3.py
    :linenos:
    :emphasize-lines: 18-19,21-22

The total evaluation took roughly one second, indicating that both delayed coroutines ran in parallel:

.. code-block :: text

    Running two tasks concurrently.
    After 1.03s, both evaluations finished returning: hello, world!

In the examples shown, only one Wolfram kernel was used, which is a single-threaded process. Evaluating two computation-heavy Wolfram Language expressions in parallel will have no impact on performances. This requires more than one kernel, which is exactly what kernel pool was designed for.

Kernel Pool
^^^^^^^^^^^

A :class:`~wolframclient.evaluation.WolframEvaluatorPool` starts up a certain amount of evaluators and dispatch work load on them asynchronously. The pool is usable right after the first one has successfully started. Some may take more time to start and become available after a delay.

.. literalinclude:: /examples/python/asynchronous4.py
    :linenos:
    :emphasize-lines: 3,10,12-16


Evaluation output shows that if more than one evaluator was started, the total time is less than ten seconds:

.. code-block :: text

    Done after 3.04s, using up to 4 kernels.


parallel_evaluate
^^^^^^^^^^^^^^^^^

It is possible to evaluate many expressions at once using :func:`~wolframclient.evaluation.parallel_evaluate`. This method starts a kernel pool and uses it to compute expressions yield from an iterable object. The pool is then terminated.

Import the function::

    >>> from wolframclient.evaluation import parallel_evaluate

Specify the path of a target kernel::

    >>> kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
    
Build a list of ten delayed :wl:`$ProcessID`, which returns the kernel process identified (`pid`) after one second::
    
    >>> expressions = ['Pause[1]; $ProcessID' for _ in range(10)]

Evaluate in parallel and get back a list of ten values `pid`::

    >>> parallel_evaluate(kernel_path, expressions)
    [72094, 72098, 72095, 72096, 72099, 72097, 72094, 72098, 72095, 72096]

The result varies but the pattern remains the same – namely, at least one process was started, and each process is eventually used more than once.


Logging
========

Logging is often an important part of an application. The library relies on the standard :mod:`logging` module, and exposes various methods to control the level of information logged.

The first level of control is through the logging module itself. The Python library logs at various levels. Set up a basic configuration for the logging module to witness some messages in the standard output:

.. literalinclude:: /examples/python/logging1.py
    :linenos:
    :emphasize-lines: 5-6

The standard output should display:

.. code-block :: text

    INFO:wolframclient.evaluation.kernel.kernelsession:Kernel receives commands from socket: <Socket: host=127.0.0.1, port=63471>
    INFO:wolframclient.evaluation.kernel.kernelsession:Kernel writes evaluated expressions to socket: <Socket: host=127.0.0.1, port=63472>
    INFO:wolframclient.evaluation.kernel.kernelsession:Kernel process started with PID: 37169
    INFO:wolframclient.evaluation.kernel.kernelsession:Kernel is ready. Startup took 1.54 seconds.
    WARNING:wolframclient.evaluation.kernel.kernelsession:Infinite expression Infinity encountered.

It is also possible to log from within the kernel. This feature is disabled by default. When initializing a :class:`~wolframclient.evaluation.WolframLanguageSession`, the parameter `kernel_loglevel` can be specified with one of the following values to activate kernel logging: :class:`logging.DEBUG`, :class:`logging.INFO`, :class:`logging.WARNING`, :class:`logging.ERROR`. 

.. note :: 

    if a WolframLanguageSession is initialized with the default `kernel_loglevel` (i.e: :class:`logging.NOTSET`), kernel logging is disable for the session, and it is not possible to activate it afterward.

From the Wolfram Language, it is possible to issue log messages using one of the following functions, given with its signature:

.. code-block :: wl

    (* Sends a log message to Python with a given log level *)
    ClientLibrary`debug[args__]
    ClientLibrary`info[args__]
    ClientLibrary`warn[args__]
    ClientLibrary`error[args__]

The log level of the kernel is independent of the Python one. The following functions can be used to restrict the amount of log data sent by the kernel:

.. code-block :: wl

    (* Sends only messages of a given level and above *)
    ClientLibrary`SetDebugLogLevel[]
    ClientLibrary`SetInfoLogLevel[]
    ClientLibrary`SetWarnLogLevel[]
    ClientLibrary`SetErrorLogLevel[]
    (* Sends no message at all *)
    ClientLibrary`DisableKernelLogging[]

Control the log level at both the Python and the kernel levels:

.. literalinclude:: /examples/python/logging2.py
    :linenos:
    :emphasize-lines: 6,14-15,17,20,22,23

**********************************************
Extending serialization: Writing an Encoder
**********************************************

Serialization of Python object involves encoders, which convert an input object into a stream of bytes. The library defines encoders for most built-in Python types and for some core libraries. It stores a mapping between types and encoder implementation. In order to serialize more classes, new encoders must be registered.

An encoder is a function of two arguments, the serializer and an object, associated with a type. The object is guaranteed to be an instance of the associated type.

Register a new encoder for a user defined class:

.. literalinclude:: /examples/python/encoder1.py
    :linenos:
    :emphasize-lines: 8,12-13, 15, 20

During export, for each object to serialize, the proper encoder is found by inspecting the type hierarchy (field :data:`__mro__`). First, check that an encoder is associated with the object type, if not, repeat with the first parent type until one is found. The default encoder, associated with :data:`object`, is used in last resort.

Register some encoders for a hierarchy of classes: 

.. literalinclude:: /examples/python/encoder2.py
    :linenos:
    :emphasize-lines: 8,11,14,18,22,27,32,38,39

Note: the encoder for :data:`Animal` is never used, not even for the instance of :data:`Salmon`, because :data:`Fish` has a dedicated encoder, and type :data:`Fish` appears first in the method resolution order of type :data:`Salmon`::
    
    >>> Salmon.__mro__
    (<class '__main__.Salmon'>, <class '__main__.Fish'>, <class '__main__.Animal'>, <class 'object'>)


**********************************************
Extending WXF Parsing: Writing a WXFConsumer
**********************************************

Integer Eigenvalues
====================

Use the Wolfram Client Library to access the Wolfram Language algebra functions. Compute the integer :wl:`Eigenvalues` on a Python matrix of integers:

.. literalinclude:: /examples/python/eigenvalues1.py
    :linenos:
    :emphasize-lines: 11-14, 16

.. _complex-consumer:

Complex Eigenvalues
====================

Python has built-in :class:`complex`. By default, the function :func:`~wolframclient.deserializers.binary_deserialize` deserializes Wolfram Language functions using a generic class :class:`~wolframclient.language.expression.WLFunction`, but conveniently provides a way to extend the mapping. Define `ComplexFunctionConsumer`, a subclass of :class:`~wolframclient.deserializers.WXFConsumer` that overrides the method :meth:`~wolframclient.deserializers.WXFConsumer.build_function`. The subclassed method maps :wl:`Complex` to the built-in Python complex.

.. literalinclude:: /examples/python/eigenvalues2.py
    :emphasize-lines: 10-22, 36
    :linenos:


Symbolic Eigenvalues
====================

A Python-Heavy Approach
------------------------

Sometimes the resulting expression of an evaluation is a symbolic exact value, which nonetheless could be approximated to a numerical result. The eigenvalues of :math:`\begin{pmatrix} \pi & -2 & 0 \\ 1 & \pi & -1 \\ 0 & 2 & \pi \\ \end{pmatrix}` are :math:`\frac{1}{2}(4I+2\pi)`, :math:`\frac{1}{2}(-4I+2\pi)` and :math:`\pi`.

It is possible to build a subclass of :class:`~wolframclient.deserializers.WXFConsumer` that can convert a subset of all Wolfram Language symbols into pure built-in Python objects. It has to deal with :wl:`Plus` and :wl:`Times`, converts :wl:`Pi` to :class:`math.pi`, :wl:`Rational` to :class:`fractions.Fraction` and :wl:`Complex` to :class:`complex`. It results in a significant code inflation but provide a detailed review of the extension mechanism. However, as will be shown, this is not really necessary.

.. literalinclude:: /examples/python/eigenvalues3.py
    :emphasize-lines: 15-68, 87
    :linenos:


A Wolfram Language Alternative
------------------------------

It is recommended to delegate as much as possible to the Wolfram Language. Instead of implementing a (fragile) counterpart of core functions such as :wl:`Plus` or :wl:`Times`, it is best to compute a numerical result within the kernel. This can be achieved with the function :wl:`N`. Once applied to the eigenvalues the result becomes a mixture of complex values and reals, which was already dealt with in the :ref:`previous section<complex-consumer>`.

.. literalinclude:: /examples/python/eigenvalues3_alternative.py
    :emphasize-lines: 12-18, 32
    :linenos:
