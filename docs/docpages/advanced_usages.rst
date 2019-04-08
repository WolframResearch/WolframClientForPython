.. toctree::
   :maxdepth: 4

##########################################
Advanced Usage
##########################################

.. _adv-expression-representation:

****************************
Expression Representation
****************************

Basic aspects of expression representation are covered in :ref:`basic usages<ref-expressions>`.

Contexts
========

In the Wolfram Language, symbols live in `contexts <http://reference.wolfram.com/language/tutorial/Contexts.html>`_, which serve as a namespace mechanism. Built-in functions belong to the ``System``` context. User-defined symbols and functions are stored by default in the ``Global``` context.

Wolfram Language expressions are represented in Python using the attributes of the factory :data:`~wolframclient.language.wl`::

    >>> from wolframclient.language import wl
    >>> wl.Range(3)
    Range[3]

The attributes of the factory :data:`~wolframclient.language.wl` do not have a context attached::

    >>> wl.myFunction(1)
    myFunction[1]

The representation of a given expression using its string :wl:`InputForm` is readable but ambiguous since the context is resolved in the kernel during evaluation, and as such different kernels may return different results. See :wl:`$ContextPath`.

On the other hand, in the :wl:`WXF` format, expressions are represented with their full names. Context must be fully specified for all symbols, the exception being the ``System``` context, which can be omitted. As a consequence, in WXF, a symbol with no context is always deserialized as a ``System``` symbol.

The method :meth:`~wolframclient.evaluation.kernel.localsession.WolframLanguageSession.evaluate` accepts two types of input:

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


Use Cases
----------

Create a new function :data:`max` that takes a list of strings and returns the longer ones. The function definition is:

.. code-block :: wl

    max[s:List[__String]] := MaximalBy[s, StringLength]

Set up a new local evaluator session::

    >>> from wolframclient.evaluation import WolframLanguageSession
    >>> from wolframclient.language import wl, Global, wlexpr
    >>> session = WolframLanguageSession()

Most of the time, it is much more convenient to define functions using an :wl:`InputForm` string expression rather than :data:`~wolframclient.language.wl`. Define the function `max` for the current session::

    >>> session.evaluate(wlexpr('max[s : List[__String]] := MaximalBy[s, StringLength]'))

Apply function :data:`Global.max` to a list of strings::

    >>> session.evaluate(Global.max(['hello', 'darkness', 'my', 'old', 'friend']))
    ['darkness']

Trying to evaluate :data:`wl.max`, which is the undefined symbol ``System`max``, leads to an unevaluated expression::

    >>> session.evaluate(wl.max(['hello', 'darkness', 'my', 'old', 'friend']))
    max[['hello', 'darkness', 'my', 'old', 'friend']]


It is important to understand that :meth:`~wolframclient.language.wlexpr` applied to a string is equivalent to evaluating :wl:`ToExpression` on top of the string input, and as such some context inference is performed when evaluating. In the above example, the :data:`max` function's explicit name is ``Global`max``. When the Python object :data:`wl.max` is passed to :meth:`~wolframclient.evaluation.kernel.localsession.WolframLanguageSession.evaluate`, it is serialized to :wl:`WXF` first, which has strict context specification rules; the only context that can be omitted is the ``System``` context. As a consequence, any symbol without context is attached to the ``System``` context; `max` is thus ``System`max``, which is not defined.

Finally, terminate the session::

    >>> session.stop()

.. _adv-local-evaluation:

****************************
Local Kernel Evaluation
****************************

The following sections provide executable demonstrations of the local evaluation features of the client library.

.. note ::
    all examples require a local Wolfram Engine installed in the default location.


Evaluation Methods
======================

Synchronous
-----------

Initialize a session::

    >>> from wolframclient.evaluation import WolframLanguageSession
    >>> from wolframclient.language import wlexpr
    >>> session=WolframLanguageSession()

Expressions involving scoped variables are usually more easily represented with :func:`~wolframclient.language.wlexpr`. Compute an integral::

    >>> session.evaluate(wlexpr('NIntegrate[Sqrt[x^2 + y^2 + z^2], {x, 0, 1}, {y, 0, 1}, {z, 0, 1}]'))
    0.9605920064034617

Messages may be issued during evaluation. By default, the above evaluation methods log error messages with severity `warning`. It usually results in the message being printed out. It is also possible to retrieve both the evaluation result and the messages, wrapped in an instance of :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`, by using :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wrap`::

    >>> eval = session.evaluate_wrap('1/0')
    >>> eval.result
    DirectedInfinity[]

Messages are stored as tuples of two elements, the message name and the formatted message::

    >>> eval.messages
    [('Power::infy', 'Infinite expression Power[0, -1] encountered.')]

Asynchronous
------------

Some computations may take a significant time to finish. The library provides various form of control on evaluations.

Evaluate Future
^^^^^^^^^^^^^^^^

Evaluation methods all have a future-based counterpart::

    >>> from wolframclient.evaluation import WolframLanguageSession
    >>> session = WolframLanguageSession()
    >>> future = session.evaluate_future('Pause[3]; 1+1')

The future object is immediately returned; the computation is done in the background. Return the evaluated expression::

    >>> future.result()
    2

Sometimes a fine control over the maximum duration of an evaluation is required. :wl:`TimeConstrained` ensures that a given evaluation duration is not exceeding a timeout in seconds. When the timeout is reached, the symbol :wl:`$Aborted` is returned.

Wrap an artificially long evaluation to last at most one second::

    >>> long_eval = wl.Pause(10)
    >>> timeconstrained_eval = wl.TimeConstrained(long_eval, 1)

Evaluate the time-constrained expression::

    >>> result = session.evaluate(timeconstrained_eval)
    
Check if the result is :wl:`$Aborted`::

    >>> result.name == '$Aborted'
    True

Terminate the session::

    >>> session.terminate()

Concurrent Future 
^^^^^^^^^^^^^^^^^^

Sometimes, the result is not required immediately. Asynchronous evaluation is a way to start evaluations on a local kernel as a background task, without blocking the main Python execution. Asynchronous evaluation methods are :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_future`, :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wrap_future` and :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wxf_future`. They wrapped the evaluation result into a :class:`~concurrent.futures.Future` object. Result is the one that would be returned by the non-future method (e.g :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_future` returns the result of :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate`).

Evaluate an artificially delayed code (using :wl:`Pause`), and print the time elapsed at each step:  

.. literalinclude:: /examples/python/asynchronous1.py
    :linenos:
    :emphasize-lines: 1,4,7,11

The standard output should display:

.. code-block :: text

    Starting an evaluation delayed by 2 seconds.
    After 0.0069s, the code is running in the background, Python execution continues.
    After 2.02s, result was available. Kernel evaluation returned: 2

Coroutine and Asyncio APIs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:mod:`asyncio` provides high-level concurrent code and asynchronous evaluation using coroutines and the `async`/`await` keywords. Asynchronous evaluation based on :mod:`asyncio` requires an instance of :class:`~wolframclient.evaluation.WolframLanguageAsyncSession`, whose methods are mostly coroutines.

Define a coroutine `delayed_evaluation` that artificially delays evaluation, using an asyncio sleep coroutine. Use this newly created coroutine to evaluate a first expression, wait for the coroutine to finish and evaluate the second:

.. literalinclude:: /examples/python/asynchronous2.py
    :linenos:
    :emphasize-lines: 1-2,6-8,10,11,14,16,21-22

The timer printed in the standard output indicates that the total evaluation took roughly two seconds, which is the expected value for sequential evaluations:

.. code-block :: text

    Starting two tasks sequentially.
    After 2.04s, both evaluations finished returning: [1, 2, 3], 6

When coroutines can be evaluated independently one from each other, it is convenient to run them in parallel. Start two independent coroutines in a concurrent fashion and wait for the result:

.. literalinclude:: /examples/python/asynchronous3.py
    :linenos:
    :emphasize-lines: 14-15, 17-18

The total evaluation took roughly one second, indicating that both delayed coroutines ran in parallel:

.. code-block :: text

    Running two tasks concurrently.
    After 1.03s, both evaluations finished returning: hello, world!

In the examples shown, only one Wolfram kernel was used, which is a single-threaded process. Evaluating two computation-heavy Wolfram Language expressions in parallel will have no impact on performance. This requires more than one kernel, which is exactly what kernel pool was designed for.

Kernel Pool
^^^^^^^^^^^

A :class:`~wolframclient.evaluation.WolframEvaluatorPool` starts up a certain amount of evaluators and dispatches work load to them asynchronously. The pool is usable right after the first one has successfully started. Some may take more time to start and become available after a delay:

.. literalinclude:: /examples/python/asynchronous4.py
    :linenos:
    :emphasize-lines: 2,6,8-12


Evaluation output shows that if more than one evaluator was started, the total time is less than ten seconds:

.. code-block :: text

    Done after 3.04s, using up to 4 kernels.


parallel_evaluate
^^^^^^^^^^^^^^^^^

It is possible to evaluate many expressions at once using :func:`~wolframclient.evaluation.parallel_evaluate`. This method starts a kernel pool and uses it to compute expressions yield from an iterable object. The pool is then terminated.

Import the function::

    >>> from wolframclient.evaluation import parallel_evaluate
    
Build a list of ten delayed :wl:`$ProcessID` expressions, which returns the kernel process identified (`pid`) after one second::
    
    >>> expressions = ['Pause[1]; $ProcessID' for _ in range(10)]

Evaluate in parallel and get back a list of ten `pid` values::

    >>> parallel_evaluate(expressions)
    [72094, 72098, 72095, 72096, 72099, 72097, 72094, 72098, 72095, 72096]

The result varies, but the pattern remains the same---namely, at least one process was started, and each process is eventually used more than once.


Logging
========

Logging is often an important part of an application. The library relies on the standard :mod:`logging` module and exposes various methods to control the level of information logged.

The first level of control is through the logging module itself. The Python library logs at various levels. Set up a basic configuration for the logging module to witness some messages in the standard output:

.. literalinclude:: /examples/python/logging1.py
    :linenos:
    :emphasize-lines: 4-5

The standard output should display:

.. code-block :: text

    INFO:wolframclient.evaluation.kernel.kernelcontroller:Kernel writes commands to socket: <Socket: uri=tcp://127.0.0.1:61335>
    INFO:wolframclient.evaluation.kernel.kernelcontroller:Kernel receives evaluated expressions from socket: <Socket: uri=tcp://127.0.0.1:61336>
    INFO:wolframclient.evaluation.kernel.kernelcontroller:Kernel process started with PID: 54259
    INFO:wolframclient.evaluation.kernel.kernelcontroller:Kernel 54259 is ready. Startup took 1.74 seconds.

It is also possible to log from within the kernel. This feature is disabled by default. When initializing a :class:`~wolframclient.evaluation.WolframLanguageSession`, the parameter `kernel_loglevel` can be specified with one of the following values to activate kernel logging: :class:`logging.DEBUG`, :class:`logging.INFO`, :class:`logging.WARNING`, :class:`logging.ERROR`. 

.. note :: 

    if a :class:`~wolframclient.evaluation.WolframLanguageSession` is initialized with the default `kernel_loglevel` (i.e. :class:`logging.NOTSET`), kernel logging is disable for the session, and it is not possible to activate it afterward.

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
    :emphasize-lines: 5,10-11,13,16,18,19

**********************************************
Extending Serialization: Writing an `Encoder`
**********************************************

Serialization of a Python object involves encoders, which convert an input object into a stream of bytes. The library defines encoders for most built-in Python types and for some core libraries. It stores a mapping between types and encoder implementation. In order to serialize more classes, new encoders must be registered.

An encoder is a function of two arguments, the serializer and an object, associated with a type. The object is guaranteed to be an instance of the associated type.

Register a new encoder for a user-defined class:

.. literalinclude:: /examples/python/encoder1.py
    :linenos:
    :emphasize-lines: 7,11-12, 14, 19

During export, for each object to serialize, the proper encoder is found by inspecting the type hierarchy (field :data:`__mro__`). First, check that an encoder is associated with the object type; if not, repeat with the first parent type until one is found. The default encoder, associated with :data:`object`, is used as the last resort.

Register some encoders for a hierarchy of classes: 

.. literalinclude:: /examples/python/encoder2.py
    :linenos:
    :emphasize-lines: 5,8,11,15,19,24,29,35-36

Note: the encoder for :data:`Animal` is never used, not even for the instance of :data:`Salmon`, because :data:`Fish` has a dedicated encoder, and type :data:`Fish` appears first in the method resolution order of type :data:`Salmon`::
    
    >>> Salmon.__mro__
    (<class '__main__.Salmon'>, <class '__main__.Fish'>, <class '__main__.Animal'>, <class 'object'>)


***********************************************
Extending WXF Parsing: Writing a `WXFConsumer`
***********************************************

Integer Eigenvalues
====================

Use the Wolfram Client Library to access the Wolfram Language algebra functions. Compute the integer :wl:`Eigenvalues` on a Python matrix of integers:

.. literalinclude:: /examples/python/eigenvalues1.py
    :linenos:
    :emphasize-lines: 6-9,13

.. _complex-consumer:

Complex Eigenvalues
====================

Python has the built-in class :class:`complex`. By default, the function :func:`~wolframclient.deserializers.binary_deserialize` deserializes Wolfram Language functions using a generic class :class:`~wolframclient.language.expression.WLFunction` but conveniently provides a way to extend the mapping. Define `ComplexFunctionConsumer`, a subclass of :class:`~wolframclient.deserializers.WXFConsumer` that overrides the method :meth:`~wolframclient.deserializers.WXFConsumer.build_function`. The subclassed method maps :wl:`Complex` to the built-in Python class :class:`complex`.

.. literalinclude:: /examples/python/eigenvalues2.py
    :emphasize-lines: 5-17, 31
    :linenos:


Symbolic Eigenvalues
====================

A Python-Heavy Approach
------------------------

.. |matrix-image| image:: ../examples/svg/matrix.svg
    :alt: 3x3 Matrix. If this image does not display, it might be that your browser does not support the SVG image format.
    :class: vertical-align

.. |eigenvalue-1| image:: ../examples/svg/ev1.svg
    :alt: Eigenvalue of the matrix. If this image does not display, it might be that your browser does not support the SVG image format.
    :class: vertical-align

.. |eigenvalue-2| image:: ../examples/svg/ev2.svg
    :alt: Eigenvalue of the matrix. If this image does not display, it might be that your browser does not support the SVG image format.
    :class: vertical-align

.. |eigenvalue-3| image:: ../examples/svg/ev3.svg
    :alt: Eigenvalue of the matrix. If this image does not display, it might be that your browser does not support the SVG image format.
    :class: vertical-align


Sometimes the resulting expression of an evaluation is a symbolic exact value, which nonetheless could be approximated to a numerical result. The eigenvalues of |matrix-image| are |eigenvalue-1|, |eigenvalue-2| and |eigenvalue-3|.

It is possible to build a subclass of :class:`~wolframclient.deserializers.WXFConsumer` that can convert a subset of all Wolfram Language symbols into pure built-in Python objects. It has to deal with :wl:`Plus` and :wl:`Times` and converts :wl:`Pi` to :class:`math.pi`, :wl:`Rational` to :class:`fractions.Fraction` and :wl:`Complex` to :class:`complex`. It results in a significant code inflation but provides a detailed review of the extension mechanism. However, as will be shown, this is not really necessary:

.. literalinclude:: /examples/python/eigenvalues3.py
    :emphasize-lines: 11-64, 83
    :linenos:


A Wolfram Language Alternative
------------------------------

It is recommended to delegate as much as possible to the Wolfram Language. Instead of implementing a (fragile) counterpart of core functions such as :wl:`Plus` or :wl:`Times`, it is best to compute a numerical result within the kernel. This can be achieved with the function :wl:`N`. Once applied to the eigenvalues, the result becomes a mixture of complex values and reals, which was already dealt with in the :ref:`previous section<complex-consumer>`:

.. literalinclude:: /examples/python/eigenvalues3_alternative.py
    :emphasize-lines: 8-14, 28
    :linenos:
