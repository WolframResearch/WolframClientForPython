.. toctree::
   :maxdepth: 4

##########################################
Advanced usage and code examples
##########################################

.. note ::
    All examples require to set the variable **kernel_path** to the path of a local Wolfram Kernel.

.. _adv-expression-representation:

****************************
Expression representation
****************************

Contexts
========

In the Wolfram Language expressions live in :wl:`Context`, which serves as a namespace mechanism. Built-in functions belong to the ``System``` context. By default the context called ``Global``` is associated to user defined functions and new variables. More information about contexts can be found at `tutorial/Contexts <http://reference.wolfram.com/language/tutorial/Contexts.html>`_.

Wolfram Language expression are conveniently represented in Python using the attributes of the factory :func:`~wolframclient.language.wl`::

    >>> from wolframclient.language import wl
    >>> wl.Range(3)
    Range[3]

This factory :func:`~wolframclient.language.wl` does not specify the symbols' context::

    >>> wl.myFunction(1)
    myFunction[1]

The representation of a given expression using its string :wl:`InputForm` is readable but ambiguous, since the context is resolved in the kernel during evaluation, and as such different kernels may return different results. See :wl:`$ContextPath`.

On the other hand in the :wl:`WXF` format expressions are represented with their full name. Context must be fully specified for all symbols, with exception to ``System``` context that can be omitted. As a consequence, in WXF a symbol with no context is always deserialized as a ``System``` symbol.

The method :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate` accepts two types of input:

* Python strings are treated as a string :wl:`InputForm`, as such, context is resolved during evaluation. User defined functions are most of the time automatically created with the ``Global``` context. 
* Serializable python objects are serialized to WXF before evaluation, as such, context must be explicitly specified, except for ``System``` symbols.

``System``` Context
--------------------

In order to explicitly represent a system symbol in Python, the factory :func:`~wolframclient.language.System` can be used. First import it::

    >>> from wolframclient.language import System

Create a python object representing the built-in function :wl:`Classify`::    

    >>> System.Classify
    System`Classify

``Global``` Context
---------------------

User defined functions and variables are associated to the ``Global``` context by default. The factory :func:`~wolframclient.language.Global` can be used to represent those symbols.

Use case
----------

Create a new function using :wl:`InputForm` evaluation, that takes a list of strings and returns the longer ones. The function Wolfram Language `max` is:

.. code-block :: wl

    max[s:List[__String]] := MaximalBy[s, StringLength]

.. literalinclude:: /examples/python/globalcontext.py
    :linenos:
    :emphasize-lines: 7,13,17

In the code above, a simple replacement of `g.max` by `wl.max` shows that kernel no more unevaluates the input.

.. note :: 
    It is important to understand that :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate` applied to a string is equivalent to evaluating :wl:`ToExpression` on top of the string input, and as such some context inference are performed. In this case the `max` function is actually ``Global`max``. Whereas when the Python object is passed to :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate`, it is serialized to :wl:`WXF` first, which is strict in term of symbol context. The only context that can be omitted is the ``System``` context. As a consequence any symbol without context is attached to ``System``` context, `max` is thus ``System`max`` which is not defined.


.. _adv-local-evaluation:

****************************
Local Kernel evaluation
****************************

The following sections provide executable demonstrations of the local kernel evaluation features of the client library.

Evaluation methods
======================

Synchronous
-----------

First initialize a session::

    from wolframclient.evaluation import WolframLanguageSession

    kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
    session=WolframLanguageSession(kernel_path)
    session.start()

Expressions involving more than one function, are usually evaluate with :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate`. Compute an integral::

    >>> session.evaluate('NIntegrate[Sqrt[x^2 + y^2 + z^2], {x, 0, 1}, {y, 0, 1}, {z, 0, 1}]')
    0.9605920064034617

Messages may be issued during evaluation. By default, the above evaluation methods log the error messages with severity `warning`. It usually results in the message being printed out. It is also possible to retrieve both the evaluation result and the messages, wrapped in an instance of :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`, by using :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wrap`::

    >>> eval = session.evaluate_wrap('1/0')
    >>> eval.result
    DirectedInfinity[]

Messages are stored as tuple of two elements, the message name and the formatted message. 

    >>> eval.messages
    [('Power::infy', 'Infinite expression Infinity encountered.')]

Asynchronous
------------

Some computations may take a significant time to finish, and the result might not be required immediately. Asynchronous evaluation is a way to start evaluations on a local kernel, using a background task, without blocking the python execution. Asynchronous evaluation requires a instance of :class:`~wolframclient.evaluation.WolframLanguageAsyncSession` which contains the same method as its synchronous counterpart, except that returned values are wrapped into :class:`~concurrent.futures.Future` objects.

Evaluate an artificially delayed code (using :wl:`Pause`), and print time elapsed at each step:  

.. literalinclude:: /examples/python/asynchronous1.py
    :linenos:

The standard output should display:

.. code-block :: text

    Starting an evaluation delayed by 2 seconds.
    After 0.0069s, the code is running in the background, Python execution continues.
    After 2.02s, result was available. Kernel evaluation returned: 2

Logging
========

Logging is often an important part of an application. The library relies on the standard :mod:`logging` module, and exposes various methods to control the level of information logged.

The first level of control is through the logging module itself. The python library logs at various levels. Setup a basic configuration for the logging module to witness some messages in the standard output:

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

It's also possible to log from within the kernel. This feature is disabled by default. When initializing a :class:`~wolframclient.evaluation.WolframLanguageSession`, the parameter `kernel_loglevel` can be specified with one of the following values: :class:`logging.DEBUG`, :class:`logging.INFO`, :class:`logging.WARNING`, :class:`logging.ERROR`, to activate kernel logging. 

.. note :: If a WolframLanguageSession is initialized with the default `kernel_loglevel` (i.e: :class:`logging.NOTSET`), kernel logging is disable for the session, and it is not possible to activate it afterward.

From the Wolfram Language it is possible to issue log messages, using one of the following functions, given with their signature:

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

Control the log level both at Python and Kernel level:

.. literalinclude:: /examples/python/logging2.py
    :linenos:
    :emphasize-lines: 6,14-15,17,20,22,23

**********************************************
Extending WXF parsing – Writing a WXFConsumer
**********************************************

Integer Eigenvalues
====================

Use the Wolfram Client library to access the Wolfram Language Algebra functions. Compute the integer :wl:`Eigenvalues` on a Python matrix of integers:

.. literalinclude:: /examples/python/eigenvalues1.py
    :linenos:
    :emphasize-lines: 11-14, 16

.. _complex-consumer:

Complex Eigenvalues
====================
Python has built-in :class:`complex`. By default the function :func:`~wolframclient.deserializers.binary_deserialize` deserializes Wolfram Language functions using a generic class :class:`~wolframclient.language.expression.WLFunction`, but conveniently provides a way to extend the mapping. Define `ComplexFunctionConsumer`, a subclass of :class:`~wolframclient.deserializers.WXFConsumer`, that overrides the method :meth:`~wolframclient.deserializers.WXFConsumer.build_function`. The subclassed method maps :wl:`Complex` to built-in python complex.

.. literalinclude:: /examples/python/eigenvalues2.py
    :emphasize-lines: 10-22, 36
    :linenos:


Symbolic Eigenvalues
====================

A Python heavy approach
------------------------

Sometimes the resulting expression of an evaluation is a symbolic exact value, which nonetheless could be approximated to a numerical result. The eigenvalues of :math:`\begin{pmatrix} \pi & -2 & 0 \\ 1 & \pi & -1 \\ 0 & 2 & \pi \\ \end{pmatrix}` are :math:`\frac{1}{2}(4I+2\pi)`, :math:`\frac{1}{2}(-4I+2\pi)`, and :math:`\pi`.

It is possible to build a subclass of :class:`~wolframclient.deserializers.WXFConsumer` that can convert a subset of all Wolfram Language symbols into pure built-in Python objects. It has to deal with :wl:`Plus` and :wl:`Times`, converts :wl:`Pi` to :class:`math.pi`, :wl:`Rational` to :class:`fractions.Fraction`, and :wl:`Complex` to :class:`complex`. It results in a significant code inflation but provide a detailed review of the extension mechanism. Yet, as we will see it is not really necessary.

.. literalinclude:: /examples/python/eigenvalues3.py
    :emphasize-lines: 15-68, 87
    :linenos:


A Wolfram Language alternative
------------------------------

It is recommended to delegate as much as possible to the Wolfram Language. Instead of implementing a (fragile) counterpart of core functions such as :wl:`Plus` or :wl:`Times`, it is best to compute a numerical result within the kernel. This can be achieved with the function :wl:`N`. Once applied to the eigenvalues, the result becomes a mixture of complex values and reals, which was already dealt with in the :ref:`previous section<complex-consumer>`.

.. literalinclude:: /examples/python/eigenvalues3_alternative.py
    :emphasize-lines: 12-18, 32
    :linenos:
