.. toctree::
   :maxdepth: 4

##########################################
Advanced usage and code examples
##########################################

Local Kernel evaluation
----------------------------

The following sections cover the client library features in details.

.. note ::
    All the examples of this section require to set the variable **kernel_path** to the path to a local Wolfram Kernel.
    The variable `session` refers to an initialized session.


Evaluation methods
^^^^^^^^^^^^^^^^^^^^^^

First initialize a session::

    from wolframclient.evaluation import WolframLanguageSession

    kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
    session=WolframLanguageSession(kernel_path)
    session.start()

There are many ways to evaluation Wolfram Language expressions using a local kernel. The most concise form is suited for simple evaluation usually involving only one function::
    
    >>> session.Range(3)
    [1, 2, 3]

Yet it grants access to powerful functionality, such as built-in classifiers::

    >>> session.LanguageIdentify("Que dis-je, c'est un cap ?")
    Entity[Language, French]

When expression involve more than one function, it's usually best to use :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate`. Compute an integral::

    >>> session.evaluate('NIntegrate[Sqrt[x^2 + y^2 + z^2], {x, 0, 1}, {y, 0, 1}, {z, 0, 1}]')
    0.9605920064034617

Messages may be issued during evaluation. By default, the above evaluation methods log the error messages with severity `WARNING`. It usually results in the message being printed out. It is also possible to retrieve both the evaluation result and the messages, wrapped in an instance of :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`, by using :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wrap`::

    >>> eval = session.evaluate_wrap('1/0')
    >>> eval.result
    DirectedInfinity[]

Messages are stored as tuple of two elements, the message name and the formatted message. 

    >>> eval.messages
    [('Power::infy', 'Infinite expression Infinity encountered.')]

Logging
^^^^^^^^

Logging is often an important part of any application. The library relies on the standard :mod:`logging` module, and exposes various methods to control the level of information logged.

The first level of control is through the logging module itself. The python library logs at various level:

.. literalinclude:: /examples/python/logging1.py

It's also possible to log from within the kernel. This feature is disabled by default. When initializing a :class:`~wolframclient.evaluation.WolframLanguageSession`, the parameter `kernel_loglevel` can be specified with one of the following values: logging.DEBUG, logging.INFO, logging.WARNING, logging.ERROR, to activate kernel logging. If kernel logging is not activated when initializing the session, it is not possible to activate it afterward.

From the Wolfram Language it is possible to issue log messages, using one of the following functions:

.. code-block :: wl

    (* Sends a log message to Python with a given log level *)
    ClientLibrary`debug[msg__String]
    ClientLibrary`info[msg__String]
    ClientLibrary`warn[msg__String]
    ClientLibrary`error[msg__String]

The log level of the kernel is independent of the Python one. The following functions can be used to restrict the amount of log data sent by the kernel:

.. code-block :: wl

    (* Sends only messages of a given level and above *)
    ClientLibrary`SetDebugLogLevel[]
    ClientLibrary`SetInfoLogLevel[]
    ClientLibrary`SetWarnLogLevel[]
    ClientLibrary`SetErrorLogLevel[]
    (* Sends no message at all *)
    ClientLibrary`DisableKernelLogging[]

Manipulation of log level using Python and Kernel controls:

.. literalinclude:: /examples/python/logging2.py


Eigenvalues of a matrix of integers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the Wolfram Client library to access the Wolfram Language Algebra functions. This example makes use of :wl:`Eigenvalues` on a matrix of integers:

.. literalinclude:: /examples/python/eigenvalues1.py

Eigenvalues and complex numbers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Python has a :class:`complex` type which maps to :wl:`Complex` symbol. By default the function :func:`~wolframclient.deserializers.binary_deserialize` deserializes Wolfram Language functions using a generic class :class:`~wolframclient.language.expression.WLFunction`, but conveniently provides a way to extend the mapping. In this second example, a subclass of :class:`~wolframclient.deserializers.WXFConsumer` is defined, in order to override its method :meth:`~wolframclient.deserializers.WXFConsumer.build_function`. The child method maps :wl:`Complex` to built-in python complex.

.. literalinclude:: /examples/python/eigenvalues2.py

.. note ::
    For readability, this example does not cover the cases of a Complex which arguments are not Python number type, such as **2πi** (`Times[Complex[0, 2], Pi]`), or **1/4 + i** (`Complex[Rational[1,4],1]`).
    A quick way to address this issue is to apply function :wl:`N` to the output, losing exact precision.

Example 3: NumPy