.. toctree::
   :maxdepth: 4

##########################################
Advanced usage and code examples
##########################################

****************************
Local Kernel evaluation
****************************

The following sections provide executable demonstrations of the local kernel evaluation features of the client library.

.. note ::
    All examples require to set the variable **kernel_path** to the path of a local Wolfram Kernel.

Evaluation methods
======================

Synchronous
-----------

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

Logging
========

Logging is often an important part of an application. The library relies on the standard :mod:`logging` module, and exposes various methods to control the level of information logged.

The first level of control is through the logging module itself. The python library logs at various level:

.. literalinclude:: /examples/python/logging1.py
    :linenos:
    :emphasize-lines: 5-6

It's also possible to log from within the kernel. This feature is disabled by default. When initializing a :class:`~wolframclient.evaluation.WolframLanguageSession`, the parameter `kernel_loglevel` can be specified with one of the following values: :class:`logging.DEBUG`, :class:`logging.INFO`, :class:`logging.WARNING`, :class:`logging.ERROR`, to activate kernel logging. 

.. note :: If a WolframLanguageSession is initialized with the default `kernel_loglevel` (i.e: :class:`logging.NOTSET`), kernel logging is not available, and it is not possible to activate it afterward.

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
    :linenos:
    :emphasize-lines: 14,16,19,21,22

**********************************************
Extending WXF parsing – Writing a WXFConsumer
**********************************************

Integer Eigenvalues
====================

Use the Wolfram Client library to access the Wolfram Language Algebra functions. Compute the integer :wl:`Eigenvalues` on a Python matrix of integers:

.. literalinclude:: /examples/python/eigenvalues1.py
    :linenos:
    :emphasize-lines: 11-15

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

Sometimes the resulting expression of an evaluation is a symbolic exact value, which nonetheless could be approximated to a numerical result. The Eigenvalues of the matrix :math:`\begin{pmatrix} \pi & -2 & 0 \\ 1 & \pi & -1 \\ 0 & 2 & \pi \\ \end{pmatrix}` are :math:`\frac{1}{2}(4I+2\pi)`, :math:`\frac{1}{2}(-4I+2\pi)`, and :math:`\pi`.

It is possible to build a subclass of :class:`~wolframclient.deserializers.WXFConsumer`, that can convert some symbolic results into pure built-in Python objects. The consumer must deal with :wl:`Plus` and :wl:`Times`, converts :wl:`Pi` to :class:`math.pi`, :wl:`Rational` to :class:`fractions.Fraction`, and :wl:`Complex` to :class:`complex`. It results in a significant code inflation but provide a detailed review of the extension mechanism. Yet, as we will see it is not really necessary.

.. literalinclude:: /examples/python/eigenvalues3.py
    :emphasize-lines: 15-68, 86
    :linenos:


A Wolfram Language alternative
------------------------------

It is recommended to delegate as much as possible to the Wolfram Language. Instead of implementing a (fragile) counterpart of core functions such as :wl:`Plus` or :wl:`Times`, it is best to compute a numerical result in the kernel. This is obtained with the function :wl:`N`. Once applied to the eigenvalues, the result becomes a mixture of complex values and reals, which was already dealt with in the :ref:`previous section<complex-consumer>`.

.. literalinclude:: /examples/python/eigenvalues3_alternative.py
    :emphasize-lines: 12-18, 32
    :linenos:
