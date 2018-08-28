.. toctree::
   :maxdepth: 4

##############
Code examples
##############

Wolfram Kernel
--------------

In the following examples, the client library is used to connect to a local kernel and run some Wolfram Language computations.

.. note ::
    All the examples of this section require to set the variable **kernel_path** to the path to a local Wolfram Kernel.


Example 1: Eigenvalues of a matrix of integers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the Wolfram Client library to access the Wolfram Language Algebra functions. This example makes use of :wl:`Eigenvalues` on a matrix of integers:

.. literalinclude:: /examples/python/eigenvalues1.py

Example 2: Eigenvalues and complex numbers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Python has a :class:`complex` type which maps to :wl:`Complex` symbol. By default the function :func:`~wolframclient.deserializers.binary_deserialize` deserializes Wolfram Language functions using a generic class :class:`~wolframclient.language.expression.WLFunction`, but conveniently provides a way to extend the mapping. In this second example, a subclass of :class:`~wolframclient.deserializers.WXFConsumer` is defined, in order to override its method :meth:`~wolframclient.deserializers.WXFConsumer.build_function`. The child method maps :wl:`Complex` to built-in python complex.

.. literalinclude:: /examples/python/eigenvalues2.py

.. note ::
    For readability, this example does not cover the cases of a Complex which arguments are not Python number type, such as **2πi** (`Times[Complex[0, 2], Pi]`), or **1/4 + i** (`Complex[Rational[1,4],1]`).
    A quick way to address this issue is to apply function :wl:`N` to the output, losing exact precision.

