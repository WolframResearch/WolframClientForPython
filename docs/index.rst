.. toctree::
    :hidden:
    :maxdepth: 4

    docpages/install
    docpages/intro
    docpages/advanced_usages
    docpages/public_api

.. title::
    Wolfram Client Library for Python

.. raw:: html

    <div class='logo index-header'>
        <h1>
            <img alt="" class="align-left" src="_images/python-client-library-logo.png">
            Wolfram Language integration</br>
            <span>for Python</span>
        </h1>
    </div>
    <div>
        <h2> This is pure HTML </h2>
    </div>

.. raw:: html
    
    <h2>Install the library</h2>


.. code-block :: console

    $ pip install wolframclient

.. raw:: html

    <h2>Evaluate code</h2>


Evaluate any Wolfram Language code from Python::

    >>> from wolframclient.evaluation import WolframLanguageSession
    >>> from wolframclient.language import wl, wlexpr
    >>> session = WolframLanguageSession()
    >>> session.evaluate(wlexpr('Range[5]'))
    [1, 2, 3, 4, 5]


Immediately call built-in Wolfram Language functions in Python::

    >>> session.evaluate(wl.MinMax([1, -3, 0, 9, 5]))
    [-3, 9]


Build up Wolfram Language code directly in Python::

    >>> func_squared = wlexpr('#^2 &')
    >>> session.evaluate(wl.Map(func_squared, wl.Range(5)))
    [1, 4, 9, 16, 25]

.. raw:: html

    <h3>Direct support for PIL, Pandas, NumPy libraries</h3>


Create a Pandas :data:`DataFrame`::

    >>> import pandas
    >>> df = pandas.DataFrame({'A': [1, 2], 'B': [11, 12]}, index=['id1', 'id2'])
    >>> df
           A   B
    id1    1  11
    id2    2  12

Apply Wolfram Language function directly on complex object. Sum the values of each column::

    >>> session.evaluate(wl.Total(df))
    {'A': 3, 'B': 23}


.. raw:: html

    <h3>Define native Python functions</h3>

Define Wolfram Language functions as native Python functions:

    >>> str_reverse = session.function(wl.StringReverse)
    >>> str_reverse('abc')
    'cba'

.. raw:: html

    <h3>Represent expressions</h3>

Represent Wolfram Language expressions as Python objects::

    >>> wl.Quantity(12, "Hours")
    Quantity[12, 'Hours']

Use string InputForm::

    >>> wlexpr('f[x_] := x^2')
    (f[x_] := x^2)

.. raw:: html

    <h3>Access the power of Wolfram algorithms:</h3>

[ Data Science ] [ Numerics ] [ Graphs Theory ] ::

    >>> limit = wlexpr('Limit[x Log[x^2], x -> 0]')
    >>> session.evaluate(limit)
    0

.. raw:: html

    <h2>Get immediate access to the world's largest integrated algorithmbase</h2>


.. raw:: html

    <h3>Access the Wolfram knowledgebase</h3>


Get the closest ocean::

    >>> near_ocean = wlexpr('GeoNearest[Entity["Ocean"], Here]')
    >>> session.evaluate(near_ocean)
    [Entity['Ocean', 'AtlanticOcean']]

.. raw:: html

    <h3>Use Wolfram natural language understanding</h3>

Query `Wolfram|Alpha <https://www.wolframalpha.com/>`_ directly in Python::

    >>> session.evaluate(wl.WolframAlpha("number of moons of Saturn", "Result"))
    62

Terminate the session, release all resources::

    >>> session.terminate()
