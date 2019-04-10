.. toctree::
    :hidden:
    :maxdepth: 4

    docpages/install
    docpages/basic_usages
    docpages/advanced_usages
    docpages/public_api

.. title::
    Wolfram Client Library for Python

.. raw:: html

    <div class='document-header bodyaligned'>
        <div class='header-content'>
            <div class='left'>
                <span id='header-title-top' class='header'>
                    Wolfram Client Library
                </span>
                </br>
                <span id='header-title-bottom' class='header'>
                    for Python
                </span>
                </br>
                <span id='header-subtitle' class='header'>
                    Give Python programs access to the power of the Wolfram Language
                </span>
            </div>
            <div class='right'>
                <img alt="" class="align-right" src="_static/wolf-python-homepage.png">
            </div>
        </div>
    </header>
    </div>
    <div class='document-subheader bodyaligned'>
        <span id='header-summary'>
            The Wolfram Client Library for Python lets Python programs directly integrate Wolfram Language capabilities. Connect either to a local Wolfram Engine or to the Wolfram Cloud (or a private Wolfram Cloud).
        </span>
        <br>
    </div>
    <div class='bodyaligned' id='source-code'>
        <a href="https://github.com/WolframResearch/WolframClientForPython"> 
            <svg class='github-fav' viewBox="0 0 16 16" version="1.1" aria-hidden="true"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"></path></svg> 
            Source Code   
        </a>
    </div>



.. raw:: html
    
    <div id='install-lib' class='bodyaligned'>
        <span>Install the library:</span>
    </div>


.. code-block :: console

    $ pip install wolframclient

.. raw:: html

    <div id='setup-session' class='border-top bodyaligned code-label'>
        <span>Set up your Wolfram Language session:</span>
    </div>

.. code-block :: py

    >>> from wolframclient.evaluation import WolframLanguageSession
    >>> from wolframclient.language import wl, wlexpr
    >>> session = WolframLanguageSession()

.. raw:: html
    
    <div class='border-top bodyaligned subsection annotated'>
        <span>Evaluate any Wolfram Language code from Python:</span>
    </div>

.. code-block :: py

    >>> session.evaluate(wlexpr('Range[5]'))
    [1, 2, 3, 4, 5]

.. raw:: html
    
    <div class='border-top bodyaligned subsection annotated'>
        <span>
            Immediately call all 6000+ built-in Wolfram Language functions in Python:
        </span>
    </div>

.. code-block :: py

    >>> session.evaluate(wl.MinMax([1, -3, 0, 9, 5]))
    [-3, 9]

.. raw:: html
    
    <div class='border-top bodyaligned subsection'>
        <span>
            Build up Wolfram Language code directly in Python:
        </span>
    </div>

.. code-block :: py

    >>> func_squared = wlexpr('#^2 &')
    >>> session.evaluate(wl.Map(func_squared, wl.Range(5)))
    [1, 4, 9, 16, 25]

.. raw:: html

    <div class='border-top bodyaligned subsection'>
        <span>
            Direct support for PIL, Pandas and NumPy libraries:
        </span>
    </div>

.. raw:: html
    
    <div  class='bodyaligned code-label'>
        Create a Pandas <span class='pre'>DataFrame</span>:
    </div>

.. code-block :: python

    >>> import pandas
    >>> df = pandas.DataFrame({'A': [1, 2], 'B': [11, 12]}, index=['id1', 'id2'])
    >>> df
           A   B
    id1    1  11
    id2    2  12


.. raw:: html
    
    <div class='bodyaligned code-label'>
        Apply Wolfram Language function directly to the <span class='pre'>DataFrame</span>:
    </div>

.. code-block:: python

    >>> session.evaluate(wl.Total(df))
    {'A': 3, 'B': 23}


.. raw:: html

    <div class='border-top bodyaligned subsection'>
        <span>
            Define native Python functions:
        </span>
    </div>

.. code-block :: py

    >>> str_reverse = session.function(wl.StringReverse)
    >>> str_reverse('abc')
    'cba'


.. raw:: html

    <div class='border-top bodyaligned subsection'>
        <span>
            Represent Wolfram Language expressions as Python objects:
        </span>
    </div>

.. raw:: html
    
    <div class='bodyaligned code-label'>
        Use object representation:
    </div>

.. code-block :: py

    >>> wl.Quantity(12, "Hours")
    Quantity[12, 'Hours']

.. raw:: html
    
    <div class='bodyaligned code-label'>
        Use string representation:
    </div>

.. code-block :: py

    >>> wlexpr('f[x_] := x^2')
    (f[x_] := x^2)


.. raw:: html

    <div id='letsyou-section' class='bodyaligned'>
        <span>
            Within your Python environment, the Wolfram Client Library for Python lets you:
        </span>
    </div>

.. raw:: html

    <div class='border-top bodyaligned subsection'>
        <span>
            Access the power of Wolfram algorithms:
        </span>
    </div>

.. raw:: html
    
    <div class='bodyaligned code-label'>
        Get immediate access to the world's largest integrated algorithmbase:
    </div>

.. code-block :: py

    >>> limit = wlexpr('Limit[x Log[x^2], x -> 0]')
    >>> session.evaluate(limit)
    0
    
.. raw:: html

    <div id='wl-references-list' class='bodyaligned'>
        <ul>
            <li><a class='' href='https://reference.wolfram.com/language/#DataManipulationAndAnalysis'>Data Science</a><span class='inlinebullet'>▪</span></li>
            <li><a class='' href='https://reference.wolfram.com/language/#SymbolicAndNumericComputation'>Numerics</a><span class='inlinebullet'>▪</span></li>
            <li><a class='' href='https://reference.wolfram.com/language/#GraphsAndNetworks'>Graph Theory</a><span class='inlinebullet'>▪</span></li>
            <li><a class='' href='https://reference.wolfram.com/language/'>...</a></li>
        </ul>
    </div>

.. raw:: html

    <div class='border-top bodyaligned subsection'>
        <span>
            Access the Wolfram Knowledgebase:
        </span>
    </div>

.. raw:: html
    
    <div class='bodyaligned code-label'>
        Get the closest ocean:
    </div>

.. code-block:: python

    >>> ocean = wlexpr('GeoNearest[Entity["Ocean"], Here]')
    >>> session.evaluate(ocean)
    [Entity['Ocean', 'AtlanticOcean']]

.. raw:: html

    <div class='border-top bodyaligned subsection'>
        <span>
            Use Wolfram's natural language understanding in Python:
        </span>
    </div>

.. raw:: html
    
    <div class='bodyaligned code-label'>
        Query <a href='https://www.wolframalpha.com/'>Wolfram|Alpha</a> directly in Python:
    </div>

.. code-block:: python

    >>> session.evaluate(wl.WolframAlpha("number of moons of Saturn", "Result"))
    62

.. raw:: html
    
    <div class='bodyaligned code-label'>
        Terminate the session and release all resources:
    </div>

.. code-block:: python

    >>> session.terminate()