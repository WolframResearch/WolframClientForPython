.. toctree::
    :maxdepth: 4

Installation of the Library
============================

The library source code is available in various repositories:

* hosted on *PyPi.org* and available in `pip`
* in a public repository on `GitHub`
* bundled with Wolfram Desktop 12+

.. note::
    even though most of the features were built to work on Python 2.7, it is recommended to use the library with Python 3.5 or greater.

Install with pip
^^^^^^^^^^^^^^^^^^^

Evaluate the following command in a terminal:

.. code-block:: shell

    $ pip install wolframclient

Install from GitHub
^^^^^^^^^^^^^^^^^^^^^

Clone the library's repository:

.. code-block:: shell

    $ git clone git://github.com/wolframresearch/pythonclientlibrary


Install the library:

.. code-block:: shell

    $ pip install .

Install from the Wolfram Desktop
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The library is bundled with the Wolfram Desktop. The library location depends on your OS. Here are the usual locations:

+-----------+-------------------------------------------------------------------------------------------------------------------+
| OS        | Library path                                                                                                      |
+===========+===================================================================================================================+
| *MacOS*   | :file:`/Applications/Wolfram Desktop.app/Contents/SystemFiles/Components/WolframClientForPython`                  |
+-----------+-------------------------------------------------------------------------------------------------------------------+
| *Windows* | :file:`C:\\Program Files\\Wolfram Research\\Wolfram Desktop\\12\\SystemFiles\\Components\\WolframClientForPython` |
+-----------+-------------------------------------------------------------------------------------------------------------------+
| *Linux*   | :file:`/usr/local/Wolfram/Desktop/12/SystemFiles/Components/WolframClientForPython`                               |
+-----------+-------------------------------------------------------------------------------------------------------------------+


From a terminal, evaluate the following commands:

.. code-block:: shell

    $ cd /path/to/library
    $ pip install .
