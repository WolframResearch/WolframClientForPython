.. toctree::
    :maxdepth: 4

Installation and Prerequisites
******************************

The library source code is available in various repositories:

* hosted on `PyPI <https://pypi.org/project/wolframclient/>`_ and available in `pip`
* in a public repository on `GitHub <https://github.com/WolframResearch/WolframClientForPython>`_
* bundled with Wolfram Language 12+

Prerequisites
===============

    - Python 3.5 or higher
    - Wolfram Language 11.3 or higher
    - Git (optional)


Installation
============================

There are three methods for installing the library.

Install Using `pip` (Recommended)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Recommended for most users. It installs the latest stable version released by Wolfram Research.

Evaluate the following command in a terminal:

.. code-block:: shell

    $ pip install wolframclient

Install Using Git
^^^^^^^^^^^^^^^^^^^^^

Recommended for developers who want to install the library along with the full source code.

Clone the library's repository:

.. code-block:: shell

    $ git clone git://github.com/WolframResearch/WolframClientForPython

Install the library in your site-package directory:

.. code-block:: shell

    $ pip install .

Install from Wolfram Language--Based Products
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Future releases of the Wolfram Language will include a stable version of the client library. The library location depends on your OS. Here are the usual locations:

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
