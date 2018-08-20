.. Wolfram Client Library for Python documentation master file, created by
   sphinx-quickstart on Wed Jun  6 10:45:54 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.



=============================================================
Welcome to Wolfram Client Library for Python's documentation
=============================================================

The Client Library provides seamless Wolfram Language integration in Python.
A list of features provided by the library:

* evaluate arbitrary code on a local kernel
* evaluate arbitrary code on Wolfram cloud, public or private
* call deployed :wl:`APIFunction`
* build Python functions on top of Wolfram Language functions.
* represent arbitrary Wolfram Language code as Python object
* serialize Python object to Wolfram Language string :wl:`InputForm`
* serialize Python object to WXF
* extend serialization to any arbitrary Python class.
* parse WXF.


Table of content:
==================

.. toctree::
    :maxdepth: 4

    intro
    public_api

Installation of the library
============================

The library source code is available in various repositories:

* bundled with Wolfram Desktop 12+,
* in a public repository on `Github`,
* via `pip` (maybe?)

.. note::
    Even though most of the features were built to work on Python 2.7, it is recommended to use the library with Python 3.2 or greater.


Install from Wolfram Desktop
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The library is bundled with Wolfram Desktop in `/path/to/library`. From a terminal evaluate the following commands:

.. code-block:: shell

    $ cd /path/to/library
    $ pip install .


Install from GitHub:
^^^^^^^^^^^^^^^^^^^^^

First clone the repository:

.. code-block:: shell

    $ git clone git://github.com/wolframresearch/pythonclientlibrary


Then install it:

.. code-block:: shell

    $ pip install .


Install with pip:
^^^^^^^^^^^^^^^^^^^

Simply evaluate the following command in a terminal:

.. code-block:: shell

    $ pip install wolframclientlibrary


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

