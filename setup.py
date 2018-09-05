# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys

try:
    from setuptools import setup, find_packages
except ImportError:
    sys.stderr.write("""Could not import setuptools or your version
of the package is out of date.

Make sure you have pip and setuptools installed and upgraded and try again:
    $ python -m pip install --upgrade pip setuptools
    $ python setup.py install

""")

setup(
    name = 'wolframclient',
    version = '1.0rc1',
    description = 'A Python library with various tools to interact with the Wolfram Language and the Wolfram Cloud.',
    author = 'Wolfram Research',
    author_email = [
        'dorianb@wolfram.com',
        'riccardod@wolfram.com'
    ],
    include_package_data=True,
    packages=find_packages(),
    test_suite='wolframclient.tests',
    python_requires='>=2.7',
    install_requires = [
        'pip',
        'numpy',
        'pytz',
        'requests',
        'oauthlib',
        'zmq'
    ],
    project_urls={
        'Source code': 'https://github.com/WolframResearch/TODO',
        'Documentation': 'todo',
        'Wolfram Research': 'https://www.wolfram.com'
    }
)
