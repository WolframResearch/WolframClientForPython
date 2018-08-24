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
    version = '0.1',
    description = 'A python library to call Wolfram API and serialize expressions.',
    author = 'Wolfram Research',
    author_email = [
        'dorianb@wolfram.com',
        'riccardod@wolfram.com'
    ],
    packages=find_packages(exclude=['wolframclient.tests*']),
    package_data={'wolframclient.evaluation.kernel': ['initkernel.m']},
    install_requires = [
        'pip',
        'numpy',
        'pytz',
        'requests',
        'oauthlib'
    ],
)
