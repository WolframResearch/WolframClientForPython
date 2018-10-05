# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys
import os
# used for the sake of Python2 and 3 compatibility.
import codecs

try:
    from setuptools import setup, find_packages
except ImportError:
    sys.stderr.write("""Could not import setuptools or your version
of the package is out of date.

Make sure you have pip and setuptools installed and upgraded and try again:
    $ python -m pip install --upgrade pip setuptools
    $ python setup.py install

""")

HERE = os.path.abspath(os.path.dirname(__file__))

CLASSIFIERS = [
    "License :: OSI Approved :: MIT License",
    "Programming Language :: Python",
    "Programming Language :: Python :: 2",
    "Programming Language :: Python :: 2.7",
    "Programming Language :: Python :: 3"
    "Programming Language :: Python :: 3.4",
    "Programming Language :: Python :: 3.5",
    "Programming Language :: Python :: 3.6",
    "Programming Language :: Python :: 3.7",
    "Topic :: Software Development :: Libraries :: Wolfram Language Library"
]


def read(*rellibpath):
    with codecs.open(os.path.join(HERE, *rellibpath), 'r', encoding='utf-8') as fp:
          return fp.read()


setup(
    name = 'wolframclient',
    version = '1.0.1.dev5',
    description = 'A Python library with various tools to interact with the Wolfram Language and the Wolfram Cloud.',
    long_description = read('README.rst'),
    long_description_content_type = 'text/x-rst',
    keywords=['Wolfram Language', 'Wolfram Desktop', 'Mathematica', 'parser', 'serializer', 'WXF'],
    author = 'Wolfram Research',
    author_email = 'dorianb@wolfram.com, riccardod@wolfram.com',
    include_package_data=True,
    packages=find_packages(),
    test_suite='wolframclient.tests',
    python_requires='>=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*',
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
        'Documentation': 'https://www.wolframcloud.com/objects/dorianb/lcl/python/doc/intro.html',
        'Wolfram Research': 'https://www.wolfram.com'
    }
)
