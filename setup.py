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
    "Programming Language :: Python :: 3"
    "Programming Language :: Python :: 3.5",
    "Programming Language :: Python :: 3.6",
    "Programming Language :: Python :: 3.7",
    "Topic :: Software Development :: Libraries :: Wolfram Language Library"
]


def read(*rellibpath):
    with codecs.open(os.path.join(HERE, *rellibpath), 'r', encoding='utf-8') as fp:
          return fp.read()

def load_tests():
    import unittest
    from wolframclient.cli.commands.test import Command as TestCommand
    TestCommand().handle()

ABOUT = {}
with open(os.path.join(HERE, 'wolframclient', 'about.py'), 'r') as fp:
    exec(fp.read(), ABOUT)


setup(
    name = ABOUT['__name__'],
    version = ABOUT['__version__'],
    description = ABOUT['__description__'],
    long_description = read('README.rst'),
    long_description_content_type = 'text/x-rst',
    keywords=['Wolfram Language', 'Wolfram Desktop', 'Mathematica', 'parser', 'serializer', 'WXF'],
    author = ABOUT['__author__'],
    author_email = ABOUT['__author_email__'],
    include_package_data=True,
    packages=find_packages(),
    test_suite='setup.load_tests',
    python_requires='>=3.5.3',
    install_requires = [
        'pip',
        'numpy',
        'pytz',
        'requests',
        'aiohttp',
        'oauthlib',
        'zmq',
    ],
    project_urls={
        'Source code': 'https://github.com/WolframResearch/WolframClientForPython',
        'Documentation': 'https://wolfr.am/wolframclientdoc',
        'Wolfram Research': 'https://www.wolfram.com',
    },
    entry_points = {
        'wolframclient_serializers_encoder':[]
    }
)
