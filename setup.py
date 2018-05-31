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
    
dependencies = [
    'pip',
    'numpy',
    'pytz',
    'requests',
    'oauthlib'
]

packages = find_packages(exclude=('wolframclient.tests*',))

author_emails = [
    'contact@wolfram.com',
    'dorianb@wolfram.com',
    'riccardod@wolfram.com'
]

setup(
    name='Wolfram Client for Python',
    version='1.0',
    description='A python library to call Wolfram API.',
    author='Wolfram Research',
    author_email=author_emails,
    packages=packages,
    install_requires=dependencies,
)
