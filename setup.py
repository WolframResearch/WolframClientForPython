from setuptools import setup, find_packages

dependencies = [
    'numpy',
    'pytz',
    'requests', 
    'oauthlib'
    ]

packages = find_packages(exclude=('wolframclient.tests*',))

author_emails = [
    'riccardod@wolfram.com',
    'dorianb@wolfram.com',
    'contact@wolfram.com'
].sort()

setup(
    name='Wolfram Client for Python',
    version='1.0',
    description='A python library to call Wolfram API.',
    author='Wolfram Research',
    author_email=author_emails,
    packages=packages,
    install_requires=dependencies,
)
