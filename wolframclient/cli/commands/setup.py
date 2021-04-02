from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils import six
from wolframclient.utils.decorators import to_tuple


@to_tuple
def dependencies():
    yield ("pytz", "2018.6")
    if not six.JYTHON:
        yield ("numpy", not six.PY2 and "1.15.3" or None)
        yield ("pillow", "7.1.2")
        yield ("requests", "2.20.0")
        yield ("oauthlib", "2.1.0")
        yield ("pyzmq", "17.1.2")
        yield ("pandas", "1.0.4")
        yield ("unittest-xml-reporting", None)
    if not six.PY2:
        yield ("aiohttp", "3.6.2")


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    dependencies = dependencies()
