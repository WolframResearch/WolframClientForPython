from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils import six
from wolframclient.utils.decorators import to_tuple


@to_tuple
def dependencies():
    yield "pytz"

    if not six.PY2:
        yield "aiohttp"
        yield "numpy"
        yield "oauthlib"
        yield "pandas"
        yield "pillow"
        yield "pyzmq"
        yield "requests"
        yield "unittest-xml-reporting"
        yield "certifi>=2017.4.17"


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    dependencies = dependencies()
