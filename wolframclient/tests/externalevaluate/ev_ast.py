from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.externalevaluate import execute_from_string
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_execute_from_string(self):

        context = {}

        result = execute_from_string("a = 2+2", session_data=context)

        self.assertEqual(result, None)
        self.assertEqual(context.get("a", None), 4)

        result = execute_from_string("a", session_data=context)

        self.assertEqual(result, 4)

        result = execute_from_string("z = a + 4\nz", session_data=context)

        self.assertEqual(result, 8)
        self.assertEqual(context.get("a", None), 4)
        self.assertEqual(context.get("z", None), 8)

    def test_context(self):
        session_data = {}
        local_context = {"a": 3}

        result = execute_from_string("a", session_data=session_data, constants=local_context)
        self.assertEqual(result, 3)

        result = execute_from_string(
            "a = 12; a", session_data=session_data, constants=local_context
        )

        self.assertEqual(result, 12)
        self.assertEqual(session_data.get("a", None), 12)

    def test_context_deletion(self):

        session_data = {"a": 3}

        execute_from_string("del a", session_data=session_data)

        with self.assertRaises(KeyError):
            session_data["a"]

    def test_globals(self):
        execute_from_string(
            "import numpy\ndef arange(n): return numpy.arange(n).reshape(n)\n\narange(10)"
        )
