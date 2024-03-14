from __future__ import absolute_import, print_function, unicode_literals

from threading import Thread

import zmq
from wolframclient.language import wl
from wolframclient.serializers import export
from wolframclient.utils.externalevaluate import start_zmq_loop
from wolframclient.utils.tests import TestCase as BaseTestCase


from wolframclient.utils.externalevaluate import ExternalEvaluateConsumer
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils.functional import composition, is_iterable, iterate, map

def create_evaluation_command(string, context = {}, hook_symbol = wl.ExternalEvaluate.Private.ExternalEvaluateCommand):
    return hook_symbol('Eval', (string, context))

class TestCase(BaseTestCase):

    starting_port = 9000

    def compare(self, string_version, result):
        self.assertEqual(string_version, export(result, target_format="wxf"))

    def _test_using_loop(self, messages, port):

        hook = wl.ExternalEvaluate.Private.ExternalEvaluateCommand

        def threaded_function(port=port, message_limit=len(messages)):
            start_zmq_loop(port=port, message_limit=message_limit)

        thread = Thread(target=threaded_function)
        thread.start()

        client = zmq.Context().socket(zmq.PAIR)
        client.connect("tcp://127.0.0.1:%s" % port)

        for message, context, result in messages:

            client.send(export(create_evaluation_command(message, context), target_format="wxf"))

            msg = client.recv()

            self.compare(msg, result)


    def test_variable_assign(self):
        self._test_using_loop(
            (
                ("a = 2", {}, wl.Null),
                ("a", {}, 2),
            ),
            12345
        )

    def test_eval_with_context(self):
        self._test_using_loop(
            (
                ("a = 3", {}, wl.Null),
                ("a", {'a': 2}, 2),
            ),
            12346
        )

    def test_eval_with_delete(self):
        self._test_using_loop(
            (
                ("a = 3", {}, wl.Null),
                ("del a", {}, wl.Null),
                ("'a' in globals()", {}, False),
            ),
            12347
        )

