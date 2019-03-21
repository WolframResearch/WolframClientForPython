# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from threading import Thread

import zmq

from wolframclient.language import wl
from wolframclient.serializers import export
from wolframclient.utils.externalevaluate import (EXPORT_KWARGS, StdoutProxy,
                                                  start_zmq_loop)
from wolframclient.utils.tests import TestCase as BaseTestCase

STRING = "foo"
STRING_NEWLINE = "abc\nABC"
STRING_MULTILINE = """first
second
third
"""


class TestCase(BaseTestCase):
    def compare(self, string_version, result):
        self.assertEqual(string_version, export(result, **EXPORT_KWARGS))

    def test_zmq_loop(self):

        port = 9000

        messages = [("a = 2", wl.Null), ("a", 2)]

        def threaded_function(port=port, message_limit=len(messages)):
            start_zmq_loop(
                port=port, message_limit=message_limit, write_to_stdout=False)

        thread = Thread(target=threaded_function)
        thread.start()

        client = zmq.Context().socket(zmq.PAIR)
        client.connect('tcp://127.0.0.1:%s' % port)

        for message, result in messages:

            client.send(export({'input': message}, target_format='wxf'))

            msg = client.recv()

            self.compare(msg, result)

    def test_stdout_proxy(self):

        output = []

        class TestStdoutProxy(StdoutProxy):
            def send_lines(self, *lines):
                #this custom class makes us test that strings are sent correctly
                #without dealing with Print and exported code
                output.extend(lines)

        proxy = TestStdoutProxy(None)
        proxy.write(STRING)

        self.assertEqual(output, [])

        proxy.write("\n")

        self.assertEqual(output, ['foo'])

        proxy.write(STRING)

        self.assertEqual(output, ['foo'])

        proxy.write(STRING_NEWLINE)

        self.assertEqual(output, ['foo', 'fooabc'])

        proxy.write(STRING_NEWLINE)

        self.assertEqual(output, ['foo', 'fooabc', 'ABCabc'])

        proxy.write(STRING_MULTILINE)

        self.assertEqual(
            output, ['foo', 'fooabc', 'ABCabc', 'ABCfirst', 'second', 'third'])

        proxy.write(STRING_NEWLINE)

        self.assertEqual(
            output,
            ['foo', 'fooabc', 'ABCabc', 'ABCfirst', 'second', 'third', 'abc'])

        proxy.flush()

        self.assertEqual(output, [
            'foo', 'fooabc', 'ABCabc', 'ABCfirst', 'second', 'third', 'abc',
            'ABC'
        ])
