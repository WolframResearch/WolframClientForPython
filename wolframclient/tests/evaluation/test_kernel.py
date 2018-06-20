# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import logging
import unittest
from wolframclient.language import wl
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils import six
if not six.JYTHON:
    from wolframclient.evaluation import WolframLanguageSession


logging.basicConfig(filename='/tmp/python_testsuites.log',
                    filemode='a',
                    format='%(asctime)s, %(name)s %(levelname)s %(message)s',
                    level=logging.INFO)
logger = logging.getLogger(__name__)


@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCase(BaseTestCase):

    KERNEL_PATH = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

    def setUp(self):
        self.session = WolframLanguageSession(TestCase.KERNEL_PATH, log_kernel=False)
        self.session.set_parameter('STARTUP_READ_TIMEOUT', 2)
        self.session.set_parameter('TERMINATE_READ_TIMEOUT', 1)
        self.session.start()

    def tearDown(self):
        if self.session is not None:
            self.session.terminate()

    def test_evaluate_basic_inputform(self):
        res = self.session.evaluate('1+1')
        self.assertEqual(res.result(), b'2')

    def test_evaluate_basic_wl(self):
        res = self.session.evaluate(wl.Plus(1, 2))
        self.assertEqual(res.result(), b'3')

    def test_evaluate_variable_updates(self):
        self.session.evaluate('ClearAll[x]; x=1')
        self.session.evaluate('x++')
        res = self.session.evaluate('x+=10')
        self.assertEqual(res.result(), b'12')

    def test_evaluate_variable_context(self):
        self.session.evaluate('ClearAll[x]; x[] := foo')
        res = self.session.evaluate('Context[x]')
        self.assertEqual(res.result(), b'"Global`"')
        res = self.session.evaluate('Context[info]')
        self.assertEqual(res.result(), b'"Global`"')

    def test_malformed_expr(self):
        res = self.session.evaluate('Range[5')
        self.assertTrue(res.result().success)

    @unittest.skipIf(six.PY2, "No async call on Python2.")
    def test_evaluate_async(self):
        future1 = self.session.evaluate_async('3+4')
        future2 = self.session.evaluate_async('10+1')
        future3 = self.session.evaluate_async('100+1')
        self.assertEqual(future1.result().result(), b'7')
        self.assertEqual(future2.result().result(), b'11')
        self.assertEqual(future3.result().result(), b'101')
