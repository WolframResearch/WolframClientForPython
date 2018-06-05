# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import wolframclient
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils import six

import logging
import unittest

logging.basicConfig(filename='/tmp/python_testsuites.log',
                    filemode='a',
                    format='%(asctime)s, %(name)s %(levelname)s %(message)s',
                    level=logging.INFO)
logger = logging.getLogger(__name__)


@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCase(BaseTestCase):
    KERNEL_PATH = '/Applications/Mathematica.app/Contents/MacOS/WolframKernel'
    def setUp(self):
        self.session = wolframclient.WolframLanguageSession(TestCase.KERNEL_PATH)
        self.session.start()

    def tearDown(self):
        if self.session is not None:
            self.session.terminate()

    def test_evaluate_basic_inputform(self):
        res = self.session.evaluate('1+1')
        self.assertEqual(res, b'2')

    def test_evaluate_basic_wl(self):
        res = self.session.evaluate(wolframclient.wl.Plus(1, 2))
        self.assertEqual(res, b'3')

    def test_evaluate_variable_updates(self):
        with wolframclient.WolframLanguageSession(TestCase.KERNEL_PATH) as new_session:
            new_session.evaluate('x=1')
            new_session.evaluate('x++')
            res = new_session.evaluate('x+=10')
            self.assertEqual(res, b'12')

    def test_evaluate_variable_context(self):
        with wolframclient.WolframLanguageSession(TestCase.KERNEL_PATH) as new_session:
            new_session.evaluate('x[] := foo')
            res = new_session.evaluate('Context[x]')
            self.assertEqual(res, b'"Global`"')
            res = new_session.evaluate('Context[info]')
            self.assertEqual(res, b'"Global`"')
