from __future__ import absolute_import, print_function, unicode_literals

import unittest

from wolframclient.evaluation.cloud.cloudsession import URLBuilder
from wolframclient.evaluation.configuration import WolframPublicCloudConfig

class TestURLBuilder(unittest.TestCase):
    def test_append_no_base(self):
        builder = URLBuilder('')
        url = builder.append('http://wolfram.com').append('foo').get()
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_simple_append(self):
        builder = URLBuilder('http://wolfram.com')
        url = builder.append('foo').get()
        self.assertEqual(url, 'http://wolfram.com/foo')
        
    def test_simple_append_end_slash(self):
        builder = URLBuilder('http://wolfram.com/')
        url = builder.append('foo').get()
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_simple_append_start_slash(self):
        builder = URLBuilder('http://wolfram.com')
        url = builder.append('/foo').get()
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_simple_append_two_slash(self):
        builder = URLBuilder('http://wolfram.com/')
        url = builder.append('/foo').get()
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_extend(self):
        builder = URLBuilder('http://wolfram.com/')
        url = builder.extend('foo','bar','baz').get()
        self.assertEqual(url, 'http://wolfram.com/foo/bar/baz')