# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from wolframclient.utils.api import zmq, time

logger = logging.getLogger(__name__)

class SocketException(Exception):
    pass


class Socket(object):
    """ Wrapper around ZMQ socket """

    def __init__(self, protocol='tcp', host='127.0.0.1', port=None, zmq_type=zmq.PAIR):
        self.zmq_type = zmq_type
        self.uri = None
        self.bound = False
        self.zmq_socket = zmq.Context.instance().socket(zmq_type)
        self.closed = False

    def can_bind_or_fail(self):
        if self.bound:
            raise SocketException('Already bounded.')
        if self.closed:
            raise SocketException('Socket has been closed.')

    def bind_to_uri(self, uri):
        self.can_bind_or_fail()
        self.zmq_socket.bind(uri)
        self.uri = uri
        logger.debug('ZMQ socket bound to ' + uri)
        self.bound = True
        return self.zmq_socket

    def bind(self, protocol='tcp', host='127.0.0.1', port=None):
        self.can_bind_or_fail()
        if protocol == 'inproc':
            self.uri = 'inproc://%s' % host
            self.zmq_socket.bind(self.uri)
        elif port:
            self.uri = '%s://%s:%s' % (protocol, host, port)
            self.zmq_socket.bind(self.uri)
        else:
            port = self.zmq_socket.bind_to_random_port('%s://%s' % (protocol, host))
            self.uri = '%s://%s:%s' % (protocol, host, port)
        logger.debug('ZMQ socket bound to ' + self.uri)
        self.bound = True
        return self.zmq_socket

    def recv(self, *args, **kwargs):
        return self.zmq_socket.recv(*args, **kwargs)

    def recv_json(self, *args, **kwargs):
        return self.zmq_socket.recv_json(*args, **kwargs)

    def send(self, *args, **kwargs):
        return self.zmq_socket.send(*args, **kwargs)

    def poll(self, *args, **kwargs):
        return self.zmq_socket.poll(*args, **kwargs)

    def recv_timeout(self,
                     timeout=2.,
                     retry_sleep_time=0.001,
                     sleep=time.sleep):
        """ Read a socket in a non-blocking fashion, until a timeout is reached, retrying at a given interval.
        The sleep function is passed as a parameter and can be conveniently modified to support Event based interruption.
        """
        if not self.bound:
            raise SocketException('ZMQ socket not bound.')
        if timeout < 0:
            raise ValueError('Timeout must be a positive number.')
        retry = 0
        start = time.perf_counter()
        while time.perf_counter() - start < timeout:
            try:
                return self.zmq_socket.recv(flags=zmq.NOBLOCK)
            except zmq.Again:
                retry += 1
                try:
                    sleep(retry_sleep_time)
                except TimeoutError:
                    break
        raise SocketException(
            'Read time out. Failed to read any message from socket %s after %.1f seconds and %i retries.'
            % (self.uri, time.perf_counter() - start, retry))

    def close(self):
        self.zmq_socket.close()
        self.closed = True
        self.bound = False

    def __repr__(self):
        if self.bound:
            return '<Socket: uri=%s>' % self.uri
        else:
            return '<Socket (not bounded): uri=%s>' % self.uri
