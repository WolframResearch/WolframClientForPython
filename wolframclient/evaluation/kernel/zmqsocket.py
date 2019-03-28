# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from functools import wraps

from wolframclient.exception import WolframLanguageException
from wolframclient.utils.api import time, zmq

logger = logging.getLogger(__name__)


class SocketException(WolframLanguageException):
    pass


class SocketAborted(SocketException):
    pass


class SocketOperationTimeout(SocketException):
    pass


def abortable():
    def outer(recv_method):
        @wraps(recv_method)
        def recv_abortable(socket,
                           timeout=None,
                           abort_check_period=.1,
                           abort_event=None,
                           **kwargs):
            if not socket.bound:
                raise SocketException('ZMQ socket not bound.')
            if timeout and timeout < 0:
                raise ValueError('Timeout must be a positive number.')
            retry = 0
            start = time.perf_counter()
            # fix inconsistencies
            if timeout and abort_check_period > timeout:
                abort_check_period = timeout
            abort_check_period = 1000. * abort_check_period
            while True:
                if socket.zmq_socket.poll(timeout=abort_check_period) > 0:
                    try:
                        return recv_method(socket, flags=zmq.NOBLOCK, **kwargs)
                    # just in case there is more than one consumer.
                    except zmq.Again:
                        pass
                retry += 1
                if abort_event:
                    if abort_event.is_set():
                        raise SocketAborted('Socket operation aborted.')
                if timeout and (time.perf_counter() - start > timeout):
                    break
            raise SocketOperationTimeout(
                'Failed to read any message from socket %s after %.1f seconds and %i retries.'
                % (socket.uri, time.perf_counter() - start, retry))

        return recv_abortable

    return outer


class Socket(object):
    """ Wrapper around ZMQ socket """

    def __init__(self,
                 protocol='tcp',
                 host='127.0.0.1',
                 port=None,
                 zmq_type=zmq.PAIR):
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
            port = self.zmq_socket.bind_to_random_port(
                '%s://%s' % (protocol, host))
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

    @abortable()
    def recv_abortable(self, **kwargs):
        # def abortable_recv(self, timeout=None, abort_check_period=0.1, abort_event=None, copy=True):
        """ Read a socket in a non-blocking fashion, until a timeout is reached, or until an abort Event is set."""
        return self.recv(**kwargs)

    @abortable()
    def recv_json_abortable(self, **kwargs):
        """ Read a socket for a json message, in a non-blocking fashion, until a timeout is reached, or until an abort Event is set."""
        return self.recv_json(**kwargs)

    def close(self):
        self.zmq_socket.close()
        self.closed = True
        self.bound = False

    def __repr__(self):
        if self.bound:
            return '<Socket: uri=%s>' % self.uri
        else:
            return '<Socket (not bounded): uri=%s>' % self.uri
