from __future__ import absolute_import, print_function, unicode_literals
import logging
import os.path as path
from os.path import expandvars, expanduser, dirname
from subprocess import Popen, PIPE
from threading import Thread, Event
from time import sleep
from wolframclient.utils.six import string_types, binary_type, integer_types
from wolframclient.serializers import export
from wolframclient.language.expression import wl
from wolframclient.utils.encoding import force_text
import zmq
import json
import time
from math import floor
from time import perf_counter

logger = logging.getLogger(__name__)


class KernelError(Exception):
    pass


class KernelLogger(Thread):

    TO_PY_LOG_LEVEL = {
        1: logging.DEBUG,
        2: logging.INFO,
        3: logging.WARN,
        4: logging.FATAL
    }

    MAX_MESSAGE_BEFORE_QUIT = 32

    def __init__(self, socket):
        if not isinstance(socket, Socket):
            raise ValueError('Expecting a Socket.')
        self.socket = socket
        self.socket._bind()
        logger.info('Initializing Kernel logger on socket ' + self.socket.uri)
        super(KernelLogger, self).__init__(name='wolframkernel-logger-%s:%s' % (self.socket.host, self.socket.port))
        self.logger = logging.getLogger('WolframKernel-%s:%s' % (self.socket.host, self.socket.port))
        self.stopped = Event()

    def run(self):
        logger.debug('Start receiving kernel logger messages.')
        msg_after_quit = 0
        zmq_socket = self.socket.zmq_socket
        try:
            while msg_after_quit < KernelLogger.MAX_MESSAGE_BEFORE_QUIT:
                try:
                    msg = zmq_socket.recv_json(flags=zmq.NOBLOCK)
                    level = KernelLogger.TO_PY_LOG_LEVEL.get(msg.get('level', 3))
                    msg_text = msg.get('msg', 'Malformed kernel message. Missing key "msg".')
                    self.logger.log(level, msg_text)
                    if self.stopped.is_set():
                        msg_after_quit += 1
                except zmq.Again:
                    if self.stopped.is_set():
                        break
                    else:
                        time.sleep(.1)
        # no matter what we try to close the socket:
        finally:
            logger.info('Terminating kernel logger thread.')
            if msg_after_quit == KernelLogger.MAX_MESSAGE_BEFORE_QUIT:
                logger.warn('The maximum number of messages to log after a session finishes has been reached. \
                Some messages may have been discarded.')
            try:
                self.socket.close()
            except:
                logger.fatal('Failed to close ZMQ logging socket.')


class WolframKernelSession(object):
    def __init__(self, kernel, initfile=None, log_kernel=True,
                 in_socket=None, out_socket=None, logger_socket=None):
        if not isinstance(kernel, WolframKernel):
            raise ValueError('Expecting a WolframScript instance.')

        self.kernel = kernel
        if initfile is None:
            self.initfile = path.join(dirname(__file__), 'initkernel.m')
        else:
            self.initfile = initfile
        logger.debug('Initializing kernel using script: %s', self.initfile)
        # Socket to which we push new expressions to evaluate. 
        if in_socket is None:
            self.in_socket = Socket(zmq_type=zmq.PUSH)
        elif not isinstance(in_socket, Socket):
            raise ValueError('Expecting kernel input socket to be a Socket instance.')
        else:
            in_socket.zmq_type = zmq.PUSH
            self.in_socket = in_socket

        if out_socket is None:
            self.out_socket = Socket(zmq_type=zmq.PULL)
        elif not isinstance(out_socket, Socket):
            raise ValueError('Expecting kernel output socket to be a Socket instance.')
        else:
            out_socket.zmq_type = zmq.PULL
            self.out_socket = out_socket

        if logger_socket is None:
            self.logger_socket = Socket(zmq_type=zmq.PULL)
        elif not isinstance(out_socket, Socket):
            raise ValueError('Expecting kernel logger socket to be a Socket instance.')
        else:
            logger_socket.zmq_type = zmq.PULL
            self.logger_socket = logger_socket

        self.kernel_proc = None
        self.log = log_kernel
        self.kernel_logger = None
        self.evaluation_count = 0

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, type, value, traceback):
        self._clean()

    def _clean(self):
        # Exception handling here
        if self.in_socket is not None:
            self.in_socket.close()
        if self.out_socket is not None:
            self.out_socket.close()
        if self.kernel_logger is not None:
            self.kernel_logger.stopped.set()
            self.kernel_logger.join()
        if self.kernel_proc is not None:
            self.kernel_proc.terminate()

    KERNEL_OK = b'OK'

    def start(self):
        # start the evaluation zmq sockets
        self.in_socket._bind()
        self.out_socket._bind()
        # start the kernel process
        cmd = [self.kernel.path, '-noprompt', "-initfile", self.initfile]
        if self.log:
            self.kernel_logger = KernelLogger(socket=self.logger_socket)
            self.kernel_logger.start()
            cmd.append('-run')
            cmd.append('SlaveKernelPrivateStart["%s", "%s", "%s"];' 
                       % (self.in_socket.uri, self.out_socket.uri, self.logger_socket.uri))
        else:
            cmd.append('-run')
            cmd.append('SlaveKernelPrivateStart["%s", "%s"];'
                       % (self.in_socket.uri, self.out_socket.uri))

        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Kernel called using command: %s.' % ' '.join(cmd))

        try:
            self.kernel_proc = Popen(cmd, stdout=PIPE)
            logger.info('Kernel process started with PID: %s' % self.kernel_proc.pid)
            # First message must be "OK", acknowledging everything is up and running 
            # on the kernel side.
            response = self.out_socket._read_timeout()
            if response == WolframKernelSession.KERNEL_OK:
                logger.info('Kernel is ready.')
            else:
                # this will probably fails on remote kernels (broken pipe?)
                for line in _non_blocking_pipe_readline(self.kernel_proc.stdout):
                    last = force_text(line.rstrip())
                    logger.warn(last)
                raise KernelError(
                    'Failed to establish communication with the kernel. See log for more information.')
        except Exception as e:
            self._clean()
            raise e

    @property
    def started(self):
        return self.kernel_proc is not None

    def evaluate(self, expr):
        logger.debug('new evaluation on: %s', self)
        if not self.started:
            raise KernelError('Kernel is not started.')
        if isinstance(expr, binary_type):
            self.in_socket.zmq_socket.send(expr)
        else:
            self.in_socket.zmq_session.send(export(expr))
        # read the message as bytes.
        msgstr = self.out_socket.zmq_socket.recv()
        self.evaluation_count += 1
        return msgstr

    def __repr__(self):
        return '<WolframKernelSession: in:%s, out:%s>' % (self.in_socket.uri, self.out_socket.uri)


def _non_blocking_pipe_readline(pipe):
    ''' Read lines from a given `PIPE` in a non-blocking fashing.

    Modifies the internal flags of the given pipe to do so.
    '''
    import fcntl
    from os import O_NONBLOCK
    flags = fcntl.fcntl(pipe, fcntl.F_GETFL)
    fcntl.fcntl(pipe, fcntl.F_SETFL, flags | O_NONBLOCK)
    for line in pipe:
        yield line


class SocketException(Exception):
    pass


class Socket(object):
    def __init__(self, host='127.0.0.1', port=None, zmq_type=zmq.PAIR):
        self.host = host
        self.port = port
        if self.port is None:
            self.uri = 'tcp://' + self.host
        else:
            self.uri = 'tcp://%s:%s' % (self.host, self.port)
        self.zmq_type = zmq_type
        self.bound = False
        self.zmq_socket = self.zmq_socket = zmq.Context.instance().socket(zmq_type)

    def _bind(self):
        if self.bound:
            raise SocketException('Already bounded.')
        self.zmq_socket = zmq.Context.instance().socket(self.zmq_type)
        if self.port is None:
            logger.debug('Binding socket using random port.')
            self.port = self.zmq_socket.bind_to_random_port(self.uri)
            self.uri = 'tcp://%s:%s' % (self.host, self.port)
        else:
            self.zmq_socket.bind(self.uri)
        logger.debug('ZMQ socket bound to ' + self.uri)
        self.bound = True
        return self.zmq_socket

    RETRY_SLEEP_TIME = 0.01

    def _read_timeout(self, timeout=2.):
        if not self.bound:
            raise SocketException('ZMQ socket not bound.')
        if timeout < 0:
            raise ValueError('Timeout must be a positive number.')
        retry = 0
        start = perf_counter()
        max_retry = floor(timeout / Socket.RETRY_SLEEP_TIME)
        while perf_counter() - start < timeout:
            try:
                return self.zmq_socket.recv(flags=zmq.NOBLOCK)
            except zmq.Again:
                retry += 1
                time.sleep(Socket.RETRY_SLEEP_TIME)
        raise SocketException('Read timed out. Failed to read any message from socket %s after %.1f seconds and %i retries.'
                              % (self.uri, perf_counter() - start, retry))

    def close(self):
        self.zmq_socket.close()

    def __repr__(self):
        return '<Socket: host=%s, port=%s>' % (self.host, self.port)


def socket_uri(host='127.0.0.1', port=None):
    if port is None:
        return 'tcp://%s' + host
    else:
        return 'tcp://%s:%s' % (host, port)


class WolframKernel(object):
    def __init__(self, path, host='127.0.0.1'):
        self.path = expandvars(expanduser(path))
        self.host = host

    def __repr__(self):
        return '<WolframKernel %s on %s>' % (self.host, self.path)


def evaluate(kernel, expr, target_format='wl', **kargs):
    ''' One shot evaluation using a local kernel.
    '''
    if not isinstance(kernel, WolframKernel):
        raise ValueError('Expecting a WolframKernel instance.')
    with WolframKernelSession(kernel, log_kernel=False) as session:
        result = session.evaluate(expr)
    return result
