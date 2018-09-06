# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
import logging

# set the root level to INFO
logging.basicConfig(level=logging.INFO)

# TODO: Change this value with a valid local path to a Wolfram Kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

try:
    session = WolframLanguageSession(kernel_path)
    # this will trigger some log messages with the process ID, the sockets
    # address and the startup timer.
    session.start()
    # Warning: Infinite expression Infinity encountered.
    res = session.evaluate('1/0')
finally:
    session.terminate()
