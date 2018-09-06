# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
import logging

# set the Python root logger level to INFO
logging.basicConfig(level=logging.INFO)

# TODO: Change this value with a valid local path to a Wolfram Kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

# Start a new session, with kernel logging activated and log level set to INFO.
with WolframLanguageSession(kernel_path, kernel_loglevel=logging.INFO) as session:
    # This message is printed
    session.evaluate('ClientLibrary`info["OK -- Starting kernel evaluation"]')
    # This one is not because its level is debug. 
    session.evaluate('ClientLibrary`debug["KO -- Debug message."]')
    
    # Disable logging from the kernel side
    session.evaluate('ClientLibrary`DisableKernelLogging[]')
    # This last message will not be sent to Python.
    session.evaluate('ClientLibrary`info["KO -- End of kernel evaluation. Not printed"]')