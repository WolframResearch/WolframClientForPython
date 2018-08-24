# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
from wolframclient.logger.utils import setup_logging_to_file
import logging
setup_logging_to_file('/tmp/python.log', logging.DEBUG)
# Change the path to point to a local kernel. 
# This value corresponds to the default path to a MacOS Wolfram Desktop kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
with WolframLanguageSession(kernel_path) as session:
    array = [[1, 2, 3], [0, 4, 5], [0, 0, 6]]
    res = session.evaluate(wl.Transpose(array))
    print(res.get())
