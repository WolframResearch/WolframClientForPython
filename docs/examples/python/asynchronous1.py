# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageAsyncSession
from wolframclient.utils.api import time
# Change the path to point to a local kernel.
# This value corresponds to the default path to a MacOS Wolfram Desktop kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

with WolframLanguageAsyncSession(kernel_path) as async_session:
    start = time.perf_counter()
    print('Starting an evaluation delayed by 2 seconds.')
    future = async_session.evaluate('Pause[2]; 1+1')
    print('After %.04fs, the code is running in the background, Python execution continues.' %
          (time.perf_counter()-start))
    # wait for up to 5 seconds.
    expr = future.result(timeout = 5)
    print('After %.02fs, result was available. Kernel evaluation returned: %s' 
      % (time.perf_counter()-start, expr))
