# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function, unicode_literals
import sys
import os
import time
import asyncio
sys.path.insert(
    0, '/Users/dorianb/Work/Mathematica/Workspaces/WolframClientForPython')

from wolframclient.evaluation import WolframKernelPool
from wolframclient.evaluation import WolframLanguageAsyncSession
from wolframclient.evaluation import WolframLanguageFutureSession

from wolframclient.language import wl, wlexpr
from wolframclient.utils.api import time

import logging

logging.basicConfig(filename='/tmp/python.log',
                    filemode='a',
                    format='%(asctime)s, %(thread)s–%(name)s %(levelname)s %(message)s',
                    level=logging.DEBUG
                    )

logger = logging.getLogger('KernelPoolTest')


from wolframclient.evaluation.kernel.kernelpool import parallel_evaluate
wk = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
print(parallel_evaluate(wk, range(10)))

async def create_pool():
    pool = WolframKernelPool(wk, poolsize=3, STARTUP_READ_TIMEOUT=10, TERMINATE_READ_TIMEOUT=10)
    await pool.start()
    return pool

start = time.perf_counter()
loop = asyncio.get_event_loop()
loop.set_debug(enabled=True)
pool1 = None
pool2 = None
try:
    pool1 = loop.run_until_complete(create_pool())
    pool2 = loop.run_until_complete(create_pool())
    print(pool1.evaluate_all(range(10)))
    print(pool2.evaluate_all(['Pause[100000]']))
finally:
    for pool in (pool1, pool2):
        if pool:
            logger.info('Pool status: %s', pool)
            logger.info('Pool kernel status: %s', pool._kernels)
            loop.run_until_complete(pool.terminate())

    print('THE END. % .2f seconds.' % (time.perf_counter() - start))
