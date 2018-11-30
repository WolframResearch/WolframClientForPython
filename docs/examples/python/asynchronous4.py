# -*- coding: utf-8 -*-
import asyncio
from wolframclient.evaluation import WolframEvaluatorPool
from wolframclient.utils.api import time
# Change the path to point to a local kernel.
# This value corresponds to the default path to a MacOS Wolfram Desktop kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

async def main():
    async with WolframEvaluatorPool(kernel_path) as pool:
        start = time.perf_counter()
        tasks = [
            pool.evaluate('Pause[1]')
            for i in range(10)
            ]
        await asyncio.wait(tasks)
        print('Done after %.02fs, using up to %i kernels.' 
            % (time.perf_counter()-start, len(pool)))

# python 3.5+
loop = asyncio.get_event_loop()
loop.run_until_complete(main())

# python 3.7+
# asyncio.run(main())
