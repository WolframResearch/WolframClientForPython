import asyncio
import time
from wolframclient.evaluation import WolframEvaluatorPool

async def main():
    async with WolframEvaluatorPool() as pool:
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
