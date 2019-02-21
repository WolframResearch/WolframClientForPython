import asyncio
import time
from wolframclient.evaluation import WolframLanguageAsyncSession
from wolframclient.language import wl

async def delayed_evaluation(delay, async_session, expr):
    await asyncio.sleep(delay)
    return await async_session.evaluate(expr)

async def main():
    async with WolframLanguageAsyncSession() as async_session:
        start = time.perf_counter()
        print('Running two tasks concurrently.')
        task1 = asyncio.ensure_future(delayed_evaluation(1, async_session, '"hello"'))
        task2 = asyncio.ensure_future(delayed_evaluation(1, async_session, '"world!"'))
        # wait for the two tasks to finish
        result1 = await task1
        result2 = await task2
        print('After %.02fs, both evaluations finished returning: %s, %s'
              % (time.perf_counter()-start, result1, result2))

# python 3.5+
loop = asyncio.get_event_loop()
loop.run_until_complete(main())

# python 3.7+
# asyncio.run(main())
