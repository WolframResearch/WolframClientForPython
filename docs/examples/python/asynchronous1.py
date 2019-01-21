from wolframclient.evaluation import WolframLanguageFutureSession
from wolframclient.utils.api import time

with WolframLanguageFutureSession() as future_session:
    start = time.perf_counter()
    print('Starting an evaluation delayed by 2 seconds.')
    future = future_session.evaluate('Pause[2]; 1+1')
    print('After %.04fs, the code is running in the background, Python execution continues.' %
          (time.perf_counter()-start))
    # wait for up to 5 seconds.
    expr = future.result(timeout = 5)
    print('After %.02fs, result was available. Kernel evaluation returned: %s' 
      % (time.perf_counter()-start, expr))
