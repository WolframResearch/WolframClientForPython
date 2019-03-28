from time import perf_counter
from wolframclient.evaluation import WolframLanguageSession

with WolframLanguageSession() as session:
    start = perf_counter()
    print('Starting an evaluation delayed by 2 seconds.')
    future = session.evaluate_future('Pause[2]; 1+1')
    print('After %.04fs, the code is running in the background, Python execution continues.' %
          (perf_counter()-start))
    # wait for up to 5 seconds.
    expr = future.result(timeout = 5)
    print('After %.02fs, result was available. Kernel evaluation returned: %s' 
      % (perf_counter()-start, expr))
