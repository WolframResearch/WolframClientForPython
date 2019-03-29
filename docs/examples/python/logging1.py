from wolframclient.evaluation import WolframLanguageSession
import logging

# set the root level to INFO
logging.basicConfig(level=logging.INFO)

try:
    session = WolframLanguageSession()
    # this will trigger some log messages with the process ID, the sockets
    # address and the startup timer.
    session.start()
    # Warning: Infinite expression Power[0, -1] encountered.
    res = session.evaluate('1/0')
finally:
    session.terminate()
