from wolframclient.evaluation import WolframLanguageSession
import logging

# set the Python root logger level to INFO
logging.basicConfig(level=logging.INFO)

# Start a new session, with kernel logging activated and log level set to INFO.
with WolframLanguageSession(kernel_loglevel=logging.INFO) as session:
    # This message is printed
    session.evaluate('ClientLibrary`info["ON -- Example message printed from the kernel \
with log level INFO --"]')
    # This one is not because its level is debug. 
    session.evaluate('ClientLibrary`debug["OFF -- Debug message."]')
    
    # Disable logging from the kernel side
    session.evaluate('ClientLibrary`DisableKernelLogging[]')
    # These messages will not be sent to Python.
    session.evaluate('ClientLibrary`fatal["OFF -- Fatal message. Not printed"]')
    session.evaluate('ClientLibrary`info["OFF -- End of kernel evaluation. Not printed"]')
