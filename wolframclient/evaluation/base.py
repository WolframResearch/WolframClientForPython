# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import asyncio
import warnings

from wolframclient.language import wlexpr
from wolframclient.language.expression import WLFunction
from wolframclient.utils.asyncio import wait_all
from wolframclient.utils.six import PY_36, string_types
"""
NOTES

 - wolfram.evaluation.__init__ contains a bare try except.

 - CloudFunction is implemented only for cloud kernels
 - hardcoded class calls should be class attributes
 - all classes should have evaluate, evaluate_many (we should not hardcode ToExpression behaviour in evaluate)
 - you are always checking the input type, while this can help some users it's really preventing users from doing duck typing



 - WolframEvaluatorPool should be constructed using a generator of other kernels class
 - WolframKernal should have a copy method
 - should terminated kernels restart automatically instead of raise an Exception?

"""


class WolframEvaluatorBase(object):
    """ All evaluators should inherit from thos base class. """
    stopped = True # avoid error in __del__ if __init__ failed.
    # started = False # all evaluators should be in this state at init.
    
    @property
    def started(self):
        raise NotImplementedError

    def __del__(self, _warnings=warnings):
        if not self.stopped:
            if PY_36:
                kwargs = {'source': self}
            else:
                kwargs = {}
            _warnings.warn(
                "Unclosed instance of %s: %s" %
                (self.__class__.__name__, self), ResourceWarning, **kwargs)


class WolframEvaluator(WolframEvaluatorBase):
    """ Synchronous evaluator abstract class. """
    def evaluate(self, expr):
        """ Evaluate a given Wolfram Language expression. """
        return self.evaluate_wrap(expr).get()

    def evaluate_many(self, expr_list):
        """ Evaluate a given list of Wolfram Language expression.
        
        The list is provided as an iterable object. 
        """
        return map(self.evaluate, expr_list)

    def evaluate_wrap(self, expr):
        """ Evaluate a given Wolfram Language expression and return a result object with the result and meta information. """
        raise NotImplementedError

    def start(self):
        """ Start the evaluator. 
        
        Once this function was called, the evaluator must be ready to evaluate incoming expressions. 
        """
        raise NotImplementedError

    def stop(self):
        """ Gracefully stop the evaluator. Try to stop the evaluator, but wait for current evaluation to finish first. """
        raise NotImplementedError

    def terminate(self):
        """ Immediatly stop the evaluator, eventually killing running jobs resulting in cancelled evaluations. """
        raise NotImplementedError

    def restart(self):
        """ Restart a given evaluator by stopping it in case it was already started. """
        if self.started:
            self.stop()
        self.start()

    def function(self, expr):
        """Return a python function from a Wolfram Language function `expr`.

        The object returned can be applied on arguments as any other Python function, and
        is evaluated using the underlying Wolfram evaluator.
        """
        def inner(*args, **opts):
            return self.evaluate(WLFunction(expr, *args, **opts))

        return inner

    def duplicate(self):
        """ Build a new instance using the same configuration as the one being duplicated. """
        raise NotImplementedError

    def __enter__(self):
        """Evaluator must be usable with context managers."""
        if not self.started:
            self.start()
        return self

    def __exit__(self, type, value, traceback):
        """Context manager stop function."""
        if self.started:
            self.stop()


class WolframAsyncEvaluator(WolframEvaluatorBase):
    """ Asynchronous evaluators are similar to synchronous ones except that they make heavy use of coroutines
    and need an event loop. 
    
    Most methods from this class are similar to their counterpart from :class:`~wolframclient.evaluation.base.WolframEvaluator`,
    except that they are coroutines. """
    def __init__(self, loop=None):
        if loop:
            self._loop = loop
        else:
            self._loop = asyncio.get_event_loop()

    async def evaluate(self, expr):
        result = await self.evaluate_wrap(expr)
        return await result.get()

    async def evaluate_many(self, expr_list):
        return await wait_all(map(self.evaluate, expr_list), loop=self._loop)

    async def evaluate_wrap(self, expr):
        raise NotImplementedError

    async def start(self):
        raise NotImplementedError

    async def stop(self):
        #Graceful stop
        raise NotImplementedError

    async def terminate(self):
        raise NotImplementedError

    async def restart(self):
        if self.started:
            await self.stop()
        await self.start()

    def duplicate(self):
        raise NotImplementedError

    def function(self, expr):
        """ Return a coroutine from a Wolfram Language function. """
        async def inner(*args, **opts):
            return await self.evaluate(WLFunction(expr, *args, **opts))

        return inner

    def __enter__(self):
        """ A user friendly message when 'async with' is not used. 
        
        This method should not be implemented in child classes."""
        raise NotImplementedError("%s must be used in a 'async with' block." %
                                  self.__class__.__name__)

    def __exit__(self, type, value, traceback):
        """ Let the __enter__ method fail and propagate doing nothing. 
        
        This method should not be implemented in child classes."""
        pass

    async def __aenter__(self):
        """ Asynchronous context management start function. """
        if not self.started:
            await self.start()
        return self

    async def __aexit__(self, type, value, traceback):
        """ Asynchronous context management stop function. """
        if self.started:
            await self.stop()

    def __del__(self, _warnings=warnings):
        super().__del__(_warnings=warnings)
        if self._loop and not self.stopped and not self._loop.is_closed():
            context = {
                self.__class__.__name__: self,
                'message': 'Unclosed evaluator.'
            }
            self._loop.call_exception_handler(context)

def normalize_input(expr, string_as_inputform=True):
    """ Normalize a given python object representing an expr to as object
    as expected by evaluators.

    Option `string_as_inputform` specifies if strings should be treated
    as input form strings and wrapped into wlexpr.
    """
    if string_as_inputform and isinstance(expr, string_types):
        return wlexpr(expr)
    else:
        return expr