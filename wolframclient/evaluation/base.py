# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import asyncio
import warnings

from wolframclient.language import wlexpr
from wolframclient.language.expression import WLFunction
from wolframclient.utils import six
from wolframclient.utils.functional import map


class WolframEvaluatorBase(object):
    """ All evaluators should inherit from this base class. 
    
    `string_as_inputform` specifies if strings should be treated
        as input form strings and wrapped into :func:`~wolframclient.language.wlexpr`
    """
    stopped = True  # avoid error in __del__ if __init__ failed.

    def __init__(self, inputform_string_evaluation=True, **kwargs):
        self.inputform_string_evaluation = inputform_string_evaluation

    @property
    def started(self):
        raise NotImplementedError

    def __del__(self, _warnings=warnings):
        if not self.stopped:
            if six.PY_36:
                kwargs = {'source': self}
            else:
                kwargs = {}
            _warnings.warn(
                "Unclosed instance of %s: %s" %
                (self.__class__.__name__, self), ResourceWarning, **kwargs)

    def normalize_input(self, expr):
        """ Normalize a given Python object representing an expr to as object
        as expected by evaluators.
        """
        if self.inputform_string_evaluation and isinstance(
                expr, six.string_types):
            return wlexpr(expr)
        else:
            return expr

    def duplicate(self):
        """ Build a new instance using the same configuration as the one being duplicated. """
        raise NotImplementedError


class WolframEvaluator(WolframEvaluatorBase):
    """ Synchronous evaluator abstract class. """

    def evaluate(self, expr):
        """ Evaluate a given Wolfram Language expression. """
        return self.evaluate_wrap(expr).get()

    def evaluate_future(self, expr):
        """ Evaluate a given Wolfram Language expression asynchronously.
        
        Return a :class:`~concurrent.futures.Future` object. """
        raise NotImplementedError

    def evaluate_many(self, expr_list):
        """ Evaluate a given list of Wolfram Language expressions.
        
        The list is provided as an iterable object. 
        """
        # for consistency with the async version, return a list.
        return list(map(self.evaluate, expr_list))

    def evaluate_wrap(self, expr):
        """ Evaluate a given Wolfram Language expression and return a result object with the result and meta
        information. """
        raise NotImplementedError

    def evaluate_wrap_future(self, expr):
        """ Asynchronously call `evaluate_wrap`.
        
        Return a :class:`~concurrent.futures.Future` object. """
        raise NotImplementedError

    def start(self):
        """ Start the evaluator. 
        
        Once this function is called, the evaluator must be ready to evaluate incoming expressions.
        """
        raise NotImplementedError

    def stop(self):
        """ Gracefully stop the evaluator. Try to stop the evaluator but wait for the current evaluation to finish
        first.
        """
        raise NotImplementedError

    def terminate(self):
        """ Immediately stop the evaluator, which will kill the running jobs, resulting in cancelled evaluations. """
        raise NotImplementedError

    def restart(self):
        """ Restart a given evaluator by stopping it in cases where it is already started. """
        if self.started:
            self.stop()
        self.start()

    def function(self, expr):
        """Return a Python function from a Wolfram Language function `expr`.

        The object returned can be applied on arguments as any other Python function and is evaluated using the
        underlying Wolfram evaluator.
        """
        normalized_expr = self.normalize_input(expr)

        def inner(*args, **opts):
            return self.evaluate(WLFunction(normalized_expr, *args, **opts))

        return inner

    def function_future(self, expr):
        """Return a Python function from a Wolfram Language function `expr`, that evaluates asynchronously, returning a
         future object. """
        normalized_expr = self.normalize_input(expr)

        def inner(*args, **opts):
            return self.evaluate_future(
                WLFunction(normalized_expr, *args, **opts))

        return inner

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

    def __init__(self, loop=None, **kwargs):
        super().__init__(**kwargs)
        self._loop = loop or asyncio.get_event_loop()

    async def evaluate(self, expr):
        result = await self.evaluate_wrap(expr)
        return await result.get()

    async def evaluate_many(self, expr_list):
        return await asyncio.gather(
            *map(self.evaluate, expr_list), loop=self._loop)

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

    def function(self, expr):
        """ Return a coroutine from a Wolfram Language function. 
        
        The coroutine returned is attached to a given asynchronous evaluator.
        """
        normalized_expr = self.normalize_input(expr)

        async def inner(*args, **opts):
            return await self.evaluate(
                WLFunction(normalized_expr, *args, **opts))

        return inner

    def __enter__(self):
        """ A user friendly message when 'async with' is not used. 
        
        This method should not be implemented in child classes."""
        raise NotImplementedError("%s must be used in a 'async with' block." %
                                  self.__class__.__name__)

    def __exit__(self, type, value, traceback):
        """ Let the __enter__ method fail and propagate doing nothing. 
        
        This method should not be implemented in child classes."""

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
