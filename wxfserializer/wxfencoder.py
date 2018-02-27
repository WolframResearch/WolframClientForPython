import wxfserializer.wxfexpr as wxfexpr

class WXFEncoder:
    ''' `WXFEncoder` defines a class of chained generators that eventually encode
    a given python object into instance(s) of `WXFExpr`. During initalization it is
    possible to define a fallback encoder to which it's possible to delegate using 
    `self.fallback(python_expr)`.
    '''
    __slots__ = '_fallback_encoder', '_root_encoder', '_default'

    def __init__(self, fallback_encoder=None):
        self._fallback_encoder = fallback_encoder
        if fallback_encoder is not None:
            fallback_encoder._root_encoder = self
        # root until it is used as a fallback encoder.
        self._root_encoder = self

    def encode(self, pythonExpr):
        yield from self._root_encoder.to_wxf(pythonExpr)
    '''
    Function to implement in sub-classes.
    '''
    def to_wxf(self, pythonExpr):
        raise TypeError('Object of type %s is not WXF serializable' %
                        pythonExpr.__class__.__name__)

    @property
    def default(self):
        return self._root_encoder.default

    @default.setter
    def default(self, default):
        if callable(default):
            self._root_encoder.default = default
        else:
            raise TypeError('Expecting default property must be callable.')

    @default.deleter
    def default(self):
        del(self._root_encoder.default)

    def fallback(self, pythonExpr):
        if self._fallback_encoder is not None:
            yield from self._fallback_encoder.to_wxf(pythonExpr)
        else:
            raise TypeError('Not supported python type')


class DefaultWXFEncoder(WXFEncoder):
    '''
    The most straight forward serialization of python expressions to their
    Wolfram Language equivalent. This class is meant to represent basically JSON like
    objects, and is intended to be used as a fallback provider in extended 
    provider implementation. As such it should only deal with obvious convertion,
    e.g: `int` to `Integer`. 
    
    Iterator are not supported but can easily be added in a sub-class.
    '''

    def to_wxf(self, pythonExpr):
        if isinstance(pythonExpr, str):
            yield wxfexpr.WXFExprString(pythonExpr)
        elif isinstance(pythonExpr, int):
            yield wxfexpr.WXFExprInteger(pythonExpr)
        elif isinstance(pythonExpr, list):
            yield wxfexpr.WXFExprFunction(len(pythonExpr))
            yield wxfexpr.WXFExprSymbol('List')
            for pyArg in iter(pythonExpr):
                yield from self.encode(pyArg)
        elif isinstance(pythonExpr, dict):
            yield wxfexpr.WXFExprAssociation(len(pythonExpr))
            for key, value in pythonExpr.items():
                yield wxfexpr.WXFExprRule()
                yield from self.encode(key)
                yield from self.encode(value)
        elif pythonExpr is True:
            yield wxfexpr.WXFExprSymbol('True')
        elif pythonExpr is False:
            yield wxfexpr.WXFExprSymbol('False')
        elif pythonExpr is None:
            yield wxfexpr.WXFExprSymbol('None')
        elif isinstance(pythonExpr, float):
            yield wxfexpr.WXFExprReal(pythonExpr)
        elif isinstance(pythonExpr, complex):
            yield wxfexpr.WXFExprFunction(2)
            yield wxfexpr.WXFExprSymbol('Complex')
            yield wxfexpr.WXFExprReal(pythonExpr.real)
            yield wxfexpr.WXFExprReal(pythonExpr.imag)
        else:
            yield from self.fallback(pythonExpr)
