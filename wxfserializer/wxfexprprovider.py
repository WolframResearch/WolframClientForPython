"""
"""
import sys
import wxfserializer.wxfexpr as wxfexpr

class WXFEncoder:
    ''' `WXFEncoder` defines a class of chained generators.
    '''
    __slots__ = '_fallback_encoder', 'root_encoder', '_default'

    def __init__(self, fallback_encoder = None):
        self._fallback_encoder = fallback_encoder
        if fallback_encoder is not None:
            fallback_encoder.root_encoder = self
        # root until it is used as a fallback encoder.
        self.root_encoder = self
    
    def encode(self, pythonExpr):
        yield from self.root_encoder.to_wxf(pythonExpr)

    def to_wxf(self, pythonExpr):
        raise TypeError('Object of type %s is not WXF serializable' % o.__class__.__name__)

    @property
    def default(self):
        return self.root_encoder.default
        
    @default.setter
    def default(self, default):
        if callable(default):
            self.root_encoder.default = default
        else:
            raise TypeError('Default property must be callable.')

    @default.deleter
    def default(self):
        del(self.root_encoder.default)


    def fallback(self, pythonExpr):
        if self._fallback_encoder is not None:
            yield from self._fallback_encoder.to_wxf(pythonExpr)
        else:
            raise TypeError('Not supported python type')


class WXFExprProvider:

    __slots__ = 'encoder', 'default'

    def __init__(self, encoder=None, default=None):
        if encoder is not None:
            self.encoder = encoder
        else:
            self.encoder = DefaultWXFEncoder()
        if default is not None:
            self.encoder.default = default

    def pythonToWXFExpr(self, pythonExpr):
       yield from self.encoder.encode(pythonExpr)


class DefaultWXFEncoder(WXFEncoder):
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
        else:
            yield from self.fallback(pythonExpr)
