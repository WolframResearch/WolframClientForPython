import numpy
from wxfexprprovider import WXFExprProvider, DefaultWXFEncoder, WXFEncoder
from wxfexpr import ArrayTypes
import wxfexpr

class WXFExprProviderNumPy(WXFExprProvider):

    def __init__(self, fallback_encoder=None, default=None):
        if fallback_encoder is None:
            super(WXFExprProviderNumPy, self).__init__(encoder=NumPyWXFEncoder(DefaultWXFEncoder()), default=default)
        else:
            super(WXFExprProviderNumPy, self).__init__(
                encoder=NumPyWXFEncoder(fallback_encoder), default=default)

class NumPyWXFEncoder(WXFEncoder):

    def to_wxf(self, pythonExpr):
        if isinstance(pythonExpr, numpy.ndarray):
            # TODO: deal with endianness.
            dimensions = pythonExpr.shape
            rank = len(pythonExpr.shape)
            # TODO deal with endianness
            data = pythonExpr
            if pythonExpr.dtype == numpy.int8:
                arrayExpr = wxfexpr.WXFExprPackedArray(
                    rank, dimensions, ArrayTypes.Integer8)
            elif pythonExpr.dtype == numpy.int16 or pythonExpr.dtype == numpy.uint8:
                arrayExpr = wxfexpr.WXFExprPackedArray(
                    rank, dimensions, ArrayTypes.Integer16)
            elif numpy.dtype == numpy.int32 or pythonExpr.dtype == numpy.uint16:
                arrayExpr = wxfexpr.WXFExprPackedArray(
                    rank, dimensions, ArrayTypes.Integer32)
            elif pythonExpr.dtype == numpy.int64 or pythonExpr.dtype == numpy.uint32:
                arrayExpr = wxfexpr.WXFExprPackedArray(
                    rank, dimensions, ArrayTypes.Integer64)
            elif pythonExpr.dtype == numpy.float64:
                arrayExpr = wxfexpr.WXFExprPackedArray(
                    rank, dimensions, ArrayTypes.Real64)
            elif pythonExpr.dtype == numpy.float32:
                arrayExpr = wxfexpr.WXFExprPackedArray(
                    rank, dimensions, ArrayTypes.Real32)
            # TODO other types: uint64, complex?
            else:
                raise Exception(
                    'NumPy serialization not implemented for ', repr(pythonExpr.dtype))

            arrayExpr.data = data
            yield arrayExpr
        else:
            yield from super(NumPyWXFEncoder, self).fallback(pythonExpr)
