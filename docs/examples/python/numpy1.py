from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
import numpy
from imageio import imsave
# TODO: Change this value with a valid local path to a Wolfram Kernel.
kernel_path = '/Applications/Wolfram Desktop12.app/Contents/MacOS/WolframKernel'
with WolframLanguageSession(kernel_path) as session:
    expr = wl.Rasterize(wl.Plot(wl.Sin(wl.x), [wl.x, 0, wl.Pi]))
    res = session.evaluate(expr)
    if res.success:
        image = res.get()
        image_data = image.args[0]
        print('Image data is a numpy array of type %s and dimensions %s' %
            (image_data.dtype, image_data.shape))
        imsave('/tmp/plot.png', image_data)
    else:
        print('Something went wrong: %s' % res.failure)