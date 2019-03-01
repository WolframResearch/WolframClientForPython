# Version 1.0.0.dev5
- Introducing a new class `WolframEngineController`. Major code change to ensure ZMQ sockets are isolated in a given thread. Add a new controller class, a `Thread`, that own the sockets connected to the kernel. Interaction with this thread is done through a Queue.
- Merge `WolframLanguageFutureSession` into `WolframLanguageSession`. All evaluate methods have a future counterpart.
- Merge `WolframCloudFutureSession` into `WolframCloudSession`. Add relevant future methods.
- Rename `WolframAPICall` method `add_parameter` to `set_parameter` since setting is exactly what it does.
- rename evaluator parameters `STARTUP_READ_TIMEOUT` to `STARTUP_TIMEOUT`, and `TERMINATE_READ_TIMEOUT` to `TERMINATE_TIMEOUT`.
- CPU consumption reduced to ~1% with the latest ZMQLink version.
- Local session checks for the Wolfram Engine version at startup and fails with a specific message.

# Version 1.0.0.dev4
- Inconsistent class name `WolframCloudSessionFuture` replaced with `WolframCloudFutureSession`.
- Rework completely `Dispatch` class. Add a `Dispatch` instance called `wolfram_encoder` representing the multi dispatch function that encodes Python object.
- Add an entry point allowing new encoders to be registered as separated libraries (plugins).
- Add support for pandas classes: `Series` and `DataFrame`.
- Extend support for NumPy numbers.
- Local evaluators can be initialized without a Wolfram Engine path, in which case default paths are scanned to find the most recent product.

# Version 1.0.0.dev3
- Add `CHANGELOG.md` to the repository to help users track the changes.
- Major refactoring of the evaluation module.
    - add base classes for synchronous and asynchronous evaluation, specifying the evaluation scheme.
    - add cloud session implementation based on event loop using the aiohttp library.
    - rename asynchronous session based on future module from `WolframCloudAsyncSession` to `WolframCloudSessionFuture`.
    - add new implementation of `WolframCloudAsyncSession` based on asyncio and coroutines.
- Cloud session named option `authentication` renamed `credentials`.
- Significant improvement of `PIL.Image` support. Speed up serialization and deserialization of images, and add support for more input formats.


# Version 1.0.0.dev2
First released version of the library as a release candidate.