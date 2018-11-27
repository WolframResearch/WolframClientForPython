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