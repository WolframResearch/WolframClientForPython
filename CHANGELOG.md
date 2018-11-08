# Version 1.0.0.dev3
- Add `CHANGELOG.md` to the repository to help users track the changes.
- Major refactoring of the evaluation module.
    - add base classes to define evaluation schemes, synchronous and asynchronous.
    - add cloud session implementation based on asyncio and aiohttp.
    - rename asynchronous session based on future module from `WolframCloudAsyncSession` to `WolframCloudSessionFuture`.
    - add new implementation of `WolframCloudAsyncSession` based on asyncio and coroutines.
- Cloud session named option `authentication` renamed `credentials`.


# Version 1.0.0.dev2
First released version of the library as a release candidate.