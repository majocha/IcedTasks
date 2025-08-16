namespace IcedTasks

open System
open System.Runtime.ExceptionServices

type ExceptionDispatchInfoNull =
#if NULLABLE
    ExceptionDispatchInfo | null
#else
    ExceptionDispatchInfo
#endif

type IDisposableNull =
#if NULLABLE
    IDisposable | null
#else
    IDisposable
#endif

type IAsyncDisposableNull =
#if NULLABLE
    IAsyncDisposable | null
#else
    IAsyncDisposable
#endif
