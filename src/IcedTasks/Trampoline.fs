namespace IcedTasks

open System
open System.Runtime.ExceptionServices
open System.Threading
open System.Runtime.CompilerServices
open IcedTasks.TaskLike

type DynamicState =
    | Running
    | SetResult
    | SetException of ExceptionDispatchInfo
    | Awaiting of ICriticalNotifyCompletion
    | Bounce of DynamicState
    | Immediate of DynamicState

type Trampoline private () =

    let ownerThreadId = Thread.CurrentThread.ManagedThreadId

    static let holder = new ThreadLocal<_>(fun () -> Trampoline())

    let mutable depth = 0

    [<Literal>]
    let MaxDepth = 50

    //// calling TryEnsureSufficientExecutionStack is relatively expensive, so we only call it every MaxDepth calls
    //        if current.Value % MaxDepth = 0 then
    //#if NETSTANDARD2_0
    //            try RuntimeHelpers.EnsureSufficientExecutionStack(); true with _ -> false
    //#else
    //            RuntimeHelpers.TryEnsureSufficientExecutionStack()
    //#endif
    //        else
    //            true

    let mutable pending: Action voption = ValueNone
    let mutable running = false

    let mutable primed = true

    let start () =
        try
            running <- true

            while pending.IsSome do
                let next = pending.Value
                pending <- ValueNone
                next.Invoke()
        finally
            running <- false

    let set action =
        assert (Thread.CurrentThread.ManagedThreadId = ownerThreadId) // "Trampoline used from wrong thread"
        assert pending.IsNone // "Trampoline set while already pending"

        pending <- ValueSome action

        if not running then
            start ()

    interface ICriticalNotifyCompletion with
        member _.OnCompleted continuation = set continuation
        member _.UnsafeOnCompleted continuation = set continuation

    member this.Ref: ICriticalNotifyCompletion ref = ref this

    member _.ShouldBounce =
        not running || (depth <- depth + 1; depth % MaxDepth = 0)

    member _.Prime() = primed <- true

    member _.WasPrimed() = 
        let wasPrimed = primed
        primed <- false
        wasPrimed

    static member Current = holder.Value

module ExceptionCache =
    let store = ConditionalWeakTable<exn, ExceptionDispatchInfo>()

    let inline CaptureOrRetrieve (exn: exn) =
        match store.TryGetValue exn with
        | true, edi when edi.SourceException = exn -> edi
        | _ ->
            let edi = ExceptionDispatchInfo.Capture exn

            try
                store.Add(exn, edi)
            with _ ->
                ()

            edi

    let inline Throw (exn: exn) =
        let edi = CaptureOrRetrieve exn
        edi.Throw()
        Unchecked.defaultof<_>

    let inline GetResultOrThrow awaiter =
        try
            Awaiter.GetResult awaiter
        with exn ->
            Throw exn
