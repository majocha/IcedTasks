namespace IcedTasks

open System
open System.Runtime.ExceptionServices
open System.Threading
open System.Runtime.CompilerServices

type Trampoline private () =

    let ownerThreadId = Thread.CurrentThread.ManagedThreadId

    let mutable next: ValueOption<Action> = ValueNone
    let mutable executing = false

    let loop () =
        try
            while next.IsSome do
                let action = next.Value
                next <- ValueNone
                action.Invoke()
        finally
            executing <- false

    let start action =
        next <- ValueSome action
        executing <- true
        loop ()

    let set action =
        assert (Thread.CurrentThread.ManagedThreadId = ownerThreadId)
        assert next.IsNone
        if executing then next <- ValueSome action else start action

    static let holder = new ThreadLocal<Trampoline>(fun () -> Trampoline())

    interface ICriticalNotifyCompletion with
        member this.OnCompleted(continuation: Action) = set continuation
        member this.UnsafeOnCompleted(continuation: Action) = set continuation

    member this.AwaiterRef = ref (this :> ICriticalNotifyCompletion)
    member this.Awaiter = (this :> ICriticalNotifyCompletion)

    static member Current = holder.Value

module BindDepthCounter =
    [<Literal>]
    let bindLimit = 50

    let counter = new ThreadLocal<int>()

    let inline Check () =
        counter.Value <-
            counter.Value
            + 1

        if
            counter.Value
            >= bindLimit
        then
            counter.Value <- 0
            true
        else
            false

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
