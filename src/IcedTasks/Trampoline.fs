namespace IcedTasks

open System
open System.Runtime.ExceptionServices
open System.Threading
open System.Runtime.CompilerServices

[<AutoOpen>]
module Assert =
    let failIfNot condition msg =
        if not condition then
            failwith $" assertion failed {msg}"

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
        executing <- true
        loop ()

    let set action =
        failIfNot (Thread.CurrentThread.ManagedThreadId = ownerThreadId) "thread"
        failIfNot next.IsNone "next is not None"
        next <- ValueSome action

        if not executing then
            start action

    let setDynamic action =
        failIfNot (Thread.CurrentThread.ManagedThreadId = ownerThreadId) "thread"
        failIfNot next.IsNone "next is not None in setDynamic"
        next <- ValueSome action

        if not executing then
            start action

    static let holder = new ThreadLocal<Trampoline>(fun () -> Trampoline())

    interface ICriticalNotifyCompletion with
        member _.OnCompleted(continuation: Action) = set continuation
        member _.UnsafeOnCompleted(continuation: Action) = setDynamic continuation

    member this.AwaiterRef: ICriticalNotifyCompletion ref = ref this

    static member Current = holder.Value

module BindDepthCounter =
    [<Literal>]
    let bindLimit = 10

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

[<Struct>]
type DynamicState =
    | InitialYield
    | Running
    | SetResult
    | SetException of ExceptionDispatchInfo

[<Struct>]
type DynamicContinuation =
    | Stop
    | Immediate
    | Await of ICriticalNotifyCompletion
