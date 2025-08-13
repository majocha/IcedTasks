namespace IcedTasks

open System
open System.Runtime.ExceptionServices
open System.Threading
open System.Runtime.CompilerServices

type Trampoline private () =

    let ownerThreadId = Thread.CurrentThread.ManagedThreadId

    let mutable bindCount = 0

    [<Literal>]
    let bindLimit = 100

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
        assert next.IsNone

        next <- ValueSome action
        executing <- true
        loop ()

    let set action =
        assert (Thread.CurrentThread.ManagedThreadId = ownerThreadId)

        bindCount <- 0
        if executing then next <- ValueSome action else start action

    static let current = new ThreadLocal<Trampoline>(fun () -> Trampoline())

    static member Current = current.Value

    member _.CheckBindLimit() =
        bindCount <-
            bindCount
            + 1

        bindCount
        >= bindLimit

    member val Awaiter =
        { new ICriticalNotifyCompletion with
            member this.OnCompleted(continuation: Action) = set continuation
            member this.UnsafeOnCompleted(continuation: Action) = set continuation
        }

module ExceptionCache =
    let private store = ConditionalWeakTable<exn, ExceptionDispatchInfo>()

    let Throw (exn: exn) =
        match store.TryGetValue exn with
        | true, edi when edi.SourceException = exn -> edi.Throw()
        | _ ->
            let edi = ExceptionDispatchInfo.Capture exn

            try
                store.Add(exn, edi)
            with _ ->
                () //

            edi.Throw()

        Unchecked.defaultof<_>
