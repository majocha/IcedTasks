namespace IcedTasks

open System
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Collections

[<AutoOpen>]
module ColdTaskTypes =
    /// CancellationToken -> Task<'T>
    type CancellableTask<'T> = CancellationToken -> Task<'T>
    /// CancellationToken -> Task
    type CancellableTask = CancellationToken -> Task
    /// CancellationToken -> ValueTask<'T>
    type CancellableValueTask<'T> = CancellationToken -> ValueTask<'T>
    /// CancellationToken -> ValueTask
    type CancellableValueTask = CancellationToken -> ValueTask
    /// unit -> Task<'T>
    type ColdTask<'T> = unit -> Task<'T>
    /// unit -> Task
    type ColdTask = unit -> Task
    /// unit -> ValueTask<'T>
    type ColdValueTask<'T> = unit -> ValueTask<'T>
    /// unit -> ValueTask
    type ColdValueTask = unit -> ValueTask

[<AutoOpen>]
module AwaitableHelpers =

    /// A structure that looks like an Awaiter
    type Awaiter<'Awaiter, 'TResult
        when 'Awaiter :> ICriticalNotifyCompletion
        and 'Awaiter: (member get_IsCompleted: unit -> bool)
        and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter

    type Awaitable<'Awaitable, 'Awaiter, 'TResult when 'Awaitable: (member GetAwaiter: unit -> Awaiter<'Awaiter, 'TResult>)> = 'Awaitable
    type ColdAwaitable<'Awaitable, 'Awaiter, 'TResult when 'Awaitable: (member GetAwaiter: unit -> Awaiter<'Awaiter, 'TResult>)> =
        unit -> 'Awaitable
    type CancellableAwaitable<'Awaitable, 'Awaiter, 'TResult when 'Awaitable: (member GetAwaiter: unit -> Awaiter<'Awaiter, 'TResult>)> =
        CancellationToken -> 'Awaitable

    module Awaiter =
        let inline isCompleted (awaiter: Awaiter<_, _>) = awaiter.get_IsCompleted ()
        let inline getResult (awaiter: Awaiter<_, _>) = awaiter.GetResult()
        let inline onCompleted (awaiter: Awaiter<_, _>) continuation = awaiter.OnCompleted continuation
        let inline unsafeOnCompleted (awaiter: Awaiter<_, _>) continuation = awaiter.UnsafeOnCompleted continuation

    module Awaitable =
        let inline getAwaiter (awaitable: Awaitable<_, _, _>) =
            awaitable.GetAwaiter()

[<AutoOpen>]
module RuntimeAsyncBuilder =

    [<RequireQualifiedAccess>]
    module Cancellation =
        let token = AsyncLocal<CancellationToken>()

        let inline setToken ct = token.Value <- ct

        let inline throwIfCancellationRequested() =
            token.Value.ThrowIfCancellationRequested()

    let inline isAlreadyBackground () =
        isNull SynchronizationContext.Current && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)

    type Started<'T> = delegate of unit -> 'T

    [<NoEagerConstraintApplication>]
    let inline startAwaitable awaitable =
        let awaiter = Awaitable.getAwaiter awaitable
        Started(fun () ->
            AsyncHelpers.UnsafeAwaitAwaiter awaiter
            Awaiter.getResult awaiter)


type RuntimeAsyncBuilder() =

    member inline _.Delay([<InlineIfLambda>] generator: unit -> 'T) : unit -> 'T =
        fun () ->
            Cancellation.throwIfCancellationRequested()
            generator()

    member inline _.Zero() = ()
    member inline _.Return(value: 'T) = value

    member inline _.Combine(first: unit, [<InlineIfLambda>] second) =
        ignore first
        second()

    member inline _.Combine(first, [<InlineIfLambda>] second) =
        first()
        second()
    member inline _.TryWith([<InlineIfLambda>] body: unit -> 'T, [<InlineIfLambda>] handler: exn -> 'T) =
        try body() with error -> handler error
    member inline _.TryFinally([<InlineIfLambda>] body: unit -> 'T, [<InlineIfLambda>] compensation: unit -> unit) =
        try body() finally compensation()
    member inline _.Using(resource, [<InlineIfLambda>] body) =
        try
            body resource
        finally
            match box resource with
            | :? IAsyncDisposable as disposable -> AsyncHelpers.Await(disposable.DisposeAsync())
            | :? IDisposable as disposable -> disposable.Dispose()
            | _ -> ()

    member inline _.While(guard: unit -> bool, [<InlineIfLambda>] body: unit -> unit) =
        while guard() do body()

    member inline _.For(sequence: seq<'T>, [<InlineIfLambda>] body: 'T -> unit) =
        for item in sequence do body item

    member inline this.For(sequence: IAsyncEnumerable<'T>, [<InlineIfLambda>] body: 'T -> unit) =
        this.Using(sequence.GetAsyncEnumerator(Cancellation.token.Value), fun enumerator ->
            while enumerator.MoveNextAsync() |> AsyncHelpers.Await do
                body enumerator.Current)

    member inline _.Bind([<InlineIfLambda>] await: Started<'T>, [<InlineIfLambda>] continuation) = await.Invoke() |> continuation

    member inline _.ReturnFrom([<InlineIfLambda>] await: Started<'T>) = await.Invoke()

    member inline _.MergeSources([<InlineIfLambda>] left: Started<'A>, [<InlineIfLambda>] right: Started<'B>) =
        Started(fun () ->
            let left = left.Invoke()
            let right = right.Invoke()
            struct (left, right))

[<AutoOpen>]
module RuntimeAsyncBuilderAwaitableExtensions =
    type RuntimeAsyncBuilder with
        member inline _.Source(awaitable) = startAwaitable awaitable

        member inline this.Source([<InlineIfLambda>] coldAwaitable) = startAwaitable (coldAwaitable ())

        member inline this.Source([<InlineIfLambda>] cancellableAwaitable) = startAwaitable (cancellableAwaitable Cancellation.token.Value)


[<AutoOpen>]
module RuntimeAsyncBuilderSources =
    type RuntimeAsyncBuilder with

        // Cold start sources
        member inline _.Source([<InlineIfLambda>] coldTask: ColdTask<'T>) =
            let task = coldTask () in Started (fun () -> task |> AsyncHelpers.Await)    

        member inline _.Source([<InlineIfLambda>] coldTask: ColdTask) =
            let task = coldTask () in Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source([<InlineIfLambda>] cancellableTask: CancellableTask) =
            let task = cancellableTask Cancellation.token.Value in Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source([<InlineIfLambda>] cancellableTask: CancellableTask<'T>) =
            let task = cancellableTask Cancellation.token.Value in Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source([<InlineIfLambda>] coldTask: ColdValueTask<'T>) =
            let task = coldTask () in Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source([<InlineIfLambda>] coldTask: ColdValueTask) =
            let task = coldTask () in Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source([<InlineIfLambda>] cancellableTask: CancellableValueTask<'T>) =
            let task = cancellableTask Cancellation.token.Value in Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source([<InlineIfLambda>] cancellableTask: CancellableValueTask) =
            let task = cancellableTask Cancellation.token.Value in Started (fun () -> task |> AsyncHelpers.Await)

        // Accepted sources for For
        member inline _.Source(sequence: 'T seq) = sequence
        member inline _.Source(sequence: IAsyncEnumerable<'T>) = sequence

        // Cannonical runtime async Bind sources
        member inline _.Source(task: Task<'T>) = Started (fun () -> task |> AsyncHelpers.Await)
        member inline _.Source(task: Task) = Started (fun () -> task |> AsyncHelpers.Await)
        member inline _.Source(task: ValueTask<'T>) = Started (fun () -> task |> AsyncHelpers.Await)
        member inline _.Source(task: ValueTask) = Started (fun () -> task |> AsyncHelpers.Await)

        member inline _.Source(computation: Async<'T>) =
            let task = Async.StartImmediateAsTask(computation, Cancellation.token.Value)
            Started( fun () -> task |> AsyncHelpers.Await)
