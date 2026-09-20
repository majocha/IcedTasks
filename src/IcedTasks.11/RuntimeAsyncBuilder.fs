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
module AsyncHelpers =

    [<RequireQualifiedAccess>]
    module Cancellation =
        let token = AsyncLocal<CancellationToken>()

        let inline setToken ct = token.Value <- ct

        let inline throwIfCancellationRequested() =
            token.Value.ThrowIfCancellationRequested()

    let inline isAlreadyBackground () =
        isNull SynchronizationContext.Current && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)

    [<RequireQualifiedAccess>]
    type StartedAwaitable<'T> =
    | ValueTaskUnit of ValueTask
    | ValueTask of ValueTask<'T>
    | TaskUnit of Task
    | Task of Task<'T>

    let inline startAwaitable awaitable =
        __runtimeAsyncReturnValueTask(
            let awaiter = Awaitable.getAwaiter awaitable
            if not (Awaiter.isCompleted awaiter) then
                AsyncHelpers.AwaitAwaiter awaiter
            Awaiter.getResult awaiter)

    let inline await awaited =
        match awaited with
        | StartedAwaitable.ValueTaskUnit vt -> AsyncHelpers.Await vt; Unchecked.defaultof<_>
        | StartedAwaitable.ValueTask vt -> AsyncHelpers.Await vt
        | StartedAwaitable.TaskUnit t -> AsyncHelpers.Await t; Unchecked.defaultof<_>
        | StartedAwaitable.Task t -> AsyncHelpers.Await t


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

    member inline _.Bind(awaited: StartedAwaitable<'T>, [<InlineIfLambda>] continuation) = continuation (await awaited)

    member inline _.ReturnFrom(awaited: StartedAwaitable<'T>) = await awaited

[<AutoOpen>]
module RuntimeAsyncBuilderAwaitableExtensions =
    type RuntimeAsyncBuilder with
        member inline _.Source(awaitable) =
            startAwaitable awaitable |> StartedAwaitable.ValueTask

        member inline this.Source(coldAwaitable) =
            coldAwaitable () |> startAwaitable |> StartedAwaitable.ValueTask

        member inline this.Source(cancellableAwaitable) =
            cancellableAwaitable Cancellation.token.Value |> startAwaitable |> StartedAwaitable.ValueTask

        member inline _.MergeSources(left, right) =
            ValueTask.FromResult(struct(await left, await right)) |> StartedAwaitable.ValueTask

[<AutoOpen>]
module RuntimeAsyncBuilderSources =
    type RuntimeAsyncBuilder with
        member inline _.Source(sequence: 'T seq) = sequence
        member inline _.Source(sequence: IAsyncEnumerable<'T>) = sequence

        member inline _.Source(task: Task<'T>) = StartedAwaitable.Task task
        member inline _.Source(task: Task) = StartedAwaitable.TaskUnit task
        member inline _.Source(task: ValueTask<'T>) = StartedAwaitable.ValueTask task
        member inline _.Source(task: ValueTask) = StartedAwaitable.ValueTaskUnit task

        member inline _.Source(coldTask: ColdTask<'T>) = coldTask () |> StartedAwaitable.Task
        member inline _.Source(coldTask: ColdTask) = coldTask () |> StartedAwaitable.TaskUnit
        member inline _.Source(cancellableTask: CancellableTask) = cancellableTask Cancellation.token.Value |> StartedAwaitable.TaskUnit
        member inline _.Source(cancellableTask: CancellableTask<'T>) = cancellableTask Cancellation.token.Value |> StartedAwaitable.Task   
        member inline _.Source(coldTask: ColdValueTask<'T>) = coldTask () |> StartedAwaitable.ValueTask
        member inline _.Source(coldTask: ColdValueTask) = coldTask () |> StartedAwaitable.ValueTaskUnit
        member inline _.Source(cancellableTask: CancellableValueTask<'T>) = cancellableTask Cancellation.token.Value |> StartedAwaitable.ValueTask
        member inline _.Source(cancellableTask: CancellableValueTask) = cancellableTask Cancellation.token.Value |> StartedAwaitable.ValueTaskUnit

        member inline _.Source(computation: Async<'T>) = Async.StartImmediateAsTask(computation, Cancellation.token.Value) |> StartedAwaitable.Task
