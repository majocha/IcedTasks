module RuntimeAsyncBuilder

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

module InternalHelpers =

    /// A structure that looks like an Awaiter
    type Awaiter<'Awaiter, 'TResult
        when 'Awaiter :> ICriticalNotifyCompletion
        and 'Awaiter: (member get_IsCompleted: unit -> bool)
        and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter

    type Awaitable<'Awaitable, 'Awaiter, 'TResult when 'Awaitable: (member GetAwaiter: unit -> Awaiter<'Awaiter, 'TResult>)> = 'Awaitable

    module Awaiter =
        let inline isCompleted (awaiter: Awaiter<_, _>) = awaiter.get_IsCompleted ()
        let inline getResult (awaiter: Awaiter<_, _>) = awaiter.GetResult()
        let inline onCompleted (awaiter: Awaiter<_, _>) continuation = awaiter.OnCompleted continuation
        let inline unsafeOnCompleted (awaiter: Awaiter<_, _>) continuation = awaiter.UnsafeOnCompleted continuation

    module Awaitable =
        let inline getAwaiter (awaitable: Awaitable<_, _, _>) = awaitable.GetAwaiter()

open InternalHelpers


[<RequireQualifiedAccess>]
module Cancellation =
    let token = AsyncLocal<CancellationToken>()

    let setToken ct = token.Value <- ct

    let inline throwIfCancellationRequested() =
        token.Value.ThrowIfCancellationRequested()

type RuntimeAsyncBuilder() =
    static let cancellationToken = AsyncLocal<CancellationToken>()


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
        this.Using(sequence.GetAsyncEnumerator(), fun enumerator ->
            while enumerator.MoveNextAsync() |> AsyncHelpers.Await do
                body enumerator.Current)

    member inline _.MergeSources(left, right) = struct(left, right)

    member inline _.Source(sequence: seq<'T>) = sequence
    member inline _.Source(sequence: IAsyncEnumerable<'T>) = sequence
    member inline _.Source(task: Task<'T>) = task
    member inline _.Source(task: Task) = task
    member inline _.Source(task: ValueTask<'T>) = task
    member inline _.Source(task: ValueTask) = task
    member inline _.Source(computation: Async<'T>) = Async.StartImmediateAsTask computation

    member inline _.Bind(task: Task<'T>, [<InlineIfLambda>] continuation) =
        task |> AsyncHelpers.Await |> continuation
    member inline _.Bind(task: Task, [<InlineIfLambda>] continuation) =
        task |> AsyncHelpers.Await |> continuation
    member inline _.Bind(task: ValueTask<'T>, [<InlineIfLambda>] continuation) =
        task |> AsyncHelpers.Await |> continuation
    member inline _.Bind(task: ValueTask, [<InlineIfLambda>] continuation) =
        task |> AsyncHelpers.Await |> continuation

    member inline _.ReturnFrom(source: Task<'T>) = AsyncHelpers.Await source
    member inline _.ReturnFrom(source: Task) = AsyncHelpers.Await source
    member inline _.ReturnFrom(source: ValueTask<'T>) = AsyncHelpers.Await source
    member inline _.ReturnFrom(source: ValueTask) = AsyncHelpers.Await source

    member inline _.Bind(awaiter: Awaiter<_, _>, [<InlineIfLambda>] continuation) =
        if not (Awaiter.isCompleted awaiter) then
            AsyncHelpers.AwaitAwaiter awaiter
        Awaiter.getResult awaiter |> continuation

[<AutoOpen>]
module RuntimeAsyncBuilderExtensions =
    type RuntimeAsyncBuilder with
        member inline _.Source(coldTask: ColdTask<'T>) = coldTask ()
        member inline _.Source(coldTask: ColdTask) = coldTask ()
        member inline _.Source(coldTask: ColdValueTask<'T>) = coldTask ()
        member inline _.Source(coldTask: ColdValueTask) = coldTask ()
        member inline _.Source(cancellableTask: CancellableTask) = cancellableTask Cancellation.token.Value
        member inline _.Source(cancellableTask: CancellableTask<'T>) = cancellableTask Cancellation.token.Value
        member inline _.Source(cancellableTask: CancellableValueTask<'T>) = cancellableTask Cancellation.token.Value
        member inline _.Source(cancellableTask: CancellableValueTask) = cancellableTask Cancellation.token.Value
        member inline _.Source(awaitable: Awaitable<_, _, _>) = Awaitable.getAwaiter awaitable
