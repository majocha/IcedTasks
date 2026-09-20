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
module InternalHelpers =

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
        let inline getAwaiter (awaitable: Awaitable<_, _, _>) = awaitable.GetAwaiter()

    [<Struct>]
    type Awaited<'T> = Awaited of 'T

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

    member inline _.Bind(Awaited result, [<InlineIfLambda>] continuation) = continuation result
    member inline _.ReturnFrom(Awaited result) = result
    member inline _.MergeSources(Awaited left, Awaited right) = Awaited (struct (left, right))

[<AutoOpen>]
module RuntimeAsyncBuilderExtensionsLowPriority =
    type RuntimeAsyncBuilder with
        member inline _.Source(awaitable: Awaitable<_, _, _>) =
            let awaiter = Awaitable.getAwaiter awaitable
            if not (Awaiter.isCompleted awaiter) then
                AsyncHelpers.UnsafeAwaitAwaiter awaiter
            Awaiter.getResult awaiter |> Awaited
        member inline this.Source(coldAwaitable: unit -> Awaitable<_, _, _>) = this.Source(coldAwaitable ())
        member inline this.Source(cancellableAwaitable: CancellationToken -> Awaitable<_, _, _>) = this.Source(cancellableAwaitable Cancellation.token.Value)

[<AutoOpen>]
module RuntimeAsyncBuilderExtensions =
    type RuntimeAsyncBuilder with
        member inline _.Source(sequence: seq<'T>) = sequence
        member inline _.Source(sequence: IAsyncEnumerable<'T>) = sequence
        member inline _.Source(task: Task) = task |> AsyncHelpers.Await |> Awaited
        member inline _.Source(task: ValueTask) = task |> AsyncHelpers.Await |> Awaited
        member inline _.Source(task: Task<'T>) = task |> AsyncHelpers.Await |> Awaited
        member inline _.Source(task: ValueTask<'T>) = task |> AsyncHelpers.Await |> Awaited

        member inline this.Source(coldTask: ColdTask<'T>) = this.Source(coldTask ())
        member inline this.Source(coldTask: ColdTask) = this.Source(coldTask ())
        member inline this.Source(coldTask: ColdValueTask<'T>) = this.Source(coldTask ())
        member inline this.Source(coldTask: ColdValueTask) = this.Source(coldTask ())
        member inline this.Source(cancellableTask: CancellableTask) = this.Source(cancellableTask Cancellation.token.Value)
        member inline this.Source(cancellableTask: CancellableTask<'T>) = this.Source(cancellableTask Cancellation.token.Value)
        member inline this.Source(cancellableTask: CancellableValueTask<'T>) = this.Source(cancellableTask Cancellation.token.Value)
        member inline this.Source(cancellableTask: CancellableValueTask) = this.Source(cancellableTask Cancellation.token.Value)
        member inline this.Source(computation: Async<'T>) = this.Source(Async.StartImmediateAsTask computation)


