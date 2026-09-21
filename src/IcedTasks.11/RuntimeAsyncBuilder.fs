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

open AwaitableHelpers

type Started<'T> = delegate of unit -> 'T

[<AutoOpen>]
module RuntimeAsyncBuilder =
    let inline isAlreadyBackground () =
        isNull SynchronizationContext.Current
        && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)

    [<NoEagerConstraintApplication>]
    let inline startAwaitable awaitable =
        let awaiter = Awaitable.getAwaiter awaitable

        Started(fun () ->
            AsyncHelpers.UnsafeAwaitAwaiter awaiter
            Awaiter.getResult awaiter
        )

type RuntimeAsyncBuilder() =

    member inline _.Delay([<InlineIfLambda>] generator: unit -> 'T) : unit -> 'T =
        fun () -> generator ()

    member inline _.Zero() = ()
    member inline _.Return(value: 'T) = value

    member inline _.Combine(first: unit, [<InlineIfLambda>] second) =
        ignore first
        second ()

    member inline _.Combine(first, [<InlineIfLambda>] second) =
        first ()
        second ()

    member inline _.TryWith
        ([<InlineIfLambda>] body: unit -> 'T, [<InlineIfLambda>] handler: exn -> 'T)
        =
        try
            body ()
        with error ->
            handler error

    member inline _.TryFinally
        ([<InlineIfLambda>] body: unit -> 'T, [<InlineIfLambda>] compensation: unit -> unit)
        =
        try
            body ()
        finally
            compensation ()

    member inline _.Using(resource, [<InlineIfLambda>] body) =
        try
            body resource
        finally
            match box resource with
            | :? IAsyncDisposable as disposable -> AsyncHelpers.Await(disposable.DisposeAsync())
            | :? IDisposable as disposable -> disposable.Dispose()
            | _ -> ()

    member inline _.While(guard: unit -> bool, [<InlineIfLambda>] body: unit -> unit) =
        while guard () do
            body ()

    member inline _.For(sequence: seq<'T>, [<InlineIfLambda>] body: 'T -> unit) =
        for item in sequence do
            body item

    member inline this.For(sequence: IAsyncEnumerable<'T>, [<InlineIfLambda>] body: 'T -> unit) =
        this.Using(
            sequence.GetAsyncEnumerator(),
            fun enumerator ->
                while enumerator.MoveNextAsync()
                      |> AsyncHelpers.Await do
                    body enumerator.Current
        )

    member inline _.Bind([<InlineIfLambda>] await: Started<'T>, [<InlineIfLambda>] continuation) =
        await.Invoke()
        |> continuation

    member inline _.ReturnFrom([<InlineIfLambda>] await: Started<'T>) = await.Invoke()

    member inline _.MergeSources
        ([<InlineIfLambda>] left: Started<'A>, [<InlineIfLambda>] right: Started<'B>)
        =
        Started(fun () ->
            let left = left.Invoke()
            let right = right.Invoke()
            struct (left, right)
        )

[<AutoOpen>]
module RuntimeAsyncBuilderAwaitableExtensions =
    type RuntimeAsyncBuilder with
        member inline _.Source(awaitable) = startAwaitable awaitable

        member inline this.Source([<InlineIfLambda>] coldAwaitable) =
            startAwaitable (coldAwaitable ())

        member inline this.Source([<InlineIfLambda>] cancellableAwaitable) =
            startAwaitable (cancellableAwaitable CancellationToken.None)

[<AutoOpen>]
module RuntimeAsyncBuilderSources =
    type RuntimeAsyncBuilder with

        // Accepted sources for For
        member inline _.Source(sequence: 'T seq) = sequence
        member inline _.Source(sequence: IAsyncEnumerable<'T>) = sequence

        // Cannonical runtime async Bind sources
        member inline _.Source(task: Task<'T>) =
            Started(fun () ->
                task
                |> AsyncHelpers.Await
            )

        member inline _.Source(task: Task) =
            Started(fun () ->
                task
                |> AsyncHelpers.Await
            )

        member inline _.Source(task: ValueTask<'T>) =
            Started(fun () ->
                task
                |> AsyncHelpers.Await
            )

        member inline _.Source(task: ValueTask) =
            Started(fun () ->
                task
                |> AsyncHelpers.Await
            )

        // Cold start sources
        member inline this.Source(computation: Async<'T>) =
            this.Source(Async.StartImmediateAsTask(computation))

        member inline this.Source([<InlineIfLambda>] coldTask: ColdTask<'T>) =
            this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] coldTask: ColdTask) = this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableTask) =
            this.Source(cancellableTask CancellationToken.None)

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableTask<'T>) =
            this.Source(cancellableTask CancellationToken.None)

        member inline this.Source([<InlineIfLambda>] coldTask: ColdValueTask<'T>) =
            this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] coldTask: ColdValueTask) =
            this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableValueTask<'T>) =
            this.Source(cancellableTask CancellationToken.None)

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableValueTask) =
            this.Source(cancellableTask CancellationToken.None)
