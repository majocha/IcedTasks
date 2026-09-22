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

module CancellableRuntimeAsyncBuilderHelpers =

    // A delegate to unify dissimilar builder source types, this allows us to have no additional Bind or MergeSources overloads.
    // The delegate takes the cancellation token threaded through the builder; its invocation is inlined, so this is zero cost.
    type Started<'T> = delegate of unit -> 'T
    // We need to distinguish between hot and cold awaitables, we can pass the cancellation token only to the cold ones.
    // Ideally the signature should be CancellationToken -> Started<'T>, but the Started<_> delegates execute AsyncHelpers.Await
    // and must be inlined unconditionally into async method body.
    type Cancellable<'T> = delegate of CancellationToken -> 'T

    [<NoEagerConstraintApplication>]
    let inline startAwaitable awaitable =
        let awaiter = Awaitable.getAwaiter awaitable

        Started(fun () ->
            AsyncHelpers.UnsafeAwaitAwaiter awaiter
            Awaiter.getResult awaiter
        )

    [<NoEagerConstraintApplication>]
    let inline startCancellableAwaitable cancellableAwaitable =
        Cancellable(fun ct ->
            let awaiter =
                cancellableAwaitable ct
                |> Awaitable.getAwaiter

            AsyncHelpers.UnsafeAwaitAwaiter awaiter
            Awaiter.getResult awaiter
        )

open CancellableRuntimeAsyncBuilderHelpers

module CancellableRuntimeAsyncBuilder =
    let inline isAlreadyBackground () =
        isNull SynchronizationContext.Current
        && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)

type CancellableRuntimeAsyncBuilder() =

    // The code type of the builder is `CancellationToken -> 'T`, i.e. the cancellation token is
    // passed along as state to every delayed continuation.
    member inline _.Delay
        ([<InlineIfLambda>] generator: unit -> CancellationToken -> 'T)
        : CancellationToken -> 'T =
        fun ct ->
            ct.ThrowIfCancellationRequested()
            generator () ct

    member inline _.Zero() : CancellationToken -> unit = fun _ -> ()

    member inline _.Return(value: 'T) : CancellationToken -> 'T = fun _ -> value

    member inline _.Combine
        (
            [<InlineIfLambda>] first: CancellationToken -> 'A,
            [<InlineIfLambda>] second: CancellationToken -> 'T
        ) : CancellationToken -> 'T =
        fun ct ->
            first ct
            |> ignore

            second ct

    member inline _.TryWith
        (
            [<InlineIfLambda>] body: CancellationToken -> 'T,
            [<InlineIfLambda>] handler: exn -> CancellationToken -> 'T
        ) : CancellationToken -> 'T =
        fun ct ->
            try
                body ct
            with error ->
                handler error ct

    member inline _.TryFinally
        (
            [<InlineIfLambda>] body: CancellationToken -> 'T,
            [<InlineIfLambda>] compensation: unit -> unit
        ) : CancellationToken -> 'T =
        fun ct ->
            try
                body ct
            finally
                compensation ()

    member inline _.Using
        (resource: #IDisposable | null, [<InlineIfLambda>] body: 'T -> CancellationToken -> 'U)
        : CancellationToken -> 'U =
        fun ct ->
            try
                body resource ct
            finally
                if not (isNull (box resource)) then resource.Dispose()

    member inline _.While
        (guard: unit -> bool, [<InlineIfLambda>] body: CancellationToken -> unit)
        : CancellationToken -> unit =
        fun ct ->
            while guard () do
                body ct

    member inline _.For
        (sequence: seq<'T>, [<InlineIfLambda>] body: 'T -> CancellationToken -> unit)
        : CancellationToken -> unit =
        fun ct ->
            for item in sequence do
                body item ct

    member inline _.Bind
        (
            [<InlineIfLambda>] awaited: Started<'T>,
            [<InlineIfLambda>] continuation: 'T -> CancellationToken -> 'U
        ) : CancellationToken -> 'U =
        fun ct -> continuation (awaited.Invoke()) ct

    member inline this.Bind
        (
            [<InlineIfLambda>] cancellable: Cancellable<'T>,
            [<InlineIfLambda>] continuation: 'T -> CancellationToken -> 'U
        ) : CancellationToken -> 'U =
        fun ct -> continuation (cancellable.Invoke ct) ct

    member inline _.ReturnFrom([<InlineIfLambda>] awaited: Started<'T>) : CancellationToken -> 'T =
        fun ct -> awaited.Invoke()

    member inline _.ReturnFrom
        ([<InlineIfLambda>] cancellable: Cancellable<'T>)
        : CancellationToken -> 'T =
        fun ct -> cancellable.Invoke ct

    member inline _.MergeSources
        ([<InlineIfLambda>] left: Started<'A>, [<InlineIfLambda>] right: Started<'B>)
        =
        let left = left.Invoke()
        let right = right.Invoke()
        Started(fun () -> struct (left, right))

    member inline this.MergeSources
        ([<InlineIfLambda>] left: Cancellable<'A>, [<InlineIfLambda>] right: Cancellable<'B>)
        =
        Cancellable(fun ct ->
            let right = __runtimeAsyncReturnValueTask (right.Invoke ct)
            let left = left.Invoke ct

            struct (left,
                    right
                    |> AsyncHelpers.Await)
        )

    member inline this.MergeSources
        ([<InlineIfLambda>] left: Started<'A>, [<InlineIfLambda>] right: Cancellable<'B>)
        =
        Cancellable(fun ct ->
            let right = right.Invoke ct
            let left = left.Invoke()
            struct (left, right)
        )

    member inline this.MergeSources
        ([<InlineIfLambda>] left: Cancellable<'A>, [<InlineIfLambda>] right: Started<'B>)
        =
        Cancellable(fun ct ->
            let left = left.Invoke ct
            let right = right.Invoke()
            struct (left, right)
        )

[<AutoOpen>]
module CancellableRuntimeAsyncBuilderAsyncDisposableExtensions =
    type CancellableRuntimeAsyncBuilder with
        member inline _.Using
            (resource: #IAsyncDisposable | null, [<InlineIfLambda>] body: 'T -> CancellationToken -> 'U)
            : CancellationToken -> 'U =
            fun ct ->
                try
                    body resource ct
                finally
                    if not (isNull (box resource)) then
                        resource.DisposeAsync() |> AsyncHelpers.Await

        member inline this.For
            (sequence: IAsyncEnumerable<'T>, [<InlineIfLambda>] body: 'T -> CancellationToken -> unit)
            : CancellationToken -> unit =
            fun ct ->
                this.Using
                    (sequence.GetAsyncEnumerator ct,
                     fun enumerator ct ->
                         while enumerator.MoveNextAsync()
                               |> AsyncHelpers.Await do
                             body enumerator.Current ct)
                    ct

[<AutoOpen>]
module CancellableRuntimeAsyncBuilderAwaitableExtensions =
    type CancellableRuntimeAsyncBuilder with
        member inline _.Source(awaitable) = startAwaitable awaitable

        member inline this.Source([<InlineIfLambda>] coldAwaitable) =
            startAwaitable (coldAwaitable ())

        member inline this.Source([<InlineIfLambda>] cancellableAwaitable) =
            startCancellableAwaitable cancellableAwaitable

[<AutoOpen>]
module CancellableRuntimeAsyncBuilderSources =
    type CancellableRuntimeAsyncBuilder with

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
            Cancellable(fun ct ->
                Async.StartImmediateAsTask(computation, ct)
                |> AsyncHelpers.Await
            )

        member inline this.Source([<InlineIfLambda>] coldTask: ColdTask<'T>) =
            this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] coldTask: ColdTask) = this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableTask) =
            Cancellable(fun ct ->
                cancellableTask ct
                |> AsyncHelpers.Await
            )

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableTask<'T>) =
            Cancellable(fun ct ->
                cancellableTask ct
                |> AsyncHelpers.Await
            )

        member inline this.Source([<InlineIfLambda>] coldTask: ColdValueTask<'T>) =
            this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] coldTask: ColdValueTask) =
            this.Source(coldTask ())

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableValueTask<'T>) =
            Cancellable(fun ct ->
                cancellableTask ct
                |> AsyncHelpers.Await
            )

        member inline this.Source([<InlineIfLambda>] cancellableTask: CancellableValueTask) =
            Cancellable(fun ct ->
                cancellableTask ct
                |> AsyncHelpers.Await
            )
