// Task builder for F# that compiles to allocation-free paths for synchronous code.
//
// Originally written in 2016 by Robert Peele (humbobst@gmail.com)
// New operator-based overload resolution for F# 4.0 compatibility by Gustavo Leon in 2018.
// Revised for insertion into FSharp.Core by Microsoft, 2019.
// Revised to implement CancellationToken semantics
//
// Original notice:
// To the extent possible under law, the author(s) have dedicated all copyright and related and neighboring rights
// to this software to the public domain worldwide. This software is distributed without any warranty.

namespace IcedTasks

open IcedTasks.TaskLike
open IcedTasks.CancellablePoolingValueTasks

/// Contains methods to build CancellableTasks using the F# computation expression syntax
[<AutoOpen>]
module CancellableTasks =

    open System
    open System.Runtime.CompilerServices
    open System.Threading
    open System.Threading.Tasks
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Collections
    open IcedTasks

    /// Contains methods to build CancellableTasks using the F# computation expression syntax
    type CancellableTaskBuilder() =

        inherit RuntimeAsyncBuilder()

        member inline _.Run([<InlineIfLambda>] code) : CancellableTask<'T> =
            fun ct -> __runtimeAsyncReturn(
                Cancellation.setToken ct
                code())


    /// Contains methods to build CancellableTasks using the F# computation expression syntax
    type BackgroundCancellableTaskBuilder() =

        inherit RuntimeAsyncBuilder()

        member inline _.Run([<InlineIfLambda>] code) : CancellableTask<'T> =
            fun ct ->
                Task.Run( fun () ->
                    __runtimeAsyncReturn(
                    Cancellation.setToken ct
                    code()))

    /// Contains the cancellableTask computation expressions.
    [<AutoOpen>]
    module CancellableTaskBuilder =

        let cancellableTask = CancellableTaskBuilder()
        let backgroundCancellableTask = BackgroundCancellableTaskBuilder()

    [<AutoOpen>]
    module HighPriority =

        type AsyncEx with

            static member inline AwaitCancellableTask
                ([<InlineIfLambda>] t: CancellableTask<'T>)
                =
                asyncEx {
                    let! ct = Async.CancellationToken
                    return! t ct
                }

            static member inline AwaitCancellableTask
                ([<InlineIfLambda>] t: CancellableTask)
                =
                asyncEx {
                    let! ct = Async.CancellationToken
                    return! t ct
                }

        type Microsoft.FSharp.Control.Async with

            static member inline AwaitCancellableTask
                ([<InlineIfLambda>] t: CancellableTask<'T>)
                =
                async {
                    let! ct = Async.CancellationToken
                    return! t ct |> Async.AwaitTask
                }

            static member inline AwaitCancellableTask
                ([<InlineIfLambda>] t: CancellableTask)
                =
                async {
                    let! ct = Async.CancellationToken
                    return! t ct |> Async.AwaitTask
                }

            static member inline AsCancellableTask
                (computation: Async<'T>)
                : CancellableTask<'T> =
                fun ct -> Async.StartAsTask(computation, cancellationToken = ct)

    /// <summary>
    /// A set of extension methods making it possible to bind against <see cref='T:IcedTasks.CancellableTasks.CancellableTask`1'/> in async computations.
    /// </summary>
    [<AutoOpen>]
    module AsyncExtensions =

        type AsyncExBuilder with

            member inline this.Source([<InlineIfLambda>] t: CancellableTask<'T>) : Async<'T> =
                AsyncEx.AwaitCancellableTask t

            member inline this.Source([<InlineIfLambda>] t: CancellableTask) : Async<unit> =
                AsyncEx.AwaitCancellableTask t

        type Microsoft.FSharp.Control.AsyncBuilder with

            member inline this.Bind
                (
                    [<InlineIfLambda>] t: CancellableTask<'T>,
                    [<InlineIfLambda>] binder: ('T -> Async<'U>)
                ) : Async<'U> =
                this.Bind(Async.AwaitCancellableTask t, binder)

            member inline this.ReturnFrom([<InlineIfLambda>] t: CancellableTask<'T>) : Async<'T> =
                this.ReturnFrom(Async.AwaitCancellableTask t)

            member inline this.Bind
                (
                    [<InlineIfLambda>] t: CancellableTask,
                    [<InlineIfLambda>] binder: (unit -> Async<'U>)
                ) : Async<'U> =
                this.Bind(Async.AwaitCancellableTask t, binder)

            member inline this.ReturnFrom([<InlineIfLambda>] t: CancellableTask) : Async<unit> =
                this.ReturnFrom(Async.AwaitCancellableTask t)

    // There is explicitly no Binds for `CancellableTasks` in `Microsoft.FSharp.Control.TaskBuilderBase`.
    // You need to explicitly pass in a `CancellationToken`to start it, you can use `CancellationToken.None`.
    // Reason is I don't want people to assume cancellation is happening without the caller being explicit about where the CancellationToken came from.
    // Similar reasoning for `IcedTasks.ColdTasks.ColdTaskBuilderBase`.

    /// Contains functional helper functions for composing and converting CancellableTask values.
    [<RequireQualifiedAccess>]
    module CancellableTask =

        /// <summary>Gets the default cancellation token for executing computations.</summary>
        ///
        /// <returns>The default CancellationToken.</returns>
        ///
        /// <category index="3">Cancellation and Exceptions</category>
        ///
        /// <example id="default-cancellation-token-1">
        /// <code lang="F#">
        /// use tokenSource = new CancellationTokenSource()
        /// let primes = [ 2; 3; 5; 7; 11 ]
        /// for i in primes do
        ///     let computation =
        ///         cancellableTask {
        ///             let! cancellationToken = CancellableTask.getCancellationToken()
        ///             do! Task.Delay(i * 1000, cancellationToken)
        ///             printfn $"{i}"
        ///         }
        ///     computation tokenSource.Token |> ignore
        /// Thread.Sleep(6000)
        /// tokenSource.Cancel()
        /// printfn "Tasks Finished"
        /// </code>
        /// This will print "2" 2 seconds from start, "3" 3 seconds from start, "5" 5 seconds from start, cease computation and then
        /// followed by "Tasks Finished".
        /// </example>
        let inline getCancellationToken () =
            fun (ct: CancellationToken) -> ValueTask<CancellationToken> ct

        /// <summary>Lifts an item to a CancellableTask.</summary>
        /// <param name="item">The item to be the result of the CancellableTask.</param>
        /// <returns>A CancellableTask with the item as the result.</returns>
        let inline singleton (item: 'item) : CancellableTask<'item> = fun _ -> Task.FromResult(item)


        /// <summary>Allows chaining of CancellableTasks.</summary>
        /// <param name="binder">The continuation.</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the binder.</returns>
        let inline bind
            ([<InlineIfLambda>] binder: 'input -> CancellableTask<'output>)
            ([<InlineIfLambda>] cTask: CancellableTask<'input>)
            =
            cancellableTask {
                let! cResult = cTask
                return! binder cResult
            }

        /// <summary>Allows chaining of CancellableTasks.</summary>
        /// <param name="mapper">The continuation.</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the mapper wrapped in a CancellableTasks.</returns>
        let inline map
            ([<InlineIfLambda>] mapper: 'input -> 'output)
            ([<InlineIfLambda>] cTask: CancellableTask<'input>)
            =
            cancellableTask {
                let! cResult = cTask
                return mapper cResult
            }

        /// <summary>Allows chaining of CancellableTasks.</summary>
        /// <param name="applicable">A function wrapped in a CancellableTasks</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the applicable.</returns>
        let inline apply<'input, 'output>
            ([<InlineIfLambda>] applicable: CancellableTask<'input -> 'output>)
            ([<InlineIfLambda>] cTask: CancellableTask<'input>)
            =
            cancellableTask {
                let! (applier: 'input -> 'output) = applicable
                let! (cResult: 'input) = cTask
                return applier cResult
            }

        /// <summary>Takes two CancellableTasks, starts them serially in order of left to right, and returns a tuple of the pair.</summary>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>A tuple of the parameters passed in</returns>
        let inline zip
            ([<InlineIfLambda>] left: CancellableTask<'left>)
            ([<InlineIfLambda>] right: CancellableTask<'right>)
            =
            cancellableTask {
                let! r1 = left
                let! r2 = right
                return r1, r2
            }

        /// <summary>Takes two CancellableTasks, starts them concurrently with the same cancellation token, and returns a tuple of the pair.</summary>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>A tuple of the parameters passed in.</returns>
        /// <example id="cancellable-task-parallel-zip-1">
        /// <code lang="F#">
        /// let both =
        ///     CancellableTask.parallelZip firstOperation secondOperation
        ///
        /// let! left, right = both cancellationToken
        /// </code>
        /// </example>
        let inline parallelZip
            ([<InlineIfLambda>] left: CancellableTask<'left>)
            ([<InlineIfLambda>] right: CancellableTask<'right>)
            =
            cancellableTask {
                let! ct = getCancellationToken ()
                let r1 = left ct
                let r2 = right ct
                let! r1 = r1
                let! r2 = r2
                return r1, r2
            }


        /// <summary>Creates a task that will complete when all of the <see cref='T:IcedTasks.CancellableTasks.CancellableTask`1'/> values in an enumerable collection have completed.</summary>
        /// <param name="tasks">The tasks to wait on for completion</param>
        /// <returns>A CancellableTask that represents the completion of all of the supplied tasks.</returns>
        /// <exception cref="T:System.ArgumentNullException">The <paramref name="tasks" /> argument was <see langword="null" />.</exception>
        /// <exception cref="T:System.ArgumentException">The <paramref name="tasks" /> collection contained a <see langword="null" /> task.</exception>
        let inline whenAll (tasks: CancellableTask<_> seq) =
            cancellableTask {
                let! ct = getCancellationToken ()

                let! results =
                    tasks
                    |> Seq.map (fun t -> t ct)
                    |> Task.WhenAll

                return results
            }

        /// <summary>Creates a task that runs the supplied CancellableTasks with a maximum degree of parallelism and completes when all have completed.</summary>
        /// <param name="tasks">The tasks to wait on for completion</param>
        /// <param name="maxDegreeOfParallelism">The maximum number of tasks to run concurrently.</param>
        /// <returns>A CancellableTask that represents the completion of all of the supplied tasks.</returns>
        /// <exception cref="T:System.ArgumentNullException">The <paramref name="tasks" /> argument was <see langword="null" />.</exception>
        /// <exception cref="T:System.ArgumentException">The <paramref name="tasks" /> collection contained a <see langword="null" /> task.</exception>
        /// <example id="cancellable-task-when-all-throttled-1">
        /// <code lang="F#">
        /// let loadAll ids =
        ///     ids
        ///     |> Seq.map loadOne
        ///     |> CancellableTask.whenAllThrottled 4
        /// </code>
        /// </example>
        let inline whenAllThrottled (maxDegreeOfParallelism: int) (tasks: CancellableTask<_> seq) =
            cancellableTask {
                let! ct = getCancellationToken ()

                use semaphore =
                    new SemaphoreSlim(
                        initialCount = maxDegreeOfParallelism,
                        maxCount = maxDegreeOfParallelism
                    )

                let! results =
                    tasks
                    |> Seq.map (fun t ->
                        task {
                            do! semaphore.WaitAsync ct

                            try
                                return! t ct
                            finally
                                semaphore.Release()
                                |> ignore

                        }
                    )
                    |> Task.WhenAll

                return results
            }

        /// <summary>Creates a <see cref='T:IcedTasks.CancellableTasks.CancellableTask`1'/> that will complete when all of the <see cref='T:IcedTasks.CancellableTasks.CancellableTask`1'/>s in an enumerable collection have completed sequentially.</summary>
        /// <param name="tasks">The tasks to wait on for completion</param>
        /// <returns>A CancellableTask that represents the completion of all of the supplied tasks.</returns>
        let inline sequential (tasks: CancellableTask<'a> seq) =
            cancellableTask {
                let mutable results = ArrayCollector<'a>()

                for t in tasks do
                    let! result = t
                    results.Add result

                return results.Close()
            }


        /// <summary>Converts a non-generic CancellableTask to a CancellableTask\&lt;unit\&gt;.</summary>
        /// <param name="unitCancellableTask">The CancellableTask to convert.</param>
        /// <returns>A CancellableTask\&lt;unit\&gt; that completes when <paramref name="unitCancellableTask" /> completes.</returns>
        let inline ofUnit ([<InlineIfLambda>] unitCancellableTask: CancellableTask) =
            cancellableTask { return! unitCancellableTask }

        /// <summary>Converts a CancellableTask\&lt;_&gt; to a non-generic CancellableTask by discarding the result.</summary>
        /// <param name="ctask">The CancellableTask to convert.</param>
        /// <returns>A non-generic CancellableTask that completes when <paramref name="ctask" /> completes.</returns>
        let inline toUnit ([<InlineIfLambda>] ctask: CancellableTask<_>) : CancellableTask =
            fun ct -> ctask ct
