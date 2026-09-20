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

#if NET6_0_OR_GREATER

open IcedTasks
open IcedTasks.TaskLike
open IcedTasks.ValueTasks
open IcedTasks.CancellableValueTasks

/// Contains methods to build pooling-backed CancellableValueTasks using the F# computation expression syntax.
[<AutoOpen>]
module CancellablePoolingValueTasks =

    open System
    open System.Runtime.CompilerServices
    open System.Threading
    open System.Threading.Tasks
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Collections

    /// Contains the cancellablePoolingValueTask computation expression builder.
    [<AutoOpen>]
    module CancellableValueTaskBuilder =

        /// <summary>
        /// Builds a cancellablePoolingValueTask using computation expression syntax.
        ///
        /// This utilizes <see cref="T:System.Runtime.CompilerServices.PoolingAsyncValueTaskMethodBuilder`1">System.Runtime.CompilerServices.PoolingAsyncValueTaskMethodBuilder</see>
        /// as described in <see href="https://devblogs.microsoft.com/dotnet/async-valuetask-pooling-in-net-5/">Async ValueTask Pooling in .NET 5</see>.
        /// </summary>
        ///
        /// <remarks>
        /// Instead of needing an attribute the compiler needs to know about like in <see href="https://github.com/dotnet/runtime/issues/49903">dotnet/runtime/issues/49903</see> this is a specific computation expression.
        /// </remarks>
        let cancellablePoolingValueTask = CancellableValueTaskBuilder()


        /// <summary>
        /// Builds a cancellablePoolingValueTask using computation expression syntax.
        ///
        /// This utilizes <see cref="T:System.Runtime.CompilerServices.PoolingAsyncValueTaskMethodBuilder`1">System.Runtime.CompilerServices.PoolingAsyncValueTaskMethodBuilder</see>
        /// as described in <see href="https://devblogs.microsoft.com/dotnet/async-valuetask-pooling-in-net-5/">Async ValueTask Pooling in .NET 5</see>.
        /// </summary>
        ///
        /// <remarks>
        /// Instead of needing an attribute the compiler needs to know about like in <see href="https://github.com/dotnet/runtime/issues/49903">dotnet/runtime/issues/49903</see> this is a specific computation expression.
        /// </remarks>
        let cancelablePVTask = cancellablePoolingValueTask

    // There is explicitly no Binds for `CancellableValueTasks` in `Microsoft.FSharp.Control.TaskBuilderBase`.
    // You need to explicitly pass in a `CancellationToken`to start it, you can use `CancellationToken.None`.
    // Reason is I don't want people to assume cancellation is happening without the caller being explicit about where the CancellationToken came from.
    // Similar reasoning for `IcedTasks.ColdTasks.ColdTaskBuilderBase`.

    /// Contains functional helper functions for composing and converting pooling-backed CancellableValueTask values.
    [<RequireQualifiedAccess>]
    module CancellableValueTask =

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
        ///         cancellableValueTask {
        ///             let! cancellationToken = CancellableValueTask.getCancellationToken()
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

        /// <summary>Lifts an item to a CancellableValueTask.</summary>
        /// <param name="item">The item to be the result of the CancellableValueTask.</param>
        /// <returns>A CancellableValueTask with the item as the result.</returns>
        let inline singleton (item: 'item) : CancellableValueTask<'item> =
            fun (ct: CancellationToken) -> ValueTask<'item> item


        /// <summary>Allows chaining of CancellableValueTasks.</summary>
        /// <param name="binder">The continuation.</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the binder.</returns>
        let inline bind
            ([<InlineIfLambda>] binder: 'input -> CancellableValueTask<'output>)
            ([<InlineIfLambda>] cTask: CancellableValueTask<'input>)
            =
            cancellablePoolingValueTask {
                let! cResult = cTask
                return! binder cResult
            }

        /// <summary>Allows chaining of CancellableValueTasks.</summary>
        /// <param name="mapper">The continuation.</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the mapper wrapped in a CancellableValueTasks.</returns>
        let inline map
            ([<InlineIfLambda>] mapper: 'input -> 'output)
            ([<InlineIfLambda>] cTask: CancellableValueTask<'input>)
            =
            cancellablePoolingValueTask {
                let! cResult = cTask
                return mapper cResult
            }

        /// <summary>Allows chaining of CancellableValueTasks.</summary>
        /// <param name="applicable">A function wrapped in a CancellableValueTasks</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the applicable.</returns>
        let inline apply
            ([<InlineIfLambda>] applicable: CancellableValueTask<'input -> 'output>)
            ([<InlineIfLambda>] cTask: CancellableValueTask<'input>)
            =
            cancellablePoolingValueTask {
                let! applier = applicable
                let! cResult = cTask
                return applier cResult
            }

        /// <summary>Takes two CancellableValueTasks, starts them serially in order of left to right, and returns a tuple of the pair.</summary>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>A tuple of the parameters passed in</returns>
        let inline zip
            ([<InlineIfLambda>] left: CancellableValueTask<'left>)
            ([<InlineIfLambda>] right: CancellableValueTask<'right>)
            =
            cancellablePoolingValueTask {
                let! r1 = left
                let! r2 = right
                return r1, r2
            }

        /// <summary>Takes two CancellableValueTasks, starts them concurrently with the same cancellation token, and returns a tuple of the pair.</summary>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>A tuple of the parameters passed in.</returns>
        /// <example id="cancellable-pooling-value-task-parallel-zip-1">
        /// <code lang="F#">
        /// let both =
        ///     CancellableValueTask.parallelZip firstOperation secondOperation
        ///
        /// let! left, right = both cancellationToken |> Async.AwaitValueTask
        /// </code>
        /// </example>
        let inline parallelZip
            ([<InlineIfLambda>] left: CancellableValueTask<'left>)
            ([<InlineIfLambda>] right: CancellableValueTask<'right>)
            =
            cancellablePoolingValueTask {
                let! ct = getCancellationToken ()
                let r1 = left ct
                let r2 = right ct
                let! r1 = r1
                let! r2 = r2
                return r1, r2
            }


        /// <summary>Converts a non-generic CancellableValueTask to a CancellableValueTask\&lt;unit\&gt;.</summary>
        /// <param name="unitCancellableTask">The CancellableValueTask to convert.</param>
        /// <returns>A CancellableValueTask\&lt;unit\&gt; that completes when <paramref name="unitCancellableTask" /> completes.</returns>
        let inline ofUnit ([<InlineIfLambda>] unitCancellableTask: CancellableValueTask) =
            cancellablePoolingValueTask { return! unitCancellableTask }

        /// <summary>Converts a CancellableValueTask\&lt;_&gt; to a non-generic CancellableValueTask by discarding the result.</summary>
        /// <param name="cancellableTask">The CancellableValueTask to convert.</param>
        /// <returns>A non-generic CancellableValueTask that completes when <paramref name="cancellableTask" /> completes.</returns>
        let inline toUnit
            ([<InlineIfLambda>] cancellableTask: CancellableValueTask<_>)
            : CancellableValueTask =
            fun ct ->
                cancellableTask ct
                |> ValueTask.toUnit
#endif
