namespace IcedTasks.PoolingValueTasks

open IcedTasks
open RuntimeAsyncBuilder
open IcedTasks.ValueTasks

/// Contains methods to build PoolingValueTasks using the F# computation expression syntax
[<AutoOpen>]
module PoolingValueTasks =
    open System
    open System.Runtime.CompilerServices
    open System.Threading.Tasks
    open Microsoft.FSharp.Core.CompilerServices

    /// Contains the poolingValueTask computation expression builder.
    [<AutoOpen>]
    module ValueTaskBuilder =

        /// <summary>
        /// Builds a poolingValueTask using computation expression syntax.
        /// </summary>
        let poolingValueTask = ValueTaskBuilder()

        /// <summary>
        /// Alias for <see cref="F:IcedTasks.PoolingValueTasks.PoolingValueTasks.ValueTaskBuilder.poolingValueTask" />.
        /// </summary>
        let pvTask = poolingValueTask

        /// Contains functional helper functions for composing and converting pooling-backed <see cref="T:System.Threading.Tasks.ValueTask`1" /> values.
        [<RequireQualifiedAccess>]
        module ValueTask =
            open System.Threading.Tasks

            /// <summary>Lifts an item to a ValueTask.</summary>
            /// <param name="item">The item to be the result of the ValueTask.</param>
            /// <returns>A ValueTask with the item as the result.</returns>
            let inline singleton (item: 'item) : ValueTask<'item> = ValueTask<'item> item


            /// <summary>Allows chaining of PoolingValueTasks.</summary>
            /// <param name="binder">The continuation.</param>
            /// <param name="cTask">The value.</param>
            /// <returns>The result of the binder.</returns>
            let inline bind
                ([<InlineIfLambda>] binder: 'input -> ValueTask<'output>)
                (cTask: ValueTask<'input>)
                =
                poolingValueTask {
                    let! cResult = cTask
                    return! binder cResult
                }

            /// <summary>Allows chaining of PoolingValueTasks.</summary>
            /// <param name="mapper">The continuation.</param>
            /// <param name="cTask">The value.</param>
            /// <returns>The result of the mapper wrapped in a PoolingValueTasks.</returns>
            let inline map
                ([<InlineIfLambda>] mapper: 'input -> 'output)
                (cTask: ValueTask<'input>)
                =
                poolingValueTask {
                    let! cResult = cTask
                    return mapper cResult
                }

            /// <summary>Allows chaining of PoolingValueTasks.</summary>
            /// <param name="applicable">A function wrapped in a PoolingValueTasks</param>
            /// <param name="cTask">The value.</param>
            /// <returns>The result of the applicable.</returns>
            let inline apply (applicable: ValueTask<'input -> 'output>) (cTask: ValueTask<'input>) =
                poolingValueTask {
                    let! applier = applicable
                    let! cResult = cTask
                    return applier cResult
                }

            /// <summary>Takes two PoolingValueTasks, starts them serially in order of left to right, and returns a tuple of the pair.</summary>
            /// <param name="left">The left value.</param>
            /// <param name="right">The right value.</param>
            /// <returns>A tuple of the parameters passed in</returns>
            let inline zip (left: ValueTask<'left>) (right: ValueTask<'right>) =
                poolingValueTask {
                    let! r1 = left
                    let! r2 = right
                    return r1, r2
                }

            /// <summary>Converts a non-generic <see cref="T:System.Threading.Tasks.ValueTask" /> to a pooling-backed <see cref="T:System.Threading.Tasks.ValueTask`1" /> of unit.</summary>
            /// <param name="vtask">The non-generic ValueTask to convert.</param>
            /// <returns>A ValueTask whose result is unit.</returns>
            let inline ofUnit (vtask: ValueTask) : ValueTask<unit> =
                // this implementation follows Stephen Toub's advice, see:
                // https://github.com/dotnet/runtime/issues/31503#issuecomment-554415966
                if vtask.IsCompletedSuccessfully then
                    ValueTask<unit>()
                else
                    poolingValueTask { return! vtask }

            /// <summary>Wraps a <see cref="T:System.Threading.Tasks.Task`1" /> as a <see cref="T:System.Threading.Tasks.ValueTask`1" />.</summary>
            /// <param name="task">The task to wrap.</param>
            /// <returns>A ValueTask that represents the same operation as <paramref name="task" />.</returns>
            let inline ofTask (task: Task<'T>) = ValueTask<'T> task

            /// <summary>Wraps a non-generic <see cref="T:System.Threading.Tasks.Task" /> as a non-generic <see cref="T:System.Threading.Tasks.ValueTask" />.</summary>
            /// <param name="task">The task to wrap.</param>
            /// <returns>A ValueTask that represents the same operation as <paramref name="task" />.</returns>
            let inline ofTaskUnit (task: Task) = ValueTask task

            /// <summary>Retrieves a <see cref="T:System.Threading.Tasks.Task`1" /> that represents the supplied <see cref="T:System.Threading.Tasks.ValueTask`1" />.</summary>
            /// <param name="vtask">The ValueTask to convert.</param>
            /// <typeparam name="'T">The result type of the ValueTask.</typeparam>
            /// <returns>
            /// The wrapped Task if one exists, or a new Task that represents the ValueTask result.
            /// </returns>
            let inline toTask (vtask: ValueTask<'T>) = vtask.AsTask()

            /// <summary>Retrieves a non-generic <see cref="T:System.Threading.Tasks.Task" /> that represents the supplied non-generic <see cref="T:System.Threading.Tasks.ValueTask" />.</summary>
            /// <param name="vtask">The ValueTask to convert.</param>
            /// <returns>The Task representation of <paramref name="vtask" />.</returns>
            let inline toTaskUnit (vtask: ValueTask) = vtask.AsTask()

            /// <summary>Converts a <see cref="T:System.Threading.Tasks.ValueTask`1" /> to its non-generic counterpart.</summary>
            /// <param name="vtask">The ValueTask whose result should be discarded.</param>
            /// <typeparam name="'T">The result type to discard.</typeparam>
            /// <returns>A non-generic ValueTask that completes when <paramref name="vtask" /> completes.</returns>
            let inline toUnit (vtask: ValueTask<'T>) : ValueTask =
                // this implementation follows Stephen Toub's advice, see:
                // https://github.com/dotnet/runtime/issues/31503#issuecomment-554415966
                if vtask.IsCompletedSuccessfully then
                    // ensure any side effect executes
                    vtask.Result
                    |> ignore

                    ValueTask()
                else
                    ValueTask(vtask.AsTask())
