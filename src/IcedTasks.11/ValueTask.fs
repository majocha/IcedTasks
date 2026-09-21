namespace IcedTasks


open System.Threading.Tasks

/// <summary>
/// Module with extension methods for <see cref="T:System.Threading.Tasks.ValueTask`1"/>.
/// </summary>
[<AutoOpen>]
module ValueTaskExtensions =

    type ValueTask with

        /// <summary>Creates a <see cref="T:System.Threading.Tasks.ValueTask" /> that's completed due to cancellation with a specified cancellation token.</summary>
        /// <param name="cancellationToken">The cancellation token with which to complete the task.</param>
        /// <returns>The canceled task.</returns>
        /// <exception cref="T:System.ArgumentOutOfRangeException">Cancellation has not been requested for <paramref name="cancellationToken" />; its <see cref="P:System.Threading.CancellationToken.IsCancellationRequested" /> property is <see langword="false" />.</exception>
        static member FromCanceled(cancellationToken) =
            new ValueTask(Task.FromCanceled(cancellationToken))

        /// <summary>Creates a <see cref="T:System.Threading.Tasks.ValueTask`1" /> that's completed due to cancellation with a specified cancellation token.</summary>
        /// <param name="cancellationToken">The cancellation token with which to complete the task.</param>
        /// <typeparam name="TResult">The type of the result returned by the task.</typeparam>
        /// <returns>The canceled task.</returns>
        /// <exception cref="T:System.ArgumentOutOfRangeException">Cancellation has not been requested for <paramref name="cancellationToken" />; its <see cref="P:System.Threading.CancellationToken.IsCancellationRequested" /> property is <see langword="false" />.</exception>
        static member FromCanceled<'T>(cancellationToken) =
            new ValueTask<'T>(Task.FromCanceled<'T>(cancellationToken))

    type Microsoft.FSharp.Control.Async with

        /// <summary>
        /// Return an asynchronous computation that will check if ValueTask is completed or wait for
        /// the given task to complete and return its result.
        /// </summary>
        /// <param name="vTask">The task to await.</param>
        static member inline AwaitValueTask(vTask: ValueTask<_>) : Async<_> =
            // https://github.com/dotnet/runtime/issues/31503#issuecomment-554415966
            if vTask.IsCompletedSuccessfully then
                async.Return vTask.Result
            else
                Async.AwaitTask(vTask.AsTask())


        /// <summary>
        /// Return an asynchronous computation that will check if ValueTask is completed or wait for
        /// the given task to complete and return its result.
        /// </summary>
        /// <param name="vTask">The task to await.</param>
        static member inline AwaitValueTask(vTask: ValueTask) : Async<unit> =
            // https://github.com/dotnet/runtime/issues/31503#issuecomment-554415966
            if vTask.IsCompletedSuccessfully then
                async.Return()
            else
                Async.AwaitTask(vTask.AsTask())


        /// <summary>
        /// Runs an asynchronous computation, starting immediately on the current operating system thread,
        /// but also returns the execution as <see cref="T:System.Threading.Tasks.ValueTask`1" />.
        /// </summary>
        static member inline AsValueTask(computation: Async<'T>) : ValueTask<'T> =
            Async.StartImmediateAsTask(computation)
            |> ValueTask<'T>


// Task builder for F# that compiles to allocation-free paths for synchronous code.
//
// Originally written in 2016 by Robert Peele (humbobst@gmail.com)
// New operator-based overload resolution for F# 4.0 compatibility by Gustavo Leon in 2018.
// Revised for insertion into FSharp.Core by Microsoft, 2019.
// Revised to implement ValueTask semantics
//
// Original notice:
// To the extent possible under law, the author(s) have dedicated all copyright and related and neighboring rights
// to this software to the public domain worldwide. This software is distributed without any warranty.

/// Contains methods to build ValueTasks using the F# computation expression syntax
[<AutoOpen>]
module ValueTasks =
    open System
    open System.Runtime.CompilerServices
    open System.Threading.Tasks
    open System.Threading
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

    ///<summary>
    /// Contains methods to build ValueTasks using the F# computation expression syntax
    /// </summary>
    type ValueTaskBuilder() =

        inherit RuntimeAsyncBuilder()

        member inline _.Run([<InlineIfLambda>] code) : ValueTask<'T> =
            __runtimeAsyncReturnValueTask(
                Cancellation.setToken CancellationToken.None
                code())

        member inline _.Source(task: ValueTask<'T>) = Started( fun () -> AsyncHelpers.Await task)

    /// Contains the valueTask computation expression builder.
    [<AutoOpen>]
    module ValueTaskBuilder =

        /// <summary>
        /// Builds a valueTask using computation expression syntax.
        /// </summary>
        let valueTask = ValueTaskBuilder()

        /// <summary>
        /// Alias for <see cref="F:IcedTasks.ValueTasks.ValueTasks.ValueTaskBuilder.valueTask" />.
        /// </summary>
        let vTask = valueTask


    /// Contains functional helper functions for composing and converting <see cref="T:System.Threading.Tasks.ValueTask`1" /> values.
    [<RequireQualifiedAccess>]
    module ValueTask =

        /// <summary>Lifts an item to a ValueTask.</summary>
        /// <param name="item">The item to be the result of the ValueTask.</param>
        /// <returns>A ValueTask with the item as the result.</returns>
        let inline singleton (item: 'item) : ValueTask<'item> = ValueTask<'item> item

        /// <summary>Allows chaining of ValueTasks.</summary>
        /// <param name="binder">The continuation.</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the binder.</returns>
        let inline bind
            ([<InlineIfLambda>] (binder: 'input -> ValueTask<'output>))
            (cTask: ValueTask<'input>)
            =
            valueTask {
                let! cResult = cTask
                return! binder cResult
            }

        /// <summary>Allows chaining of ValueTasks.</summary>
        /// <param name="mapper">The continuation.</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the mapper wrapped in a ValueTasks.</returns>
        let inline map ([<InlineIfLambda>] mapper: 'input -> 'output) (cTask: ValueTask<'input>) =
            valueTask {
                let! cResult = cTask
                return mapper cResult
            }

        /// <summary>Allows chaining of ValueTasks.</summary>
        /// <param name="applicable">A function wrapped in a ValueTasks</param>
        /// <param name="cTask">The value.</param>
        /// <returns>The result of the applicable.</returns>
        let inline apply (applicable: ValueTask<'input -> 'output>) (cTask: ValueTask<'input>) =
            valueTask {
                let! applier = applicable
                let! cResult = cTask
                return applier cResult
            }

        /// <summary>Takes two ValueTasks, starts them serially in order of left to right, and returns a tuple of the pair.</summary>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>A tuple of the parameters passed in</returns>
        let inline zip (left: ValueTask<'left>) (right: ValueTask<'right>) =
            valueTask {
                let! r1 = left
                let! r2 = right
                return r1, r2
            }

        /// <summary>Converts a non-generic <see cref="T:System.Threading.Tasks.ValueTask" /> to a <see cref="T:System.Threading.Tasks.ValueTask`1" /> of unit.</summary>
        /// <param name="vtask">The non-generic ValueTask to convert.</param>
        /// <returns>A ValueTask whose result is unit.</returns>
        let inline ofUnit (vtask: ValueTask) : ValueTask<unit> =
            // this implementation follows Stephen Toub's advice, see:
            // https://github.com/dotnet/runtime/issues/31503#issuecomment-554415966
            if vtask.IsCompletedSuccessfully then
                ValueTask<unit>()
            else
                valueTask { return! vtask }

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
