namespace IcedTasks

open System
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Collections

[<AutoOpen>]
module Tasks =

    type TaskBuilder() =
        inherit RuntimeAsyncBuilder()

        member inline this.Run([<InlineIfLambda>] code) : Task<'T> = __runtimeAsyncReturn (code ())

        member inline this.Source(task: Task<'T>) = base.Source(task)

    type BackgroundTaskBuilder() =
        inherit RuntimeAsyncBuilder()

        member inline this.Run([<InlineIfLambda>] code) : Task<'T> =
            if isAlreadyBackground () then
                __runtimeAsyncReturn (code ())
            else
                Task.Run<'T>(fun () -> __runtimeAsyncReturn (code ()))

        member inline this.Source(task: Task<'T>) = base.Source(task)


    type TaskUnitBuilder() =
        inherit RuntimeAsyncBuilder()
        member inline this.Run([<InlineIfLambda>] code) : Task = __runtimeAsyncReturnUnit (code ())


    type BackgroundTaskUnitBuilder() =
        inherit RuntimeAsyncBuilder()

        member inline this.Run([<InlineIfLambda>] code) : Task =
            if isAlreadyBackground () then
                __runtimeAsyncReturnUnit (code ())
            else
                Task.Run(fun () -> __runtimeAsyncReturnUnit (code ()))

        member inline this.Source(task: Task<'T>) = base.Source(task)


/// Contains the task computation expression builder.
[<AutoOpen>]
module TaskBuilder =

    /// <summary>
    /// Builds a task using computation expression syntax
    ///
    /// <b>NOTE:</b> This is the TaskBuilder defined in IcedTasks. This fixes any issues with the TaskBuilder defined in FSharp.Core that can't be backported.
    /// </summary>
    let task = TaskBuilder()

    /// <summary>
    /// Builds a task using computation expression syntax which switches to execute on a background thread if not already doing so.
    ///
    /// <b>NOTE:</b> This is the BackgroundTaskBuilder defined in IcedTasks. This fixes any issues with the BackgroundTaskBuilder defined in FSharp.Core that can't be backported.
    /// </summary>
    let backgroundTask = BackgroundTaskBuilder()

    /// <summary>
    /// Builds a taskUnit using computation expression syntax.
    /// </summary>
    let taskUnit = TaskUnitBuilder()

    /// <summary>
    /// Builds a taskUnit using computation expression syntax which switches to execute on a background thread if not already doing so.
    /// </summary>
    let backgroundTaskUnit = BackgroundTaskUnitBuilder()

/// Contains methods to build ColdTasks using the F# computation expression syntax
[<AutoOpen>]
module ColdTasks =

    /// Contains methods to build ColdTasks using the F# computation expression syntax
    type ColdTaskBuilder() =
        inherit RuntimeAsyncBuilder()

        member inline this.Run([<InlineIfLambda>] code) : ColdTask<'T> =
            fun () -> __runtimeAsyncReturn (code ())

    /// Contains methods to build ColdTasks using the F# computation expression syntax
    type BackgroundColdTaskBuilder() =

        inherit RuntimeAsyncBuilder()

        member inline this.Run([<InlineIfLambda>] code) : ColdTask<'T> =
            fun () ->
                if isAlreadyBackground () then
                    __runtimeAsyncReturn (code ())
                else
                    Task.Run<'T>(fun () -> __runtimeAsyncReturn (code ()))


    /// Contains the coldTasks computation expression builder.
    [<AutoOpen>]
    module ColdTaskBuilder =

        /// <summary>
        /// Builds a coldTask using computation expression syntax.
        /// </summary>
        let coldTask = ColdTaskBuilder()

        /// <summary>
        /// Builds a coldTask using computation expression syntax which switches to execute on a background thread if not already doing so.
        /// </summary>
        let backgroundColdTask = BackgroundColdTaskBuilder()

[<AutoOpen>]
module AsyncExtensions =
    type Microsoft.FSharp.Control.Async with

        /// <summary>Return an asynchronous computation that will wait for the given task to complete and return
        /// its result.</summary>
        static member inline AwaitColdTask(t: ColdTask<'T>) =
            async.Delay(fun () ->
                t ()
                |> Async.AwaitTask
            )

        /// <summary>Return an asynchronous computation that will wait for the given task to complete and return
        /// its result.</summary>
        static member inline AwaitColdTask(t: ColdTask) =
            async.Delay(fun () ->
                t ()
                |> Async.AwaitTask
            )

        /// <summary>Runs an asynchronous computation, starting on the current operating system thread.</summary>
        static member inline AsColdTask(computation: Async<'T>) : ColdTask<_> =
            fun () -> Async.StartImmediateAsTask(computation)

    /// <summary>
    /// A set of extension methods making it possible to bind against <see cref='T:IcedTasks.ColdTasks.ColdTask`1'/> in async computations.
    /// </summary>
    [<AutoOpen>]
    module AsyncExtensions2 =
        type AsyncExBuilder with

            member inline this.Source(task: ColdTask<'T>) : Async<'T> = AsyncEx.AwaitColdTask task
            member inline this.Source(task: ColdTask) : Async<unit> = AsyncEx.AwaitColdTask task

        type Microsoft.FSharp.Control.AsyncBuilder with

            member inline this.Bind(coldTask: ColdTask<'T>, binder: ('T -> Async<'U>)) : Async<'U> =
                this.Bind(Async.AwaitColdTask coldTask, binder)

            member inline this.ReturnFrom(coldTask: ColdTask<'T>) : Async<'T> =
                this.ReturnFrom(Async.AwaitColdTask coldTask)

            member inline this.Bind(coldTask: ColdTask, binder: (unit -> Async<'U>)) : Async<'U> =
                this.Bind(Async.AwaitColdTask coldTask, binder)

            member inline this.ReturnFrom(coldTask: ColdTask) : Async<unit> =
                this.ReturnFrom(Async.AwaitColdTask coldTask)

        type Microsoft.FSharp.Control.TaskBuilderBase with

            member inline this.Bind(coldTask: ColdTask<'T>, binder: ('T -> _)) =
                this.Bind(coldTask (), binder)

            member inline this.ReturnFrom(coldTask: ColdTask<'T>) = this.ReturnFrom(coldTask ())

            member inline this.Bind(coldTask: ColdTask, binder: (_ -> _)) =
                this.Bind(coldTask (), binder)

            member inline this.ReturnFrom(coldTask: ColdTask) = this.ReturnFrom(coldTask ())

    [<RequireQualifiedAccess>]
    module ColdTask =

        let inline singleton (result: 'item) : ColdTask<'item> = fun () -> Task.FromResult result

        let inline bind
            ([<InlineIfLambda>] binder: 'input -> ColdTask<'output>)
            ([<InlineIfLambda>] cTask: ColdTask<'input>)
            =
            coldTask {
                let! cResult = cTask
                return! binder cResult
            }

        let inline map
            ([<InlineIfLambda>] mapper: 'input -> 'output)
            ([<InlineIfLambda>] cTask: ColdTask<'input>)
            =
            coldTask {
                let! cResult = cTask
                return mapper cResult
            }

        let inline apply
            ([<InlineIfLambda>] applicable: ColdTask<'input -> 'output>)
            ([<InlineIfLambda>] cTask: ColdTask<'input>)
            =
            coldTask {
                let! applier = applicable
                let! cResult = cTask
                return applier cResult
            }

        let inline zip
            ([<InlineIfLambda>] left: ColdTask<'left>)
            ([<InlineIfLambda>] right: ColdTask<'right>)
            =
            coldTask {
                let! r1 = left
                let! r2 = right
                return r1, r2
            }

        let inline parallelZip
            ([<InlineIfLambda>] left: ColdTask<'left>)
            ([<InlineIfLambda>] right: ColdTask<'right>)
            =
            coldTask {
                let r1 = left ()
                let r2 = right ()
                let! r1 = r1
                let! r2 = r2
                return r1, r2
            }

        let inline ofUnit ([<InlineIfLambda>] unitColdTask: ColdTask) =
            coldTask { return! unitColdTask }

        let inline toUnit ([<InlineIfLambda>] coldTask: ColdTask<_>) : ColdTask =
            fun () -> coldTask () :> Task

        let inline internal getAwaiter ([<InlineIfLambda>] ctask: ColdTask<_>) =
            fun () -> (ctask ()).GetAwaiter()

namespace IcedTasks.Polyfill.Task

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

///// Contains the task computation expression builder.
//[<AutoOpen>]
//module TaskBuilder =

//    type TaskBuilder() =
//        inherit RuntimeAsyncBuilder()
//        member inline _.Run([<InlineIfLambda>] code) : Task<'T> =
//            __runtimeAsyncReturn(code())


//    type BackgroundTaskBuilder() =
//        inherit RuntimeAsyncBuilder()
//        member inline _.Run([<InlineIfLambda>] code) : Task<'T> =
//            if isAlreadyBackground () then
//                __runtimeAsyncReturn(code())
//            else
//                Task.Run<'T>(fun () -> __runtimeAsyncReturn(code()))

//        member inline _.Source(task: Task<'T>) = Started(fun () -> AsyncHelpers.Await task)


//    /// <summary>
//    /// Builds a task using computation expression syntax
//    ///
//    /// <b>NOTE:</b> This is the TaskBuilder defined in IcedTasks. This fixes any issues with the TaskBuilder defined in FSharp.Core that can't be backported.
//    /// </summary>
//    let task = TaskBuilder()

//    /// <summary>
//    /// Builds a task using computation expression syntax which switches to execute on a background thread if not already doing so.
//    ///
//    /// <b>NOTE:</b> This is the BackgroundTaskBuilder defined in IcedTasks. This fixes any issues with the BackgroundTaskBuilder defined in FSharp.Core that can't be backported.
//    /// </summary>
//    let backgroundTask = BackgroundTaskBuilder()
