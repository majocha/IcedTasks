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

open RuntimeAsyncBuilder

[<AutoOpen>]
module Tasks =

    type TaskBuilder() =
        inherit RuntimeAsyncBuilder()

        member inline _.Run([<InlineIfLambda>] code) : Task<'T> =
            __runtimeAsyncReturn(code())

    type BackgroundTaskBuilder() =
        inherit RuntimeAsyncBuilder()

        member inline _.Run([<InlineIfLambda>] code) : Task<'T> =
            Task.Run(fun () -> __runtimeAsyncReturn(code()))

    type TaskUnitBuilder() =
        inherit RuntimeAsyncBuilder()
            member inline _.Run([<InlineIfLambda>] code) : Task =
                __runtimeAsyncReturnUnit(code())

    type BackgroundTaskUnitBuilder() =
        inherit RuntimeAsyncBuilder()
            member inline _.Run([<InlineIfLambda>] code) : Task =
                Task.Run(fun () -> __runtimeAsyncReturnUnit(code()))

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

        member inline _.Run([<InlineIfLambda>] code) : ColdTask<'T> =
            fun () -> __runtimeAsyncReturn(code())


    /// Contains methods to build ColdTasks using the F# computation expression syntax
    type BackgroundColdTaskBuilder() =

        inherit RuntimeAsyncBuilder()

        member inline _.Run([<InlineIfLambda>] code) : ColdTask<'T> =
            fun () -> Task.Run<'T>(fun () -> __runtimeAsyncReturn(code()))


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
