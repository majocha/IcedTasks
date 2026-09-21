namespace IcedTasks

namespace IcedTasks.ValueTasksUnit

open IcedTasks

/// Contains methods to build non-generic ValueTasks using the F# computation expression syntax.
[<AutoOpen>]
module ValueTasksUnit =
    open System
    open System.Runtime.CompilerServices
    open System.Threading
    open System.Threading.Tasks
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.CompilerServices


    ///<summary>
    /// Contains methods to build non-generic ValueTasks using the F# computation expression syntax.
    /// </summary>
    type ValueTaskUnitBuilder() =

        inherit RuntimeAsyncBuilder()

        member inline this.Run([<InlineIfLambda>] code) =
            __runtimeAsyncReturnValueTaskUnit(runImplNoCancellation code)

    /// Contains the valueTaskUnit computation expression builder.
    [<AutoOpen>]
    module ValueTaskBuilder =

        /// <summary>
        /// Builds a non-generic ValueTask using computation expression syntax.
        /// </summary>
        let valueTaskUnit = ValueTaskUnitBuilder()

        /// <summary>
        /// Alias for <see cref="F:IcedTasks.ValueTasksUnit.ValueTasksUnit.ValueTaskBuilder.valueTaskUnit" />.
        /// </summary>
        let vTaskUnit = valueTaskUnit
