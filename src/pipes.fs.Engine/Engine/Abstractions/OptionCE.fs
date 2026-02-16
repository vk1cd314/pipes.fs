namespace pipes.fs.Engine.Abstractions

type OptionBuilder() =
    member _.Return(returnValue: 'value) =
        Some returnValue

    member _.ReturnFrom(existingOption: 'value option) =
        existingOption

    member _.Bind(existingOption: 'value option, binderFunction: 'value -> 'nextValue option) =
        match existingOption with
        | Some presentValue -> binderFunction presentValue
        | None              -> None

    member _.Zero() =
        None

    member _.Delay(delayedComputationFactory: unit -> 'value option) =
        delayedComputationFactory

    member _.Run(delayedComputationFactory: unit -> 'value option) =
        delayedComputationFactory ()

    member optionBuilder.Combine(firstOption: 'value option, secondOptionFactory: unit -> 'value option) =
        match firstOption with
        | Some presentValue -> Some presentValue
        | None              -> secondOptionFactory ()

[<AutoOpen>]
module OptionComputationExpression =
    let option = OptionBuilder()
