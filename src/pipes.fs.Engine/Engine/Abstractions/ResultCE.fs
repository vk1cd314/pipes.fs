namespace pipes.fs.Engine.Abstractions

type ResultBuilder() =
    member _.Return(returnValue: 'value) : Result<'value, 'error> =
        Ok returnValue

    member _.ReturnFrom(existingResult: Result<'value, 'error>) =
        existingResult

    member _.Bind(existingResult: Result<'value, 'error>, binderFunction: 'value -> Result<'nextValue, 'error>) =
        match existingResult with
        | Ok successfulValue -> binderFunction successfulValue
        | Error failureValue -> Error failureValue

    member _.Zero() : Result<unit, 'error> =
        Ok ()

    member _.Delay(delayedComputationFactory: unit -> Result<'value, 'error>) =
        delayedComputationFactory

    member _.Run(delayedComputationFactory: unit -> Result<'value, 'error>) =
        delayedComputationFactory ()

[<AutoOpen>]
module ResultComputationExpression =
    let result = ResultBuilder()
