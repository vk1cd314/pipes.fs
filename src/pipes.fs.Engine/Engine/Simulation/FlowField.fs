namespace pipes.fs.Engine.Simulation

open System
open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module FlowField =
    let private fractionalPart (inputValue: float32) =
        inputValue - floor inputValue

    let private hashedNoiseSignal
            (columnIndex: int)
            (rowIndex: int)
            (timeSeconds: float32)
        : float32 =
        let pseudoRandomSignal = sin (float (float32 columnIndex * 12.9898f + float32 rowIndex * 78.233f + timeSeconds * 11.123f)) * 43758.5453

        float32 (pseudoRandomSignal - floor pseudoRandomSignal)

    let field
            (flowKind: FlowKind)
            (timeSeconds: float32)
            (columnIndex: int)
            (rowIndex: int)
            (gridWidth: int)
            (gridHeight: int)
        : float32 * float32 =
        match flowKind with
        | FlowKind.NoFlow ->
            0.0f, 0.0f
        | FlowKind.Wind ->
            let horizontalVelocity = 0.45f + (0.12f * sin (timeSeconds * 0.9f + float32 rowIndex * 0.07f))
            let verticalVelocity = 0.08f * cos (timeSeconds * 0.6f + float32 columnIndex * 0.05f)
            horizontalVelocity, verticalVelocity
        | FlowKind.Swirl ->
            let centerColumn = float32 (gridWidth - 1) * 0.5f
            let centerRow = float32 (gridHeight - 1) * 0.5f
            let deltaXFromCenter = float32 columnIndex - centerColumn
            let deltaYFromCenter = float32 rowIndex - centerRow
            let radialMagnitude = max 1.0f (sqrt (deltaXFromCenter * deltaXFromCenter + deltaYFromCenter * deltaYFromCenter))
            let spinStrength = 0.7f + 0.15f * sin (timeSeconds * 0.5f)

            (-deltaYFromCenter / radialMagnitude) * spinStrength,
            (deltaXFromCenter / radialMagnitude) * spinStrength
        | FlowKind.Noise ->
            let primaryNoiseSample = hashedNoiseSignal columnIndex rowIndex timeSeconds
            let secondaryNoiseSample = hashedNoiseSignal (columnIndex + 19) (rowIndex - 7) (timeSeconds + 3.1f)
            let horizontalVelocity = (fractionalPart (primaryNoiseSample * 1.7f) - 0.5f) * 1.2f
            let verticalVelocity = (fractionalPart (secondaryNoiseSample * 1.9f) - 0.5f) * 1.2f
            horizontalVelocity, verticalVelocity
