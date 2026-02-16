namespace pipes.fs.Tests

open System
open pipes.fs.Domain.Primitives
open pipes.fs.Domain.Types
open pipes.fs.Engine.Render

module PropertyTests =
    type VirtualTerminalSimulationState = {
        CursorColumnIndex:     int
        CursorRowIndex:        int
        ActiveCellStyle:       CellStyle
        FrameCellUpdatesByIndex: Map<int, FrameCell>
    }

    let private randomSampleCount = 250

    let private availableCellStyles =
        [| { ForegroundColor = None
             BackgroundColor = None
             IsBold          = false }
           { ForegroundColor = Some { RedChannel = 255uy; GreenChannel = 220uy; BlueChannel = 100uy }
             BackgroundColor = None
             IsBold          = false }
           { ForegroundColor = Some { RedChannel = 100uy; GreenChannel = 255uy; BlueChannel = 240uy }
             BackgroundColor = None
             IsBold          = true } |]

    let private createFrameFromIntegers
            frameWidth
            frameHeight
            (sourceValues: int array)
        =
        let packedFrameCells =
            Array.init (frameWidth * frameHeight) (fun currentCellIndex ->
                let selectedValue =
                    if currentCellIndex < sourceValues.Length then
                        sourceValues[currentCellIndex]
                    else
                        currentCellIndex

                let selectedStyle = availableCellStyles[abs selectedValue % availableCellStyles.Length]
                let selectedCharacter = char (32 + (abs selectedValue % 60))
                FramePacking.packFrameCell selectedCharacter selectedStyle)

        { Width       = frameWidth
          Height      = frameHeight
          PackedCells = packedFrameCells }

    let private isPositionInsideFrame
            frameWidth
            frameHeight
            columnIndex
            rowIndex
        =
        columnIndex >= 0
        && rowIndex >= 0
        && columnIndex < frameWidth
        && rowIndex < frameHeight

    let private stepVirtualTerminalState
        frameWidth
        frameHeight
        (virtualTerminalSimulationState: VirtualTerminalSimulationState)
        (drawOperation: DrawOperation)
        =
        match drawOperation with
        | MoveTo (targetColumnIndex, targetRowIndex) ->
            { virtualTerminalSimulationState with
                CursorColumnIndex = targetColumnIndex
                CursorRowIndex    = targetRowIndex }
        | SetStyle replacementCellStyle ->
            { virtualTerminalSimulationState with
                ActiveCellStyle = replacementCellStyle }
        | PutText textChunk ->
            let replacementPackedCellUpdatesByIndex =
                textChunk
                |> Seq.mapi (fun characterOffset currentCharacter ->
                    virtualTerminalSimulationState.CursorColumnIndex + characterOffset,
                    virtualTerminalSimulationState.CursorRowIndex,
                    currentCharacter)
                |> Seq.fold
                    (fun frameCellUpdatesByIndex (targetColumnIndex, targetRowIndex, currentCharacter) ->
                        if isPositionInsideFrame frameWidth frameHeight targetColumnIndex targetRowIndex then
                            let targetCellIndex = Grid.linearIndex frameWidth targetColumnIndex targetRowIndex

                            Map.add
                                targetCellIndex
                                { Character = currentCharacter
                                  Style = virtualTerminalSimulationState.ActiveCellStyle }
                                frameCellUpdatesByIndex
                        else
                            frameCellUpdatesByIndex)
                    virtualTerminalSimulationState.FrameCellUpdatesByIndex

            { virtualTerminalSimulationState with
                CursorColumnIndex     = virtualTerminalSimulationState.CursorColumnIndex + textChunk.Length
                FrameCellUpdatesByIndex = replacementPackedCellUpdatesByIndex }

    let private applyDrawOperations (baseFrame: VirtualFrame) (drawOperations: DrawOperation list) =
        let baseFrameCells = baseFrame.PackedCells |> Array.map FramePacking.unpackFrameCell

        let finalVirtualTerminalSimulationState =
            drawOperations
            |> List.fold
                (stepVirtualTerminalState baseFrame.Width baseFrame.Height)
                { CursorColumnIndex       = 0
                  CursorRowIndex          = 0
                  ActiveCellStyle         = FramePacking.emptyCellStyle
                  FrameCellUpdatesByIndex = Map.empty }

        baseFrameCells
        |> Array.mapi (fun currentCellIndex existingFrameCell ->
            Map.tryFind currentCellIndex finalVirtualTerminalSimulationState.FrameCellUpdatesByIndex
            |> Option.defaultValue existingFrameCell)

    let private generateRandomIntArray (randomGenerator: Random) =
        let generatedLength = randomGenerator.Next(0, 96)
        Array.init generatedLength (fun _ -> randomGenerator.Next(-5000, 5001))

    let private runPropertyOverRandomArrayPairs propertyName propertyPredicate =
        let randomGenerator = Random(20260216 + abs (hash propertyName))

        [ 1 .. randomSampleCount ]
        |> List.iter (fun sampleIndex ->
            let firstValues = generateRandomIntArray randomGenerator
            let secondValues = generateRandomIntArray randomGenerator

            if not (propertyPredicate firstValues secondValues) then
                TestFramework.fail
                    $"{propertyName} failed at sample #{sampleIndex}. first=%A{firstValues}; second=%A{secondValues}")

    let tests =
        [ TestFramework.testCase "Property: Diff + optimize reproduces next frame" (fun () ->
              runPropertyOverRandomArrayPairs
                  "Diff + optimize reproduces next frame"
                  (fun firstValues secondValues ->
                      let frameWidth = 8
                      let frameHeight = 4
                      let previousFrame = createFrameFromIntegers frameWidth frameHeight firstValues
                      let nextFrame = createFrameFromIntegers frameWidth frameHeight secondValues
                      let drawOperations = FrameDiff.diff previousFrame nextFrame |> DrawOperationOptimizer.optimize
                      let appliedFrameCells = applyDrawOperations previousFrame drawOperations
                      let expectedFrameCells = nextFrame.PackedCells |> Array.map FramePacking.unpackFrameCell
                      appliedFrameCells = expectedFrameCells))

          TestFramework.testCase "Property: Optimize is idempotent for diff-generated operations" (fun () ->
              runPropertyOverRandomArrayPairs
                  "Optimize is idempotent for diff-generated operations"
                  (fun firstValues secondValues ->
                      let frameWidth = 8
                      let frameHeight = 4
                      let previousFrame = createFrameFromIntegers frameWidth frameHeight firstValues
                      let nextFrame = createFrameFromIntegers frameWidth frameHeight secondValues
                      let generatedDiffOperations = FrameDiff.diff previousFrame nextFrame
                      let optimizedOnce = DrawOperationOptimizer.optimize generatedDiffOperations
                      let optimizedTwice = DrawOperationOptimizer.optimize optimizedOnce
                      optimizedTwice = optimizedOnce))

          TestFramework.testCase "Property: Optimize preserves draw-operation semantics" (fun () ->
              runPropertyOverRandomArrayPairs
                  "Optimize preserves draw-operation semantics"
                  (fun firstValues secondValues ->
                      let frameWidth = 8
                      let frameHeight = 4
                      let previousFrame = createFrameFromIntegers frameWidth frameHeight firstValues
                      let nextFrame = createFrameFromIntegers frameWidth frameHeight secondValues
                      let unoptimizedDrawOperations = FrameDiff.diff previousFrame nextFrame
                      let optimizedDrawOperations = DrawOperationOptimizer.optimize unoptimizedDrawOperations

                      let frameCellsFromUnoptimizedDrawOperations =
                          applyDrawOperations previousFrame unoptimizedDrawOperations

                      let frameCellsFromOptimizedDrawOperations =
                          applyDrawOperations previousFrame optimizedDrawOperations

                      frameCellsFromUnoptimizedDrawOperations = frameCellsFromOptimizedDrawOperations)) ]
