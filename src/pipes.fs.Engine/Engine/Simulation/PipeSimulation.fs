namespace pipes.fs.Engine.Simulation

open pipes.fs.Domain.Primitives
open pipes.fs.Domain.Rules
open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module PipeSimulation =
    type GeometryMaskUpdate = int * byte
    type InkIntensityUpdate = int * float32
    type PigmentColorUpdate = int * byte

    type WalkerTransitionResult = {
        UpdatedWalkerState:  WalkerState
        GeometryMaskUpdates: array<GeometryMaskUpdate>
        InkIntensityUpdate:  InkIntensityUpdate
        PigmentColorUpdate:  PigmentColorUpdate
    }

    let private directionFromRandomIndex (randomDirectionIndex: int) =
        match randomDirectionIndex with
        | 0 -> Direction.North
        | 1 -> Direction.East
        | 2 -> Direction.South
        | _ -> Direction.West

    let private chooseUpdatedDirection
        (turnProbability:  float32)
        (currentDirection: Direction)
        (randomTurnSample: float32)
        =
        let adjustedTurnWeight = 0.15f + (0.20f * GeometryRules.clampToUnitInterval turnProbability)
        let straightMoveWeight = max 0.10f (1.0f - (2.0f * adjustedTurnWeight))

        if randomTurnSample < straightMoveWeight then
            currentDirection
        elif randomTurnSample < straightMoveWeight + adjustedTurnWeight then
            GeometryRules.rotateLeft currentDirection
        else
            GeometryRules.rotateRight currentDirection

    let private aggregateUpdatesByCellIndex
            (combineUpdateValues:    'value -> 'value -> 'value)
            (initialAggregatedValue: 'value)
            (updates:                array<int * 'value>)
        : Map<int, 'value> =
        updates
        |> Array.fold
            (fun aggregatedUpdatesByCellIndex (targetCellIndex, updateValue) ->
                let existingAggregatedValue =
                    Map.tryFind targetCellIndex aggregatedUpdatesByCellIndex
                    |> Option.defaultValue initialAggregatedValue

                let replacementAggregatedValue = combineUpdateValues existingAggregatedValue updateValue
                Map.add targetCellIndex replacementAggregatedValue aggregatedUpdatesByCellIndex)
            Map.empty

    let applyGeometryMaskUpdates (geometryMaskUpdates: GeometryMaskUpdate array) (geometryMaskGrid: Grid<byte>) =
        let aggregatedDirectionMaskByCellIndex =
            aggregateUpdatesByCellIndex (fun currentMask replacementMask -> currentMask ||| replacementMask) 0uy geometryMaskUpdates

        let updatedGeometryMaskData =
            geometryMaskGrid.data
            |> Array.mapi (fun targetCellIndex existingDirectionMask ->
                match Map.tryFind targetCellIndex aggregatedDirectionMaskByCellIndex with
                | Some additionalDirectionMask -> existingDirectionMask ||| additionalDirectionMask
                | None -> existingDirectionMask)

        { geometryMaskGrid with data = updatedGeometryMaskData }

    let applyInkIntensityUpdates (inkIntensityUpdates: InkIntensityUpdate array) (inkIntensityGrid: Grid<float32>) =
        let aggregatedInkIntensityDeltaByCellIndex =
            aggregateUpdatesByCellIndex (fun currentDelta replacementDelta -> currentDelta + replacementDelta) 0.0f inkIntensityUpdates

        let updatedInkIntensityData =
            inkIntensityGrid.data
            |> Array.mapi (fun targetCellIndex existingInkIntensity ->
                match Map.tryFind targetCellIndex aggregatedInkIntensityDeltaByCellIndex with
                | Some additiveInkIntensity ->
                    GeometryRules.clampToUnitInterval (existingInkIntensity + additiveInkIntensity)
                | None ->
                    existingInkIntensity)

        { inkIntensityGrid with data = updatedInkIntensityData }

    let applyPigmentColorUpdates (pigmentColorUpdates: PigmentColorUpdate array) (pigmentColorGrid: Grid<byte>) =
        let replacementPigmentColorByCellIndex =
            aggregateUpdatesByCellIndex (fun _ replacementColorId -> replacementColorId) 0uy pigmentColorUpdates

        let updatedPigmentColorData =
            pigmentColorGrid.data
            |> Array.mapi (fun targetCellIndex existingPigmentColor ->
                match Map.tryFind targetCellIndex replacementPigmentColorByCellIndex with
                | Some replacementPigmentColor -> replacementPigmentColor
                | None -> existingPigmentColor)

        { pigmentColorGrid with data = updatedPigmentColorData }

    let private transitionSingleWalker
        (simulationConfig:         SimulationConfig)
        (simulationModel:          SimulationModel)
        (currentWalkerState:       WalkerState)
        (deterministicRandomState: DeterministicRandomState)
        =
        let randomTurnSample, deterministicRandomStateAfterTurnSample = DeterministicRandom.nextFloat01 deterministicRandomState

        let updatedDirection = chooseUpdatedDirection simulationConfig.TurnProbability currentWalkerState.Direction randomTurnSample

        let updatedHeadPosition = GeometryRules.moveWithWrapAround simulationModel.Dimensions currentWalkerState.Position updatedDirection

        let previousCellIndex =
            Grid.linearIndex
                simulationModel.Dimensions.Width
                currentWalkerState.Position.ColumnIndex
                currentWalkerState.Position.RowIndex

        let updatedCellIndex = Grid.linearIndex simulationModel.Dimensions.Width updatedHeadPosition.ColumnIndex updatedHeadPosition.RowIndex

        let injectedInkIntensity = 0.18f + ((float32 currentWalkerState.ThicknessLevel / 255.0f) * 0.55f)

        { UpdatedWalkerState =
            { currentWalkerState with
                Position  = updatedHeadPosition
                Direction = updatedDirection }
          GeometryMaskUpdates =
            [| previousCellIndex, GeometryRules.directionBitMask updatedDirection
               updatedCellIndex,  GeometryRules.directionBitMask (GeometryRules.oppositeDirection updatedDirection) |]
          InkIntensityUpdate = updatedCellIndex, injectedInkIntensity
          PigmentColorUpdate = updatedCellIndex, currentWalkerState.PaletteColorId },
        deterministicRandomStateAfterTurnSample

    let private maybeRespawnSingleWalker
        (simulationConfig:         SimulationConfig)
        (dimensions:               Dimensions)
        (paletteColorCount:        int)
        (currentWalkerStates:      array<WalkerState>)
        (deterministicRandomState: DeterministicRandomState)
        =
        if Array.isEmpty currentWalkerStates || simulationConfig.RespawnProbability <= 0.0f then
            currentWalkerStates, deterministicRandomState
        else
            let shouldRespawnWalker, deterministicRandomStateAfterRespawnCheck =
                DeterministicRandom.nextBoolWithProbability
                    simulationConfig.RespawnProbability
                    deterministicRandomState

            if not shouldRespawnWalker then
                currentWalkerStates, deterministicRandomStateAfterRespawnCheck
            else
                let walkerIndexToRespawn, deterministicRandomStateAfterWalkerIndex =
                    DeterministicRandom.nextInt
                        currentWalkerStates.Length
                        deterministicRandomStateAfterRespawnCheck

                let spawnColumnIndex, deterministicRandomStateAfterColumn =
                    DeterministicRandom.nextInt dimensions.Width deterministicRandomStateAfterWalkerIndex

                let spawnRowIndex, deterministicRandomStateAfterRow =
                    DeterministicRandom.nextInt dimensions.Height deterministicRandomStateAfterColumn

                let randomDirectionIndex, deterministicRandomStateAfterDirection =
                    DeterministicRandom.nextInt 4 deterministicRandomStateAfterRow

                let spawnPaletteColorId, deterministicRandomStateAfterPaletteColor =
                    DeterministicRandom.nextInt (max 1 paletteColorCount) deterministicRandomStateAfterDirection

                let spawnThicknessLevel, deterministicRandomStateAfterThickness =
                    DeterministicRandom.nextInt 256 deterministicRandomStateAfterPaletteColor

                let replacementWalkerState =
                    { Position =
                        { ColumnIndex = spawnColumnIndex
                          RowIndex = spawnRowIndex }
                      Direction = directionFromRandomIndex randomDirectionIndex
                      PaletteColorId = byte spawnPaletteColorId
                      ThicknessLevel = byte spawnThicknessLevel }

                let walkerStatesAfterRespawn =
                    currentWalkerStates
                    |> Array.mapi (fun currentWalkerIndex currentWalkerState ->
                        if currentWalkerIndex = walkerIndexToRespawn then
                            replacementWalkerState
                        else
                            currentWalkerState
                    )

                walkerStatesAfterRespawn, deterministicRandomStateAfterThickness

    let stepWalkers (simulationConfig: SimulationConfig) (simulationModel: SimulationModel) =
        let walkerTransitionResults, deterministicRandomStateAfterWalkerTransitions =
            simulationModel.WalkerStates
            |> Array.mapFold
                (fun deterministicRandomState currentWalkerState ->
                    transitionSingleWalker
                        simulationConfig
                        simulationModel
                        currentWalkerState
                        deterministicRandomState)
                simulationModel.DeterministicRandomState

        let updatedWalkerStatesBeforeRespawn =
            walkerTransitionResults
            |> Array.map (fun currentWalkerTransitionResult -> currentWalkerTransitionResult.UpdatedWalkerState)

        let generatedGeometryMaskUpdates =
            walkerTransitionResults
            |> Array.collect (fun currentWalkerTransitionResult -> currentWalkerTransitionResult.GeometryMaskUpdates)

        let generatedInkIntensityUpdates =
            walkerTransitionResults
            |> Array.map (fun currentWalkerTransitionResult -> currentWalkerTransitionResult.InkIntensityUpdate)

        let generatedPigmentColorUpdates =
            walkerTransitionResults
            |> Array.map (fun currentWalkerTransitionResult -> currentWalkerTransitionResult.PigmentColorUpdate)

        let availablePaletteColorCount =
            PaletteRules.paletteColors simulationConfig.PaletteKind
            |> Array.length

        let updatedWalkerStatesAfterRespawn, deterministicRandomStateAfterRespawn =
            maybeRespawnSingleWalker
                simulationConfig
                simulationModel.Dimensions
                availablePaletteColorCount
                updatedWalkerStatesBeforeRespawn
                deterministicRandomStateAfterWalkerTransitions

        let updatedGeometryMaskGrid =
            applyGeometryMaskUpdates generatedGeometryMaskUpdates simulationModel.GeometryMaskGrid

        let updatedInkIntensityGrid =
            applyInkIntensityUpdates generatedInkIntensityUpdates simulationModel.InkIntensityGrid

        let updatedPigmentColorGrid =
            applyPigmentColorUpdates generatedPigmentColorUpdates simulationModel.PigmentColorGrid

        { simulationModel with
            WalkerStates = updatedWalkerStatesAfterRespawn
            GeometryMaskGrid = updatedGeometryMaskGrid
            InkIntensityGrid = updatedInkIntensityGrid
            PigmentColorGrid = updatedPigmentColorGrid
            DeterministicRandomState = deterministicRandomStateAfterRespawn }
