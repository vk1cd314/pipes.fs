namespace pipes.fs.Engine.Simulation

open pipes.fs.Domain.Primitives
open pipes.fs.Domain.Rules
open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module SimulationRuntime =
    let private directionFromRandomIndex (randomDirectionIndex: int) =
        match randomDirectionIndex with
        | 0 -> Direction.North
        | 1 -> Direction.East
        | 2 -> Direction.South
        | _ -> Direction.West

    let private generateRandomWalkerState
        (dimensions:               Dimensions)
        (paletteColorCount:        int)
        (deterministicRandomState: DeterministicRandomState)
        =
        let spawnColumnIndex, deterministicRandomStateAfterColumn =
            DeterministicRandom.nextInt dimensions.Width deterministicRandomState

        let spawnRowIndex, deterministicRandomStateAfterRow =
            DeterministicRandom.nextInt dimensions.Height deterministicRandomStateAfterColumn

        let randomDirectionIndex, deterministicRandomStateAfterDirection =
            DeterministicRandom.nextInt 4 deterministicRandomStateAfterRow

        let spawnPaletteColorId, deterministicRandomStateAfterPaletteColor =
            DeterministicRandom.nextInt (max 1 paletteColorCount) deterministicRandomStateAfterDirection

        let spawnThicknessLevel, deterministicRandomStateAfterThickness =
            DeterministicRandom.nextInt 256 deterministicRandomStateAfterPaletteColor

        { Position =
            { ColumnIndex = spawnColumnIndex
              RowIndex = spawnRowIndex }
          Direction = directionFromRandomIndex randomDirectionIndex
          PaletteColorId = byte spawnPaletteColorId
          ThicknessLevel = byte spawnThicknessLevel },
        deterministicRandomStateAfterThickness

    let private initializeWalkerStates (simulationConfig: SimulationConfig) (dimensions: Dimensions) =
        let requestedWalkerCount = max 1 simulationConfig.WalkerCount

        let discoveredPaletteColorCount =
            PaletteRules.paletteColors simulationConfig.PaletteKind
            |> Array.length
            |> max 1

        Array.init requestedWalkerCount id
        |> Array.mapFold (fun deterministicRandomState _ ->
            generateRandomWalkerState
                dimensions
                discoveredPaletteColorCount
                deterministicRandomState
        ) simulationConfig.RandomSeed

    let initModel (simulationConfig: SimulationConfig) (dimensions: Dimensions) =
        let initializedWalkerStates, deterministicRandomStateAfterInitialization =
            initializeWalkerStates simulationConfig dimensions

        { GeometryMaskGrid = Grid.create dimensions.Width dimensions.Height 0uy
          InkIntensityGrid = Grid.create dimensions.Width dimensions.Height 0.0f
          PigmentColorGrid = Grid.create dimensions.Width dimensions.Height 0uy
          WalkerStates = initializedWalkerStates
          DeterministicRandomState = deterministicRandomStateAfterInitialization
          ElapsedSimulationTimeSeconds = 0.0f
          Dimensions = dimensions }

    let resizeModel
            (simulationConfig: SimulationConfig)
            (dimensions:       Dimensions)
            (simulationModel:  SimulationModel)
        : SimulationModel =
        let resizedSimulationConfig =
            { simulationConfig with
                RandomSeed = simulationModel.DeterministicRandomState }

        let rebuiltSimulationModel = initModel resizedSimulationConfig dimensions

        { rebuiltSimulationModel with
            ElapsedSimulationTimeSeconds = simulationModel.ElapsedSimulationTimeSeconds }

    let step
            (simulationConfig: SimulationConfig)
            (deltaTimeSeconds: float32)
            (simulationModel:  SimulationModel)
        : SimulationModel =
        let simulationModelAfterWalkerStep = PipeSimulation.stepWalkers simulationConfig simulationModel

        let updatedInkIntensityGrid =
            match simulationConfig.SimulationMode with
            | SimulationMode.Classic ->
                InkSimulation.decay simulationConfig.DecayFactor simulationModelAfterWalkerStep.InkIntensityGrid
            | SimulationMode.Ink ->
                simulationModelAfterWalkerStep.InkIntensityGrid
                |> InkSimulation.diffuse simulationConfig.DiffusionAmount
                |> InkSimulation.advect
                    simulationConfig.FlowKind
                    simulationModelAfterWalkerStep.ElapsedSimulationTimeSeconds
                    simulationModelAfterWalkerStep.Dimensions
                |> InkSimulation.decay simulationConfig.DecayFactor

        { simulationModelAfterWalkerStep with
            InkIntensityGrid = updatedInkIntensityGrid
            ElapsedSimulationTimeSeconds =
                simulationModelAfterWalkerStep.ElapsedSimulationTimeSeconds + max 0.0f deltaTimeSeconds }
