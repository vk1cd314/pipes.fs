namespace pipes.fs.App

open System
open pipes.fs.Domain.Types
open pipes.fs.Engine.Presentation
open pipes.fs.Engine.Render
open pipes.fs.Engine.Simulation

[<RequireQualifiedAccess>]
module RuntimeLoop =
    type RuntimeLoopState = {
        TerminalDimensions:     Dimensions
        SimulationModelState:   SimulationModel
        PreviousVirtualFrame:   VirtualFrame
        PreviousFrameTimestamp: TimeSpan
    }

    type RuntimeStepInput = {
        CurrentFrameTimestamp:    TimeSpan
        LatestTerminalDimensions: Dimensions
    }

    type RuntimeStepOutput = {
        NextRuntimeLoopState:    RuntimeLoopState
        OptimizedDrawOperations: List<DrawOperation>
        ShouldClearTerminal:     bool
    }

    let createInitialState
            (simulationConfig:          SimulationConfig)
            (initialTerminalDimensions: Dimensions)
            (initialFrameTimestamp:     TimeSpan)
        : RuntimeLoopState =
        { TerminalDimensions     = initialTerminalDimensions
          SimulationModelState   = SimulationRuntime.initModel simulationConfig initialTerminalDimensions
          PreviousVirtualFrame   = FramePacking.blankFrame initialTerminalDimensions.Width initialTerminalDimensions.Height
          PreviousFrameTimestamp = initialFrameTimestamp }

    let private renderVirtualFrame (simulationConfig: SimulationConfig) (simulationModelState: SimulationModel) =
        simulationModelState
        |> ModelRenderer.renderModel simulationConfig
        |> HudOverlay.applyHudOverlay simulationConfig simulationModelState

    let private resizeRuntimeLoopState
            (simulationConfig:              SimulationConfig)
            (replacementTerminalDimensions: Dimensions)
            (runtimeLoopState:              RuntimeLoopState)
        : RuntimeLoopState =
        let resizedSimulationModelState =
            SimulationRuntime.resizeModel
                simulationConfig
                replacementTerminalDimensions
                runtimeLoopState.SimulationModelState

        { runtimeLoopState with
            TerminalDimensions = replacementTerminalDimensions
            SimulationModelState = resizedSimulationModelState
            PreviousVirtualFrame =
                FramePacking.blankFrame replacementTerminalDimensions.Width replacementTerminalDimensions.Height }

    let advanceOneFrame
            (simulationConfig: SimulationConfig)
            (runtimeStepInput: RuntimeStepInput)
            (runtimeLoopState: RuntimeLoopState)
        : RuntimeStepOutput =
        let deltaTimeSeconds = (runtimeStepInput.CurrentFrameTimestamp - runtimeLoopState.PreviousFrameTimestamp).TotalSeconds |> float32

        let hasTerminalDimensionsChanged = runtimeStepInput.LatestTerminalDimensions <> runtimeLoopState.TerminalDimensions

        let resizedRuntimeLoopState =
            if hasTerminalDimensionsChanged then
                resizeRuntimeLoopState simulationConfig runtimeStepInput.LatestTerminalDimensions runtimeLoopState
            else
                runtimeLoopState

        let steppedSimulationModelState =
            SimulationRuntime.step
                simulationConfig
                deltaTimeSeconds
                resizedRuntimeLoopState.SimulationModelState

        let nextVirtualFrame = renderVirtualFrame simulationConfig steppedSimulationModelState

        let optimizedDrawOperations =
            FrameDiff.diff resizedRuntimeLoopState.PreviousVirtualFrame nextVirtualFrame
            |> DrawOperationOptimizer.optimize

        { NextRuntimeLoopState =
            { resizedRuntimeLoopState with
                SimulationModelState   = steppedSimulationModelState
                PreviousVirtualFrame   = nextVirtualFrame
                PreviousFrameTimestamp = runtimeStepInput.CurrentFrameTimestamp }
          OptimizedDrawOperations = optimizedDrawOperations
          ShouldClearTerminal     = hasTerminalDimensionsChanged }
