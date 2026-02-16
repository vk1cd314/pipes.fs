namespace pipes.fs.Engine.Presentation

open pipes.fs.Domain.Primitives
open pipes.fs.Domain.Types
open pipes.fs.Engine.Render

[<RequireQualifiedAccess>]
module HudOverlay =
    let applyHudOverlay
            (simulationConfig: SimulationConfig)
            (simulationModel: SimulationModel)
            (virtualFrame: VirtualFrame)
        : VirtualFrame =
        if not simulationConfig.ShowHudOverlay then
            virtualFrame
        else
            let hudCellStyle =
                { ForegroundColor =
                    Some
                        { RedChannel = 250uy
                          GreenChannel = 250uy
                          BlueChannel = 190uy }
                  BackgroundColor =
                    Some
                        { RedChannel = 20uy
                          GreenChannel = 20uy
                          BlueChannel = 20uy }
                  IsBold = false }

            let elapsedTimeText = sprintf "%.1f" simulationModel.ElapsedSimulationTimeSeconds

            let hudLineText =
                $" pipes.fs | mode={simulationConfig.SimulationMode} flow={simulationConfig.FlowKind} walkers={simulationModel.WalkerStates.Length} fps={simulationConfig.FramesPerSecond} t={elapsedTimeText} "

            let fittedHudText =
                if hudLineText.Length >= virtualFrame.Width then
                    hudLineText.Substring(0, virtualFrame.Width)
                else
                    hudLineText.PadRight(virtualFrame.Width)

            let copiedPackedCells = Array.copy virtualFrame.PackedCells

            for columnIndex = 0 to fittedHudText.Length - 1 do
                let targetCellIndex = Grid.linearIndex virtualFrame.Width columnIndex 0
                copiedPackedCells[targetCellIndex] <- FramePacking.packFrameCell fittedHudText[columnIndex] hudCellStyle

            { virtualFrame with PackedCells = copiedPackedCells }
