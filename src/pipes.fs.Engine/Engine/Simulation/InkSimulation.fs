namespace pipes.fs.Engine.Simulation

open pipes.fs.Domain.Primitives
open pipes.fs.Domain.Rules
open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module InkSimulation =
    let private clampToUnitInterval =
        GeometryRules.clampToUnitInterval

    let diffuse (diffusionAmount: float32) (inkIntensityGrid: Grid<float32>) =
        let clampedDiffusionAmount = clampToUnitInterval diffusionAmount

        if clampedDiffusionAmount <= 0.0f then
            inkIntensityGrid
        else
            let retainedCenterWeight = 1.0f - clampedDiffusionAmount

            inkIntensityGrid
            |> Grid.mapCellsWithCoordinates (fun columnIndex rowIndex centerCellIntensity ->
                let northNeighborCellIndex =
                    Grid.linearIndex
                        inkIntensityGrid.width
                        columnIndex
                        (Grid.wrapCoordinate inkIntensityGrid.height (rowIndex - 1))

                let eastNeighborCellIndex =
                    Grid.linearIndex
                        inkIntensityGrid.width
                        (Grid.wrapCoordinate inkIntensityGrid.width (columnIndex + 1))
                        rowIndex

                let southNeighborCellIndex =
                    Grid.linearIndex
                        inkIntensityGrid.width
                        columnIndex
                        (Grid.wrapCoordinate inkIntensityGrid.height (rowIndex + 1))

                let westNeighborCellIndex =
                    Grid.linearIndex
                        inkIntensityGrid.width
                        (Grid.wrapCoordinate inkIntensityGrid.width (columnIndex - 1))
                        rowIndex

                let neighborAverageIntensity =
                    (inkIntensityGrid.data[northNeighborCellIndex]
                     + inkIntensityGrid.data[eastNeighborCellIndex]
                     + inkIntensityGrid.data[southNeighborCellIndex]
                     + inkIntensityGrid.data[westNeighborCellIndex])
                    * 0.25f

                ((retainedCenterWeight * centerCellIntensity) + (clampedDiffusionAmount * neighborAverageIntensity))
                |> clampToUnitInterval
            )

    let advect
            (flowKind:                     FlowKind)
            (elapsedSimulationTimeSeconds: float32)
            (dimensions:                   Dimensions)
            (inkIntensityGrid:             Grid<float32>)
        : Grid<float32> =
        inkIntensityGrid
        |> Grid.mapCellsWithCoordinates (fun columnIndex rowIndex _ ->
            let flowVelocityX, flowVelocityY =
                FlowField.field
                    flowKind
                    elapsedSimulationTimeSeconds
                    columnIndex
                    rowIndex
                    dimensions.Width
                    dimensions.Height

            let sampledSourceColumnIndex =
                Grid.wrapCoordinate
                    inkIntensityGrid.width
                    (int (float32 columnIndex - flowVelocityX + 0.5f))

            let sampledSourceRowIndex =
                Grid.wrapCoordinate
                    inkIntensityGrid.height
                    (int (float32 rowIndex - flowVelocityY + 0.5f))

            let sampledSourceCellIndex =
                Grid.linearIndex
                    inkIntensityGrid.width
                    sampledSourceColumnIndex
                    sampledSourceRowIndex

            clampToUnitInterval (inkIntensityGrid.data[sampledSourceCellIndex] * 0.99f))

    let decay (decayFactor: float32) (inkIntensityGrid: Grid<float32>) =
        let clampedDecayFactor = clampToUnitInterval decayFactor

        { inkIntensityGrid with
            data =
                inkIntensityGrid.data
                |> Array.map (fun currentCellIntensity ->
                    clampToUnitInterval (currentCellIntensity * clampedDecayFactor)) }
