namespace pipes.fs.Tests

open pipes.fs.Domain.Defaults
open pipes.fs.Domain.Types
open pipes.fs.Engine.Simulation

module SimDeterminismTests =
    let private runSimulationSteps
            stepCount
            simulationConfig
            dimensions
        =
        [ 1 .. stepCount ]
        |> List.fold
            (fun currentSimulationModel _ ->
                SimulationRuntime.step simulationConfig 0.016f currentSimulationModel)
            (SimulationRuntime.initModel simulationConfig dimensions)

    let tests =
        [ TestFramework.testCase "Simulation determinism: Same seed and config produce same state" (fun () ->
                let simulationConfig =
                    { ConfigDefaults.defaultSimulationConfig with
                        RandomSeed     = 12345UL
                        ShowHudOverlay = false }

                let dimensions =
                    { Width  = 50
                      Height = 20 }

                let firstSimulationModel = runSimulationSteps 25 simulationConfig dimensions
                let secondSimulationModel = runSimulationSteps 25 simulationConfig dimensions

                TestFramework.equal
                    firstSimulationModel.GeometryMaskGrid.data
                    secondSimulationModel.GeometryMaskGrid.data
                    "Geometry should match"

                TestFramework.equal
                    firstSimulationModel.PigmentColorGrid.data
                    secondSimulationModel.PigmentColorGrid.data
                    "Pigment should match"

                TestFramework.equal
                    firstSimulationModel.InkIntensityGrid.data
                    secondSimulationModel.InkIntensityGrid.data
                    "Ink should match") ]
