namespace pipes.fs.Tests

open pipes.fs.Domain.Types
open pipes.fs.Engine.Simulation

module FlowTests =
    let tests =
        [ TestFramework.testCase "Flow field is deterministic" (fun () ->
                let firstVector = FlowField.field FlowKind.Noise 1.23f 10 12 80 40
                let secondVector = FlowField.field FlowKind.Noise 1.23f 10 12 80 40
                TestFramework.equal firstVector secondVector "Same input should return same vector") ]
