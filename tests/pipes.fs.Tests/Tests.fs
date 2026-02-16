namespace pipes.fs.Tests

module Tests =
    let all =
        [ FrameTests.tests
          DiffTests.tests
          OpsOptimizeTests.tests
          FlowTests.tests
          SimDeterminismTests.tests
          PropertyTests.tests ]
        |> List.concat
