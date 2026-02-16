namespace pipes.fs.Tests

module Program =
    [<EntryPoint>]
    let main _ =
        TestFramework.runAll Tests.all
