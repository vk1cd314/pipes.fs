namespace pipes.fs.Tests

open System

module TestFramework =
    type TestCase = {
        Name: string
        Run:  unit -> unit
    }

    let testCase testName testExecution =
        { Name = testName
          Run  = testExecution }

    let fail failureMessage =
        failwith failureMessage

    let equal
            expectedValue
            actualValue
            message
        =
        if actualValue <> expectedValue then
            failwith $"{message} | expected: %A{expectedValue}, actual: %A{actualValue}"

    let isTrue predicateValue message =
        if not predicateValue then
            failwith message

    let isEmpty (values: 'value list) message =
        if not (List.isEmpty values) then
            failwith $"{message} | actual: %A{values}"

    let runAll (testCases: TestCase list) =
        let executeSingleTestCase testCase =
            try
                testCase.Run ()
                Console.WriteLine($"[PASS] {testCase.Name}")
                true
            with exceptionObject ->
                Console.WriteLine($"[FAIL] {testCase.Name}")
                Console.WriteLine($"       {exceptionObject.Message}")
                false

        let passedTestCount =
            testCases
            |> List.fold
                (fun currentPassedCount currentTestCase ->
                    if executeSingleTestCase currentTestCase then
                        currentPassedCount + 1
                    else
                        currentPassedCount)
                0

        let failedTestCount = testCases.Length - passedTestCount
        Console.WriteLine($"Executed {testCases.Length} tests: {passedTestCount} passed, {failedTestCount} failed.")

        if failedTestCount = 0 then
            0
        else
            1
