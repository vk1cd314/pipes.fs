namespace pipes.fs.App

open System
open System.Diagnostics
open System.Threading
open pipes.fs.Domain.Types
open pipes.fs.Engine.Config

module Program =
    let run (simulationConfig: SimulationConfig) =
        use applicationCancellationSource = new CancellationTokenSource()
        let frameStopwatch = Stopwatch.StartNew()
        let targetFrameBudgetMilliseconds = 1000.0 / float (max 1 simulationConfig.FramesPerSecond)

        Console.CancelKeyPress.Add(fun cancelKeyPressEventArgs ->
            cancelKeyPressEventArgs.Cancel <- true
            applicationCancellationSource.Cancel()
        )

        let initialTerminalDimensions = Terminal.getSize ()
        let initialRuntimeLoopState = RuntimeLoop.createInitialState simulationConfig initialTerminalDimensions frameStopwatch.Elapsed

        try
            Terminal.enterAlt ()
            Terminal.hideCursor ()
            Terminal.clear ()

            let rec continueRuntimeLoop (currentRuntimeLoopState: RuntimeLoop.RuntimeLoopState) =
                if applicationCancellationSource.IsCancellationRequested then
                    0
                else
                    let currentFrameTimestamp = frameStopwatch.Elapsed

                    let runtimeStepOutput =
                        RuntimeLoop.advanceOneFrame
                            simulationConfig
                            { CurrentFrameTimestamp = currentFrameTimestamp
                              LatestTerminalDimensions = Terminal.getSize () }
                            currentRuntimeLoopState

                    if runtimeStepOutput.ShouldClearTerminal then
                        Terminal.clear ()

                    Terminal.writeOperations runtimeStepOutput.OptimizedDrawOperations

                    let elapsedFrameMilliseconds = (frameStopwatch.Elapsed - currentFrameTimestamp).TotalMilliseconds
                    let sleepMilliseconds = (targetFrameBudgetMilliseconds - elapsedFrameMilliseconds) |> int

                    if sleepMilliseconds > 0 then
                        Thread.Sleep sleepMilliseconds

                    continueRuntimeLoop runtimeStepOutput.NextRuntimeLoopState

            continueRuntimeLoop initialRuntimeLoopState
        finally
            Terminal.resetStyle ()
            Terminal.showCursor ()
            Terminal.exitAlt ()
            Console.Out.Flush()

    [<EntryPoint>]
    let main (argv: string array) =
        match CliParsing.parse argv with
        | Ok simulationConfig ->
            run simulationConfig
        | Error errorMessage ->
            Console.Error.WriteLine(errorMessage)
            1
