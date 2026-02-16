namespace pipes.fs.Tests

open pipes.fs.Domain.Types
open pipes.fs.Engine.Render

module OpsOptimizeTests =
    let private defaultCellStyle =
        { ForegroundColor =
            Some
                { RedChannel   = 255uy
                  GreenChannel = 255uy
                  BlueChannel  = 255uy }
          BackgroundColor = None
          IsBold          = false }

    let tests =
        [ TestFramework.testCase "DrawOperationOptimizer: Drops redundant MoveTo and SetStyle" (fun () ->
                let drawOperations =
                    [ MoveTo(0, 0)
                      MoveTo(0, 0)
                      SetStyle defaultCellStyle
                      SetStyle defaultCellStyle
                      PutText "a" ]

                let optimizedOperations = DrawOperationOptimizer.optimize drawOperations

                TestFramework.equal
                    [ MoveTo(0, 0); SetStyle defaultCellStyle; PutText "a" ]
                    optimizedOperations
                    "Redundant operations should be removed"
          )

          TestFramework.testCase "DrawOperationOptimizer: Merges adjacent PutText" (fun () ->
                let drawOperations = [ MoveTo(0, 0); PutText "a"; PutText "b"; PutText "c" ]
                let optimizedOperations = DrawOperationOptimizer.optimize drawOperations
                TestFramework.equal [ MoveTo(0, 0); PutText "abc" ] optimizedOperations "Text operations should merge")

          TestFramework.testCase "DrawOperationOptimizer: Optimize is idempotent" (fun () ->
                let drawOperations =
                    [ MoveTo(0, 0)
                      SetStyle defaultCellStyle
                      PutText "ab"
                      PutText ""
                      MoveTo(2, 0)
                      MoveTo(2, 0)
                      SetStyle defaultCellStyle
                      PutText "c" ]

                let optimizedOnce = DrawOperationOptimizer.optimize drawOperations
                let optimizedTwice = DrawOperationOptimizer.optimize optimizedOnce

                TestFramework.equal optimizedOnce optimizedTwice "Optimizing twice should equal optimizing once") ]
