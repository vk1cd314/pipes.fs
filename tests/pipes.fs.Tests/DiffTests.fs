namespace pipes.fs.Tests

open pipes.fs.Domain.Types
open pipes.fs.Engine.Render

module DiffTests =
    let private defaultCellStyle =
        { ForegroundColor =
            Some
                { RedChannel   = 255uy
                  GreenChannel = 255uy
                  BlueChannel  = 255uy }
          BackgroundColor = None
          IsBold          = false }

    let private alternateCellStyle =
        { ForegroundColor =
            Some
                { RedChannel   = 120uy
                  GreenChannel = 220uy
                  BlueChannel  = 255uy }
          BackgroundColor = None
          IsBold          = true }

    let private createFrame
            width
            height
            text
        =
        let frameCells =
            text
            |> Seq.map (fun characterValue ->
                { Character = characterValue
                  Style     = defaultCellStyle })
            |> Seq.toArray

        FramePacking.frameOfCells width height frameCells

    let tests =
        [ TestFramework.testCase "FrameDiff: Equal frames produce no operations" (fun () ->
                let frame = createFrame 4 1 "abcd"
                let operations = FrameDiff.diff frame frame
                TestFramework.isEmpty operations "No operations expected")

          TestFramework.testCase "FrameDiff: Single changed cell emits minimal operation triplet" (fun () ->
                let previousFrame = createFrame 3 1 "abc"
                let nextFrame = createFrame 3 1 "axc"
                let operations = FrameDiff.diff previousFrame nextFrame

                TestFramework.equal
                    [ MoveTo(1, 0); SetStyle defaultCellStyle; PutText "x" ]
                    operations
                    "Single cell change should emit one run")

          TestFramework.testCase "FrameDiff: Row run packs into one PutText" (fun () ->
                let blankFrame = FramePacking.blankFrame 5 1

                let nextFrame =
                    [| { Character = 'h'; Style = defaultCellStyle }
                       { Character = 'e'; Style = defaultCellStyle }
                       { Character = 'l'; Style = defaultCellStyle }
                       { Character = 'l'; Style = defaultCellStyle }
                       { Character = 'o'; Style = defaultCellStyle } |]
                    |> FramePacking.frameOfCells 5 1

                let operations = FrameDiff.diff blankFrame nextFrame
                TestFramework.equal
                    [ MoveTo(0, 0); SetStyle defaultCellStyle; PutText "hello" ]
                    operations
                    "Whole row should be one run")

          TestFramework.testCase "FrameDiff: Style boundary splits into separate runs" (fun () ->
                let blankFrame = FramePacking.blankFrame 2 1

                let nextFrame =
                    [| { Character = 'a'; Style = defaultCellStyle }
                       { Character = 'b'; Style = alternateCellStyle } |]
                    |> FramePacking.frameOfCells 2 1

                let operations = FrameDiff.diff blankFrame nextFrame

                let expectedFirstCellStyle = FramePacking.styleOfPackedFrameCell nextFrame.PackedCells[0]
                let expectedSecondCellStyle = FramePacking.styleOfPackedFrameCell nextFrame.PackedCells[1]

                TestFramework.equal
                    [ MoveTo(0, 0)
                      SetStyle expectedFirstCellStyle
                      PutText "a"
                      MoveTo(1, 0)
                      SetStyle expectedSecondCellStyle
                      PutText "b" ]
                    operations
                    "Changed cells with different styles should emit separate runs"
          ) ]
