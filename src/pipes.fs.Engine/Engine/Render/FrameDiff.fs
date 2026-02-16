namespace pipes.fs.Engine.Render

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module FrameDiff =
    type DiffRun = {
        StartColumnIndex: int
        RowIndex:         int
        CellStyle:        CellStyle
        RunText:          string
    }

    let private haveMatchingDimensions (firstFrame: VirtualFrame) (secondFrame: VirtualFrame) =
        firstFrame.Width = secondFrame.Width && firstFrame.Height = secondFrame.Height

    let private rowRuns
            (previousFrame: VirtualFrame)
            (nextFrame: VirtualFrame)
            (rowIndex: int)
        =
        let rowOffset = rowIndex * nextFrame.Width
        let cellIndexAtColumn columnIndex = rowOffset + columnIndex

        let rec consumeRun
                columnIndex
                runStyle
                runCharactersReversed
            =
            if columnIndex >= nextFrame.Width then
                columnIndex, runCharactersReversed
            else
                let currentCellIndex = cellIndexAtColumn columnIndex
                let previousPackedCell = previousFrame.PackedCells[currentCellIndex]
                let nextPackedCell = nextFrame.PackedCells[currentCellIndex]

                if previousPackedCell = nextPackedCell then
                    columnIndex, runCharactersReversed
                else
                    let currentCellStyle = FramePacking.styleOfPackedFrameCell nextPackedCell

                    if currentCellStyle <> runStyle then
                        columnIndex, runCharactersReversed
                    else
                        let currentCharacter = FramePacking.characterOfPackedFrameCell nextPackedCell
                        consumeRun (columnIndex + 1) runStyle (currentCharacter :: runCharactersReversed)

        let rec scanColumns columnIndex discoveredRunsReversed =
            if columnIndex >= nextFrame.Width then
                List.rev discoveredRunsReversed
            else
                let currentCellIndex = cellIndexAtColumn columnIndex
                let previousPackedCell = previousFrame.PackedCells[currentCellIndex]
                let nextPackedCell = nextFrame.PackedCells[currentCellIndex]

                if previousPackedCell = nextPackedCell then
                    scanColumns (columnIndex + 1) discoveredRunsReversed
                else
                    let runStyle = FramePacking.styleOfPackedFrameCell nextPackedCell
                    let runEndColumnIndex, runCharactersReversed = consumeRun columnIndex runStyle []
                    let runText = System.String(runCharactersReversed |> List.rev |> List.toArray)

                    let discoveredRun =
                        { StartColumnIndex = columnIndex
                          RowIndex         = rowIndex
                          CellStyle        = runStyle
                          RunText          = runText }

                    scanColumns runEndColumnIndex (discoveredRun :: discoveredRunsReversed)

        scanColumns 0 []

    let private runsToDrawOperations (diffRuns: DiffRun list) =
        diffRuns
        |> List.collect (fun currentDiffRun ->
            [ MoveTo(currentDiffRun.StartColumnIndex, currentDiffRun.RowIndex)
              SetStyle currentDiffRun.CellStyle
              PutText currentDiffRun.RunText ])

    let rec diff (previousFrame: VirtualFrame) (nextFrame: VirtualFrame) =
        if not (haveMatchingDimensions previousFrame nextFrame) then
            let blankReferenceFrame = FramePacking.blankFrame nextFrame.Width nextFrame.Height
            diff blankReferenceFrame nextFrame
        else
            [ 0 .. nextFrame.Height - 1 ]
            |> List.collect (rowRuns previousFrame nextFrame)
            |> runsToDrawOperations
