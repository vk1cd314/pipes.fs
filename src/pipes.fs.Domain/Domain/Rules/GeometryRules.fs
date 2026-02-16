namespace pipes.fs.Domain.Rules

open pipes.fs.Domain.Primitives
open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module GeometryRules =
    let clampToUnitInterval (inputValue: float32) =
        if inputValue < 0.0f then
            0.0f
        elif inputValue > 1.0f then
            1.0f
        else
            inputValue

    let directionBitMask (direction: Direction) =
        match direction with
        | Direction.North -> 1uy
        | Direction.East  -> 2uy
        | Direction.South -> 4uy
        | Direction.West  -> 8uy

    let oppositeDirection (direction: Direction) =
        match direction with
        | Direction.North -> Direction.South
        | Direction.East  -> Direction.West
        | Direction.South -> Direction.North
        | Direction.West  -> Direction.East

    let rotateLeft (direction: Direction) =
        match direction with
        | Direction.North -> Direction.West
        | Direction.East  -> Direction.North
        | Direction.South -> Direction.East
        | Direction.West  -> Direction.South

    let rotateRight (direction: Direction) =
        match direction with
        | Direction.North -> Direction.East
        | Direction.East  -> Direction.South
        | Direction.South -> Direction.West
        | Direction.West  -> Direction.North

    let moveWithWrapAround
            (dimensions: Dimensions)
            (currentPosition: GridPosition)
            (direction: Direction)
        : GridPosition =
        let nextColumnIndex, nextRowIndex =
            match direction with
            | Direction.North -> currentPosition.ColumnIndex,     currentPosition.RowIndex - 1
            | Direction.East  -> currentPosition.ColumnIndex + 1, currentPosition.RowIndex
            | Direction.South -> currentPosition.ColumnIndex,     currentPosition.RowIndex + 1
            | Direction.West  -> currentPosition.ColumnIndex - 1, currentPosition.RowIndex

        { ColumnIndex = Grid.wrapCoordinate dimensions.Width nextColumnIndex
          RowIndex    = Grid.wrapCoordinate dimensions.Height nextRowIndex }
