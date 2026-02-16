namespace pipes.fs.Domain.Primitives

[<Struct>]
type Grid<'value> = {
    width:  int
    height: int
    data:   array<'value>
}

[<RequireQualifiedAccess>]
module Grid =
    let inline linearIndex
            (gridWidth: int)
            (columnIndex: int)
            (rowIndex: int)
            =
        (rowIndex * gridWidth) + columnIndex

    let create
            (gridWidth: int)
            (gridHeight: int)
            (initialCellValue: 'value)
        : Grid<'value> =
        if gridWidth <= 0 || gridHeight <= 0 then
            invalidArg "gridWidth/gridHeight" "Grid dimensions must be positive."

        { width  = gridWidth
          height = gridHeight
          data   = Array.create (gridWidth * gridHeight) initialCellValue }

    let mapCellsWithCoordinates (indexedCellMapper: int -> int -> 'value -> 'mappedValue) (grid: Grid<'value>) =
        let mappedCellData =
            Array.mapi (fun currentLinearIndex currentCellValue ->
                let currentColumnIndex = currentLinearIndex % grid.width
                let currentRowIndex    = currentLinearIndex / grid.width
                indexedCellMapper currentColumnIndex currentRowIndex currentCellValue
            ) grid.data

        { width  = grid.width
          height = grid.height
          data   = mappedCellData }

    let inline wrapCoordinate (bound: int) (coordinateValue: int) =
        if bound <= 0 then
            0
        else
            let wrappedRemainder = coordinateValue % bound
            if wrappedRemainder < 0 then
                wrappedRemainder + bound
            else
                wrappedRemainder
