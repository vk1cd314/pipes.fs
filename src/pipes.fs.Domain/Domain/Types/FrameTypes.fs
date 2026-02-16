namespace pipes.fs.Domain.Types

type RgbColor = {
    RedChannel:   byte
    GreenChannel: byte
    BlueChannel:  byte
}

type CellStyle = {
    ForegroundColor: Option<RgbColor>
    BackgroundColor: Option<RgbColor>
    IsBold:          bool
}

type FrameCell = {
    Character: char
    Style:     CellStyle
}

type PackedFrameCell = uint64

type VirtualFrame = {
    Width:       int
    Height:      int
    PackedCells: array<PackedFrameCell>
}

type DrawOperation =
| MoveTo   of int * int
| SetStyle of CellStyle
| PutText  of string
