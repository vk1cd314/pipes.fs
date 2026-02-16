namespace pipes.fs.Domain.Types

type Dimensions = {
    Width:  int
    Height: int
}

type GridPosition = {
    ColumnIndex: int
    RowIndex:    int
}

type Direction =
| North
| East
| South
| West

type FlowKind =
| Swirl
| Wind
| Noise
| NoFlow

type SimulationMode =
| Classic
| Ink

type PaletteKind =
| Neon
| Ocean
| Ember
| Mono

type DeterministicRandomState = uint64
