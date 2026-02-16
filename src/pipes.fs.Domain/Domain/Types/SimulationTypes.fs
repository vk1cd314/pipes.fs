namespace pipes.fs.Domain.Types

open pipes.fs.Domain.Primitives

type WalkerState = {
    Position:       GridPosition
    Direction:      Direction
    PaletteColorId: byte
    ThicknessLevel: byte
}

type SimulationConfig = {
    FramesPerSecond:    int
    WalkerCount:        int
    TurnProbability:    float32
    RespawnProbability: float32
    DiffusionAmount:    float32
    DecayFactor:        float32
    FlowKind:           FlowKind
    SimulationMode:     SimulationMode
    PaletteKind:        PaletteKind
    RandomSeed:         uint64
    ShowHudOverlay:     bool
}

type SimulationModel = {
    GeometryMaskGrid:             Grid<byte>
    InkIntensityGrid:             Grid<float32>
    PigmentColorGrid:             Grid<byte>
    WalkerStates:                 array<WalkerState>
    DeterministicRandomState:     DeterministicRandomState
    ElapsedSimulationTimeSeconds: float32
    Dimensions:                   Dimensions
}
