namespace pipes.fs.Domain.Defaults

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module ConfigDefaults =
    let defaultSimulationConfig: SimulationConfig = { 
        FramesPerSecond    = 60
        WalkerCount        = 32
        TurnProbability    = 0.35f
        RespawnProbability = 0.015f
        DiffusionAmount    = 0.12f
        DecayFactor        = 0.985f
        FlowKind           = FlowKind.Swirl
        SimulationMode     = SimulationMode.Ink
        PaletteKind        = PaletteKind.Neon
        RandomSeed         = 0xC0FFEEUL
        ShowHudOverlay     = false 
    }
