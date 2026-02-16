namespace pipes.fs.Domain.Rules

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module PaletteRules =
    let paletteColors (paletteKind: PaletteKind) =
        match paletteKind with
        | PaletteKind.Neon ->
            [| 
                (10uy, 255uy, 160uy)
                (255uy, 80uy, 120uy)
                (80uy, 200uy, 255uy)
                (250uy, 245uy, 80uy) 
            |]
        | PaletteKind.Ocean ->
            [| 
                (70uy, 180uy, 255uy)
                (40uy, 120uy, 210uy)
                (120uy, 230uy, 240uy)
                (180uy, 210uy, 255uy) 
            |]
        | PaletteKind.Ember ->
            [| 
                (255uy, 130uy, 40uy)
                (255uy, 70uy, 30uy)
                (255uy, 200uy, 90uy)
                (190uy, 40uy, 20uy) 
            |]
        | PaletteKind.Mono ->
            [| 
                (240uy, 240uy, 240uy)
                (200uy, 200uy, 200uy)
                (150uy, 150uy, 150uy)
                (100uy, 100uy, 100uy) 
            |]
