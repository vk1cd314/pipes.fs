namespace pipes.fs.Engine.Config

open System
open pipes.fs.Domain.Defaults
open pipes.fs.Domain.Rules
open pipes.fs.Domain.Types
open pipes.fs.Engine.Abstractions

[<RequireQualifiedAccess>]
module CliParsing =
    let usageText =
        [ "Usage: pipes.fs [options]"
          ""
          "Options:"
          "  --fps <int>"
          "  --walkers <int>"
          "  --turn <float 0..1>"
          "  --spawn <float 0..1>"
          "  --diffuse <float 0..1>"
          "  --decay <float 0..1>"
          "  --flow <swirl|wind|noise|none>"
          "  --mode <classic|ink>"
          "  --palette <neon|ocean|ember|mono>"
          "  --seed <uint64>"
          "  --hud <true|false>" ]
        |> String.concat Environment.NewLine

    let private parseFlowKind (argumentValue: string) =
        match argumentValue.ToLowerInvariant() with
        | "swirl" -> Ok FlowKind.Swirl
        | "wind"  -> Ok FlowKind.Wind
        | "noise" -> Ok FlowKind.Noise
        | "none"  -> Ok FlowKind.NoFlow
        | _       -> Error $"Invalid flow: {argumentValue}"

    let private parseSimulationMode (argumentValue: string) =
        match argumentValue.ToLowerInvariant() with
        | "classic" -> Ok SimulationMode.Classic
        | "ink"     -> Ok SimulationMode.Ink
        | _         -> Error $"Invalid mode: {argumentValue}"

    let private parsePaletteKind (argumentValue: string) =
        match argumentValue.ToLowerInvariant() with
        | "neon"  -> Ok PaletteKind.Neon
        | "ocean" -> Ok PaletteKind.Ocean
        | "ember" -> Ok PaletteKind.Ember
        | "mono"  -> Ok PaletteKind.Mono
        | _       -> Error $"Invalid palette: {argumentValue}"

    let private requireArgumentValue (argv: string array) (argumentIndex: int) =
        if argumentIndex + 1 >= argv.Length then
            Error $"Missing value for {argv[argumentIndex]}"
        else
            Ok argv[argumentIndex + 1]

    let private parseIntArgument (argumentValue: string) (argumentName: string) =
        match Int32.TryParse argumentValue with
        | true, parsedIntValue -> Ok parsedIntValue
        | _                    -> Error $"Invalid int for {argumentName}: {argumentValue}"

    let private parseFloatArgument (argumentValue: string) (argumentName: string) =
        match Single.TryParse argumentValue with
        | true, parsedFloatValue -> Ok parsedFloatValue
        | _                      -> Error $"Invalid float for {argumentName}: {argumentValue}"

    let private parseUInt64Argument (argumentValue: string) (argumentName: string) =
        match UInt64.TryParse argumentValue with
        | true, parsedUInt64Value -> Ok parsedUInt64Value
        | _                       -> Error $"Invalid uint64 for {argumentName}: {argumentValue}"

    let private parseBoolArgument (argumentValue: string) (argumentName: string) =
        match Boolean.TryParse argumentValue with
        | true, parsedBoolValue -> Ok parsedBoolValue
        | _                     -> Error $"Invalid bool for {argumentName}: {argumentValue}"

    let parse (argv: string array) =
        let rec parseLoop (currentConfig: SimulationConfig) argumentIndex =
            let advanceWithUpdatedConfig updatedConfig =
                parseLoop updatedConfig (argumentIndex + 2)

            let parseAndSetInt argumentName setterFunction =
                result {
                    let! argumentValue = requireArgumentValue argv argumentIndex
                    let! parsedValue = parseIntArgument argumentValue argumentName
                    return! advanceWithUpdatedConfig (setterFunction currentConfig parsedValue)
                }

            let parseAndSetFloat argumentName setterFunction =
                result {
                    let! argumentValue = requireArgumentValue argv argumentIndex
                    let! parsedValue = parseFloatArgument argumentValue argumentName
                    return! advanceWithUpdatedConfig (setterFunction currentConfig parsedValue)
                }

            let parseAndSetBool argumentName setterFunction =
                result {
                    let! argumentValue = requireArgumentValue argv argumentIndex
                    let! parsedValue = parseBoolArgument argumentValue argumentName
                    return! advanceWithUpdatedConfig (setterFunction currentConfig parsedValue)
                }

            let parseAndSetUInt64 argumentName setterFunction =
                result {
                    let! argumentValue = requireArgumentValue argv argumentIndex
                    let! parsedValue = parseUInt64Argument argumentValue argumentName
                    return! advanceWithUpdatedConfig (setterFunction currentConfig parsedValue)
                }

            result {
                if argumentIndex >= argv.Length then
                    return currentConfig
                else
                    let currentArgumentName = argv[argumentIndex]

                    match currentArgumentName with
                    | "--fps" ->
                        return!
                            parseAndSetInt "--fps" (fun config parsedFramesPerSecond -> {
                                config with
                                    FramesPerSecond = max 1 parsedFramesPerSecond
                            })
                    | "--walkers" ->
                        return!
                            parseAndSetInt "--walkers" (fun config parsedWalkerCount -> {
                                config with
                                    WalkerCount = max 1 parsedWalkerCount
                            })
                    | "--turn" ->
                        return!
                            parseAndSetFloat "--turn" (fun config parsedTurnProbability -> {
                                config with
                                    TurnProbability = GeometryRules.clampToUnitInterval parsedTurnProbability
                            })
                    | "--spawn" ->
                        return!
                            parseAndSetFloat "--spawn" (fun config parsedRespawnProbability -> {
                                config with
                                    RespawnProbability = GeometryRules.clampToUnitInterval parsedRespawnProbability
                            })
                    | "--diffuse" ->
                        return!
                            parseAndSetFloat "--diffuse" (fun config parsedDiffusionAmount -> {
                                config with
                                    DiffusionAmount = GeometryRules.clampToUnitInterval parsedDiffusionAmount
                            })
                    | "--decay" ->
                        return!
                            parseAndSetFloat "--decay" (fun config parsedDecayFactor -> {
                                config with
                                    DecayFactor = GeometryRules.clampToUnitInterval parsedDecayFactor
                            })
                    | "--flow" ->
                        let! argumentValue = requireArgumentValue argv argumentIndex
                        let! parsedFlowKind = parseFlowKind argumentValue
                        return!
                            advanceWithUpdatedConfig {
                                currentConfig with
                                    FlowKind = parsedFlowKind
                            }
                    | "--mode" ->
                        let! argumentValue = requireArgumentValue argv argumentIndex
                        let! parsedSimulationMode = parseSimulationMode argumentValue
                        return!
                            advanceWithUpdatedConfig {
                                currentConfig with
                                    SimulationMode = parsedSimulationMode
                            }
                    | "--palette" ->
                        let! argumentValue = requireArgumentValue argv argumentIndex
                        let! parsedPaletteKind = parsePaletteKind argumentValue
                        return!
                            advanceWithUpdatedConfig {
                                currentConfig with
                                    PaletteKind = parsedPaletteKind
                            }
                    | "--seed" ->
                        return!
                            parseAndSetUInt64 "--seed" (fun config parsedRandomSeed -> {
                                config with
                                    RandomSeed = parsedRandomSeed
                            })
                    | "--hud" ->
                        return!
                            parseAndSetBool "--hud" (fun config parsedHudVisibility -> {
                                config with
                                    ShowHudOverlay = parsedHudVisibility
                            })
                    | "--help"
                    | "-h" ->
                        return! Error usageText
                    | unknownArgumentName ->
                        return! Error $"Unknown option: {unknownArgumentName}{Environment.NewLine}{usageText}"
            }

        parseLoop ConfigDefaults.defaultSimulationConfig 0
