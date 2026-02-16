namespace pipes.fs.Engine.Render

open pipes.fs.Domain.Rules
open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module ModelRenderer =
    let private boxDrawingCharactersByMask =
        [| ' '
           '│'
           '─'
           '└'
           '│'
           '│'
           '┌'
           '├'
           '─'
           '┘'
           '─'
           '┴'
           '┐'
           '┤'
           '┬'
           '┼' |]

    let private shadeRampCharacters =
        [| ' '; '░'; '▒'; '▓'; '█' |]

    let private createRgbColor (redChannel, greenChannel, blueChannel) =
        { RedChannel   = redChannel
          GreenChannel = greenChannel
          BlueChannel  = blueChannel }

    let private scaleRgbColor (brightnessScale: float32) (redChannel, greenChannel, blueChannel) =
        let clampToByteRange channelValue =
            if channelValue < 0.0f then
                0uy
            elif channelValue > 255.0f then
                255uy
            else
                byte channelValue

        let clampedBrightnessScale = max 0.0f brightnessScale

        clampToByteRange (float32 redChannel * clampedBrightnessScale),
        clampToByteRange (float32 greenChannel * clampedBrightnessScale),
        clampToByteRange (float32 blueChannel * clampedBrightnessScale)

    let boxCharacterForMask (directionMask: byte) =
        boxDrawingCharactersByMask[int directionMask &&& 0x0F]

    let private buildInkHazeStyle (paletteColorTriplets: (byte * byte * byte) array) (clampedInkIntensity: float32) =
        if Array.isEmpty paletteColorTriplets then
            None
        else
            let brightnessScale = 0.2f + (0.85f * clampedInkIntensity)
            let tintColorTriplet = paletteColorTriplets[0]
            let scaledRedChannel, scaledGreenChannel, scaledBlueChannel = scaleRgbColor brightnessScale tintColorTriplet

            Some { 
                ForegroundColor =
                    Some { 
                        RedChannel = scaledRedChannel
                        GreenChannel = scaledGreenChannel
                        BlueChannel = scaledBlueChannel 
                    }
                BackgroundColor = None
                IsBold = false 
            }

    let private packedCellForIndex
            (simulationModel:            SimulationModel)
            (paletteColorTriplets:       (byte * byte * byte) array)
            (availablePaletteColorCount: int)
            (isInkModeEnabled:           bool)
            currentCellIndex
        : PackedFrameCell =
        let geometryDirectionMask = simulationModel.GeometryMaskGrid.data[currentCellIndex]

        if geometryDirectionMask <> 0uy then
            let geometryCharacter = boxCharacterForMask geometryDirectionMask
            let paletteColorIndex = int simulationModel.PigmentColorGrid.data[currentCellIndex] % availablePaletteColorCount

            let geometryStyle =
                { ForegroundColor = Some(createRgbColor paletteColorTriplets[paletteColorIndex])
                  BackgroundColor = None
                  IsBold = true }

            FramePacking.packFrameCell geometryCharacter geometryStyle
        else
            let currentInkIntensity = simulationModel.InkIntensityGrid.data[currentCellIndex]

            if isInkModeEnabled && currentInkIntensity > 0.03f then
                let clampedInkIntensity = GeometryRules.clampToUnitInterval currentInkIntensity

                let shadeCharacterIndex =
                    min
                        (shadeRampCharacters.Length - 1)
                        (int (clampedInkIntensity * float32 (shadeRampCharacters.Length - 1)))

                match buildInkHazeStyle paletteColorTriplets clampedInkIntensity with
                | Some inkHazeStyle ->
                    FramePacking.packFrameCell shadeRampCharacters[shadeCharacterIndex] inkHazeStyle
                | None ->
                    FramePacking.packFrameCell ' ' FramePacking.emptyCellStyle
            else
                FramePacking.packFrameCell ' ' FramePacking.emptyCellStyle

    let renderModel (simulationConfig: SimulationConfig) (simulationModel: SimulationModel) =
        let frameWidth = simulationModel.Dimensions.Width
        let frameHeight = simulationModel.Dimensions.Height
        let paletteColorTriplets = PaletteRules.paletteColors simulationConfig.PaletteKind
        let availablePaletteColorCount = max 1 paletteColorTriplets.Length
        let isInkModeEnabled = simulationConfig.SimulationMode = SimulationMode.Ink

        let packedFrameCells =
            simulationModel.GeometryMaskGrid.data
            |> Array.mapi (fun currentCellIndex _ ->
                packedCellForIndex
                    simulationModel
                    paletteColorTriplets
                    availablePaletteColorCount
                    isInkModeEnabled
                    currentCellIndex)

        { Width = frameWidth
          Height = frameHeight
          PackedCells = packedFrameCells }
