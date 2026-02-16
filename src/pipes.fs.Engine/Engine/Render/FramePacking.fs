namespace pipes.fs.Engine.Render

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module FramePacking =
    let emptyCellStyle: CellStyle = { 
        ForegroundColor = None
        BackgroundColor = None
        IsBold = false
    }

    let private quantizeColorChannelToSixBits (channelValue: byte) =
        ((int channelValue * 63) / 255) |> uint64

    let private expandSixBitChannelToByteRange (quantizedChannelValue: uint64) =
        ((int quantizedChannelValue * 255) / 63) |> byte

    let private packRgbColorWithBitShift
            (bitShift: int)
            (rgbColor: RgbColor)
            (packedFrameCellValue: PackedFrameCell)
        : PackedFrameCell =
        let packedRedChannel   = quantizeColorChannelToSixBits rgbColor.RedChannel <<< bitShift
        let packedGreenChannel = quantizeColorChannelToSixBits rgbColor.GreenChannel <<< (bitShift + 6)
        let packedBlueChannel  = quantizeColorChannelToSixBits rgbColor.BlueChannel <<< (bitShift + 12)
        packedFrameCellValue ||| packedRedChannel ||| packedGreenChannel ||| packedBlueChannel

    let private unpackRgbColorWithBitShift (bitShift: int) (packedFrameCellValue: PackedFrameCell) =
        let unpackedRedChannel   = expandSixBitChannelToByteRange ((packedFrameCellValue >>> bitShift) &&& 0x3FUL)
        let unpackedGreenChannel = expandSixBitChannelToByteRange ((packedFrameCellValue >>> (bitShift + 6)) &&& 0x3FUL)
        let unpackedBlueChannel  = expandSixBitChannelToByteRange ((packedFrameCellValue >>> (bitShift + 12)) &&& 0x3FUL)

        { RedChannel   = unpackedRedChannel
          GreenChannel = unpackedGreenChannel
          BlueChannel  = unpackedBlueChannel }

    let packFrameCell (characterToPack: char) (cellStyle: CellStyle) =
        let packedFrameCellWithCharacter = uint64 (uint16 characterToPack)

        let packedFrameCellWithBoldBit =
            if cellStyle.IsBold then
                packedFrameCellWithCharacter ||| (1UL <<< 16)
            else
                packedFrameCellWithCharacter

        let packedFrameCellWithForegroundColor =
            match cellStyle.ForegroundColor with
            | Some foregroundRgbColor ->
                packedFrameCellWithBoldBit
                ||| (1UL <<< 17)
                |> packRgbColorWithBitShift 18 foregroundRgbColor
            | None ->
                packedFrameCellWithBoldBit

        match cellStyle.BackgroundColor with
        | Some backgroundRgbColor ->
            packedFrameCellWithForegroundColor
            ||| (1UL <<< 36)
            |> packRgbColorWithBitShift 37 backgroundRgbColor
        | None ->
            packedFrameCellWithForegroundColor

    let unpackFrameCell (packedFrameCellValue: PackedFrameCell) =
        let unpackedCharacter = char (uint16 (packedFrameCellValue &&& 0xFFFFUL))
        let hasBoldStyle = ((packedFrameCellValue >>> 16) &&& 1UL) = 1UL

        let unpackedForegroundColor =
            if ((packedFrameCellValue >>> 17) &&& 1UL) = 1UL then
                Some(unpackRgbColorWithBitShift 18 packedFrameCellValue)
            else
                None

        let unpackedBackgroundColor =
            if ((packedFrameCellValue >>> 36) &&& 1UL) = 1UL then
                Some(unpackRgbColorWithBitShift 37 packedFrameCellValue)
            else
                None

        { Character = unpackedCharacter
          Style =
            { ForegroundColor = unpackedForegroundColor
              BackgroundColor = unpackedBackgroundColor
              IsBold = hasBoldStyle } }

    let characterOfPackedFrameCell (packedFrameCellValue: PackedFrameCell) =
        char (uint16 (packedFrameCellValue &&& 0xFFFFUL))

    let styleOfPackedFrameCell (packedFrameCellValue: PackedFrameCell) =
        (unpackFrameCell packedFrameCellValue).Style

    let blankFrame (frameWidth: int) (frameHeight: int) =
        let blankPackedFrameCell = packFrameCell ' ' emptyCellStyle

        { Width = frameWidth
          Height = frameHeight
          PackedCells = Array.create (frameWidth * frameHeight) blankPackedFrameCell }

    let frameOfCells
            (frameWidth: int)
            (frameHeight: int)
            (frameCells: FrameCell array)
        =
        if frameCells.Length <> frameWidth * frameHeight then
            invalidArg "frameCells" "Frame cell length does not match dimensions."

        { Width = frameWidth
          Height = frameHeight
          PackedCells = Array.map (fun currentFrameCell -> packFrameCell currentFrameCell.Character currentFrameCell.Style) frameCells }

    let cellsOfFrame (virtualFrame: VirtualFrame) =
        Array.map unpackFrameCell virtualFrame.PackedCells
