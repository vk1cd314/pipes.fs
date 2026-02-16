namespace pipes.fs.Engine.Render

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module AnsiEncoding =
    let private escapeSequencePrefix = "\u001b["
    let resetAnsi = escapeSequencePrefix + "0m"

    let moveToAnsi (columnIndex, rowIndex) =
        let ansiRowIndex    = rowIndex + 1
        let ansiColumnIndex = columnIndex + 1
        $"{escapeSequencePrefix}{ansiRowIndex};{ansiColumnIndex}H"

    let styleToAnsi (cellStyle: CellStyle) =
        let boldSegment =
            if cellStyle.IsBold then
                "1"
            else
                "22"

        let foregroundSegment =
            match cellStyle.ForegroundColor with
            | Some foregroundColor ->
                $"38;2;{foregroundColor.RedChannel};{foregroundColor.GreenChannel};{foregroundColor.BlueChannel}"
            | None ->
                "39"

        let backgroundSegment =
            match cellStyle.BackgroundColor with
            | Some backgroundColor ->
                $"48;2;{backgroundColor.RedChannel};{backgroundColor.GreenChannel};{backgroundColor.BlueChannel}"
            | None ->
                "49"

        let joinedAnsiStyleSegments =
            [ boldSegment; foregroundSegment; backgroundSegment ]
            |> String.concat ";"

        $"{escapeSequencePrefix}{joinedAnsiStyleSegments}m"

    let drawOperationToAnsi (currentDrawOperation: DrawOperation) =
        match currentDrawOperation with
        | MoveTo   (columnIndex, rowIndex) -> moveToAnsi (columnIndex, rowIndex)
        | SetStyle cellStyle               -> styleToAnsi cellStyle
        | PutText  textChunk               -> textChunk

    let encodeDrawOperations (drawOperations: DrawOperation list) =
        if List.isEmpty drawOperations then
            ""
        else
            let encodedDrawOperations =
                drawOperations
                |> List.map drawOperationToAnsi
                |> String.concat ""

            resetAnsi + encodedDrawOperations + resetAnsi
