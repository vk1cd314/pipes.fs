namespace pipes.fs.App

open System
open pipes.fs.Domain.Types
open pipes.fs.Engine.Render

[<RequireQualifiedAccess>]
module Terminal =
    let enterAlt () =
        Console.Out.Write "\u001b[?1049h"

    let exitAlt () =
        Console.Out.Write "\u001b[?1049l"

    let hideCursor () =
        Console.Out.Write "\u001b[?25l"

    let showCursor () =
        Console.Out.Write "\u001b[?25h"

    let clear () =
        Console.Out.Write "\u001b[2J\u001b[H"

    let resetStyle () =
        Console.Out.Write AnsiEncoding.resetAnsi

    let getSize () =
        let terminalWidth =
            try
                max 1 Console.WindowWidth
            with _ ->
                80

        let terminalHeight =
            try
                max 1 Console.WindowHeight
            with _ ->
                24

        { Width  = terminalWidth
          Height = terminalHeight }

    let writeOperations (drawOperations: DrawOperation list) =
        if not (List.isEmpty drawOperations) then
            let encodedOutput = AnsiEncoding.encodeDrawOperations drawOperations

            if encodedOutput.Length > 0 then
                Console.Out.Write encodedOutput
                Console.Out.Flush()
