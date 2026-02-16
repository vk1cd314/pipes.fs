namespace pipes.fs.Engine.Render

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module DrawOperationOptimizer =
    type VirtualTerminalState = {
        CursorColumnIndex: int
        CursorRowIndex:    int
        ActiveCellStyle:   Option<CellStyle>
    }

    type OptimizerState = {
        VirtualTerminalState:            VirtualTerminalState
        OptimizedDrawOperationsReversed: List<DrawOperation>
    }

    let private initialVirtualTerminalState = {
        CursorColumnIndex = -1
        CursorRowIndex    = -1
        ActiveCellStyle   = None
    }

    let private appendTextChunk (textChunk: string) (optimizedDrawOperationsReversed: List<DrawOperation>) =
        if textChunk = "" then
            optimizedDrawOperationsReversed
        else
            match optimizedDrawOperationsReversed with
            | PutText existingTextChunk :: remainingDrawOperationsReversed ->
                PutText (existingTextChunk + textChunk) :: remainingDrawOperationsReversed
            | _ ->
                PutText textChunk :: optimizedDrawOperationsReversed

    let private step (optimizerState: OptimizerState) (currentDrawOperation: DrawOperation) =
        let currentVirtualTerminalState = optimizerState.VirtualTerminalState
        let currentOptimizedDrawOperationsReversed = optimizerState.OptimizedDrawOperationsReversed

        match currentDrawOperation with
        | MoveTo (targetColumnIndex, targetRowIndex) ->
            if
                targetColumnIndex = currentVirtualTerminalState.CursorColumnIndex
                && targetRowIndex = currentVirtualTerminalState.CursorRowIndex
            then
                optimizerState
            else
                { VirtualTerminalState =
                    { currentVirtualTerminalState with
                        CursorColumnIndex = targetColumnIndex
                        CursorRowIndex = targetRowIndex }
                  OptimizedDrawOperationsReversed =
                    currentDrawOperation :: currentOptimizedDrawOperationsReversed }
        | SetStyle replacementCellStyle ->
            if currentVirtualTerminalState.ActiveCellStyle = Some replacementCellStyle then
                optimizerState
            else
                { VirtualTerminalState =
                    { currentVirtualTerminalState with
                        ActiveCellStyle = Some replacementCellStyle }
                  OptimizedDrawOperationsReversed =
                    currentDrawOperation :: currentOptimizedDrawOperationsReversed }
        | PutText textChunk ->
            if textChunk = "" then
                optimizerState
            else
                { VirtualTerminalState =
                    { currentVirtualTerminalState with
                        CursorColumnIndex =
                            currentVirtualTerminalState.CursorColumnIndex + textChunk.Length }
                  OptimizedDrawOperationsReversed =
                    appendTextChunk textChunk currentOptimizedDrawOperationsReversed }

    let optimize (drawOperations: DrawOperation list) =
        let finalOptimizerState =
            drawOperations
            |> List.fold
                step
                { VirtualTerminalState = initialVirtualTerminalState
                  OptimizedDrawOperationsReversed = [] }

        finalOptimizerState.OptimizedDrawOperationsReversed
        |> List.rev
