namespace pipes.fs.Tests

open pipes.fs.Engine.Render

module FrameTests =
    let tests =
        [ TestFramework.testCase "Render box chars: NS -> vertical" (fun () ->
            TestFramework.equal '│' (ModelRenderer.boxCharacterForMask 5uy) "Mask 5 should render vertical pipe")

          TestFramework.testCase "Render box chars: EW -> horizontal" (fun () ->
              TestFramework.equal '─' (ModelRenderer.boxCharacterForMask 10uy) "Mask 10 should render horizontal pipe")

          TestFramework.testCase "Render box chars: All -> cross" (fun () ->
              TestFramework.equal '┼' (ModelRenderer.boxCharacterForMask 15uy) "Mask 15 should render cross") ]
