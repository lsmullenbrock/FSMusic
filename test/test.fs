module test

open NUnit.Framework
open System.Windows

[<Test>]
let ``test window`` () =
    let window = MusWindow.makeDefaultWindow MusResources.mainWindowWidth MusResources.mainWindowHeight

    log "in test"
    assert false

    (new Application()).Run(window)
    |> ignore

    stdin.ReadLine()
    |> ignore
