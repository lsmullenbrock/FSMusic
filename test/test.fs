module test

open NUnit.Framework
open System.Windows

[<Test>]
let ``test window`` () =
    let window = MusWindow.makeWindow MusResources.mainWindowWidth MusResources.mainWindowHeight

    (new Application()).Run(window)
    |> ignore

    stdin.ReadLine()
    |> ignore
