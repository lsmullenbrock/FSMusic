module Main
open System
open MusWindow
open System.Windows

[<EntryPoint;STAThread>]
let main _ = 

    let window = makeDefaultWindow MusResources.mainWindowWidth MusResources.mainWindowHeight

    (new Application()).Run(window)
    |> ignore

    //return
    0
