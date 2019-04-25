module Main
open System
open System.Windows

[<EntryPoint;STAThread>]
let main _ = 

    //let window = makeDefaultWindow MusResources.mainWindowWidth MusResources.mainWindowHeight

    (new Application()).Run(MainWindow.mainWindow)
    |> ignore

    //return
    0
