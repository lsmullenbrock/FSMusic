open System
open MusWindow
open System.Windows

[<EntryPoint;STAThread>]
let main _ = 

    let window = makeWindow MusResources.mainWindowWidth MusResources.mainWindowHeight

    (new Application()).Run(window)
    |> ignore

    //return
    0
