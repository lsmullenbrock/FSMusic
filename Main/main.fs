module Main
open System
open System.Windows

[<EntryPoint;STAThread>]
let main _ = 
    (new Application()).Run(MainWindow.getMainWindow())
    |> ignore
    //return
    0