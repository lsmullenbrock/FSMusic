open System
open MusWindow
open System.Windows

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint;STAThread>]
let main argv = 

    let window = makeWindow MusResources.mainWindowWidth MusResources.mainWindowHeight

    (new Application()).Run(window)
    |> ignore

    printfn "%A" argv
    0 // return an integer exit code
