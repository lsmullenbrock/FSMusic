[<RequireQualifiedAccess>]
module AboutWindow

open System.Windows
open FsXaml


[<Literal>]
let private XAMLFile = "src/view/AboutWindow/aboutwindow.xaml"

type private AboutWindow = XAML<XAMLFile>

/// Exception thrown when using TextResources.resx upon aboutWindow.ShowDialog(). Will have to look into it.
[<Literal>]
let private aboutText ="""
Toy music engraver. Meant merely as a demo for right now and under heavy development. The point is to see how much I can actually get done in F# for desktop development, as well as setting the groundwork for a more robust music suite later on.
"""

/// Generates the About Window
let getAboutWindow () =
    let aboutWindow = AboutWindow()
    aboutWindow.Icon <- DrawingUtils.loadImageFile(GlyphLocations.Clefs.trebleClefLocation)
    aboutWindow.textBoxAbout.Text <- aboutText
    //return
    aboutWindow :> Window