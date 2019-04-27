[<RequireQualifiedAccess>]
module AboutWindow

open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

[<Literal>]
let private XAMLFile = __SOURCE_DIRECTORY__ + "/aboutwindow.xaml"
let private aboutWindowXAML = File.ReadAllText(XAMLFile)

/// Exception thrown when using TextResources.resx upon aboutWindow.ShowDialog(). Will have to look into it.
[<Literal>]
let private aboutText ="""
Toy music engraver. Meant merely as a demo for right now and under heavy development. The point is to see how much I can actually get done in F# for desktop development, as well as setting the groundwork for a more robust music suite later on.
"""

/// Generates the About Window
let getAboutWindow () =
    let aboutWindow = XamlReader.Parse(aboutWindowXAML) :?> Window
    aboutWindow.Icon <- DrawingUtils.loadImageFile(GlyphLocations.Clefs.trebleClefLocation)
    let textBoxAbout = aboutWindow.FindName("textBoxAbout") :?> TextBox
    textBoxAbout.Text <- aboutText
    //return
    aboutWindow