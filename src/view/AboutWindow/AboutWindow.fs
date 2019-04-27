module AboutWindow

open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

let XAMLFile = __SOURCE_DIRECTORY__ + "/aboutwindow.xaml"
let aboutWindowXAML = File.ReadAllText(XAMLFile)

let aboutWindow = XamlReader.Parse(aboutWindowXAML) :?> Window

aboutWindow.Icon <- DrawingUtils.loadImageFile(GlyphLocations.Clefs.trebleClefLocation)

let textBoxAbout = aboutWindow.FindName("textBoxAbout") :?> TextBox

/// Exception thrown when using TextResources.resx upon aboutWindow.ShowDialog(). Will have to look into it.
let aboutText ="""
Toy music engraver. Meant merely as a demo for right now and under heavy development. The point is to see how much I can actually get done in F# for desktop development, as well as setting the groundwork for a more robust music suite later on.
"""

textBoxAbout.Text <- aboutText