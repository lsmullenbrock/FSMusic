module MainWindow

open System.IO
open System.Windows
open System.Windows.Markup
open System.Windows.Controls

open Engraver
open MainWindowEvents

[<Literal>]
let private XAMLFile = __SOURCE_DIRECTORY__ + "/mainwindow.xaml"
let private mainWindowXAML = File.ReadAllText(XAMLFile)

// Find way to implement the following:
//let inline findControl controlName castToType =
//    mainWindow.FindName(controlName) :?> castToType

let getMainWindow () = 
    let mainWindow = XamlReader.Parse(mainWindowXAML) :?> Window

    mainWindow.Icon <- DrawingUtils.loadImageFile(GlyphLocations.Accidentals.flatImageLocation)

    let mainCanvas = mainWindow.FindName("mainCanvas") :?> Canvas

    let engraver = Engraver(mainCanvas)

    let buttonDrawDemo = mainWindow.FindName("buttonDrawDemo") :?> Button
    buttonDrawDemo.Click.Add (clickEvent_testCanvas engraver)

    let buttonClearCanvas = mainWindow.FindName("buttonClearCanvas") :?> Button
    buttonClearCanvas.Click.Add (clickEvent_clear engraver)

    let menuItemExit = mainWindow.FindName("menuItemExit") :?> MenuItem
    menuItemExit.Click.Add (clickEvent_close mainWindow)

    let menuItemAbout = mainWindow.FindName("menuItemAbout") :?> MenuItem
    menuItemAbout.Click.Add (clickEvent_about(AboutWindow.getAboutWindow))

    //return
    mainWindow