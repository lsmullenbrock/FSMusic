module MainWindow

open System.IO
open System.Windows
open System.Windows.Markup
open System.Windows.Controls
open System.Windows.Media

open Engraver
open MainWindowEvents

let XAMLFile = __SOURCE_DIRECTORY__ + "/mainwindow.xaml"
let mainWindowXAML = File.ReadAllText(XAMLFile)

let mainWindow = XamlReader.Parse(mainWindowXAML) :?> Window

//why can't i do this?
//let inline findControl controlName castToType =
//    mainWindow.FindName(controlName) :?> castToType

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
menuItemAbout.Click.Add (clickEvent_help)

