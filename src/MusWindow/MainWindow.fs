module MainWindow

open System.IO
open System.Windows
open System.Windows.Markup
open System.Windows.Controls

open Engraver
open MainWindowEvents

let XAMLFile = __SOURCE_DIRECTORY__ + "/mainwindow.xaml"
let mainWindowXAML = File.ReadAllText(XAMLFile)

let mainWindow = XamlReader.Parse(mainWindowXAML) :?> Window

//why can't i do this?
//let inline findControl controlName castToType =
//    mainWindow.FindName(controlName) :?> castToType

let mainCanvas = mainWindow.FindName("mainCanvas") :?> Canvas

let engraver = Engraver(mainCanvas)


let image = Fonter.getDigitImage 4

mainCanvas.Children.Add (image) |> ignore

let buttonDrawDemo = mainWindow.FindName("buttonDrawDemo") :?> Button
buttonDrawDemo.Click.Add (clickEvent_testCanvas engraver)

let buttonClearCanvas = mainWindow.FindName("buttonClearCanvas") :?> Button
buttonClearCanvas.Click.Add (clickEvent_clear engraver)

let buttonCallGC = mainWindow.FindName("buttonCallGC") :?> Button
buttonCallGC.Click.Add (clickEvent_GC)

let menuItemExit = mainWindow.FindName("menuItemExit") :?> MenuItem
menuItemExit.Click.Add (clickEvent_close mainWindow)

let menuItemAbout = mainWindow.FindName("menuItemAbout") :?> MenuItem
menuItemAbout.Click.Add (clickEvent_help)

