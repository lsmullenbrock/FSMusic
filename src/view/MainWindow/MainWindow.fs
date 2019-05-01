module MainWindow

open System.Windows

open Engraver
open MainWindowEvents

[<Literal>]
let private XAMLFile = "src/view/MainWindow/mainwindow.xaml"
type private MainWindow = FsXaml.XAML<XAMLFile>

let getMainWindow () = 
    let mainWindow = MainWindow()

    mainWindow.Icon <- DrawingUtils.loadImageFile(GlyphLocations.Accidentals.flatImageLocation)

    let engraver = Engraver(mainWindow.mainCanvas)

    mainWindow.buttonDrawDemo.Click.Add (clickEvent_testCanvas engraver)
    mainWindow.buttonClearCanvas.Click.Add (clickEvent_clear engraver)
    mainWindow.menuItemExit.Click.Add (clickEvent_close mainWindow)
    mainWindow.menuItemAbout.Click.Add (clickEvent_about(AboutWindow.getAboutWindow))

    //return
    mainWindow :> Window