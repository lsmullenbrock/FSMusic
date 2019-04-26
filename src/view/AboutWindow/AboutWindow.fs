module AboutWindow

open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

let XAMLFile = __SOURCE_DIRECTORY__ + "/aboutwindow.xaml"
let mainWindowXAML = File.ReadAllText(XAMLFile)

let mainWindow = XamlReader.Parse(mainWindowXAML) :?> Window