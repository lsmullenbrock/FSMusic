[<RequireQualifiedAccess>]
module DrawUtils

open System
open System.IO
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Media.Imaging

/// Loads image from file if it exists or the url otherwise
let inline loadImageFile file =
    //let path = Path.Combine(ImageLocations.imageLocDir, file)
    if File.Exists(file) then
        Basic.log "Loading image located at %A" file
        new BitmapImage(Uri(file, UriKind.Relative))
    else
        Basic.errMsg "Image located at '%A' was not found and could not be loaded" file
        Basic.errMsg "Returning empty BitmapImage"
        new BitmapImage()

/// Helper function to create a Line object.
let inline createLine x1 y1 x2 y2 =
    new Line(
        X1 = x1, 
        Y1 = y1, 
        X2 = x2, 
        Y2 = y2, 
        Stroke = Brushes.Black, 
        StrokeThickness = 2.
    )

/// Converts specified bitmap to an image
let inline convertFileToImage (bitmap:#BitmapSource) w h =
    new Image(
        Source = bitmap, 
        Stretch = Stretch.Fill, 
        Width = w, 
        Height = h
    )

/// Given a file location, will attempt to create an image.
let inline createImage file w h =
    convertFileToImage (loadImageFile file) w h