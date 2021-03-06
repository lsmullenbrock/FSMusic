﻿[<RequireQualifiedAccess>]
module DrawingUtils

open System
open System.IO
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Media.Imaging

/// Loads image from file if it exists or the url otherwise
let loadImageFile file =
    if File.Exists(file) then
        log "Loading image located at %A" file
        BitmapImage(Uri(file, UriKind.Absolute))        
    else
        errMsg "Image located at '%A' was not found and could not be loaded" file
        errMsg "Returning empty BitmapImage"
        new BitmapImage()

/// Helper function to create a Line object.
let createLine x1 y1 x2 y2 =
    new Line(
        X1 = x1,
        Y1 = y1,
        X2 = x2,
        Y2 = y2,
        Stroke = Brushes.Black, 
        StrokeThickness = 2.
    )

/// Converts specified bitmap to an image
let convertFileToImage (bitmap:#BitmapSource) w h =
    new Image(
        Source = bitmap, 
        Stretch = Stretch.Fill, 
        Width = w, 
        Height = h
    )

/// Given a file location, will attempt to create an image.
let createImage file w h =
    convertFileToImage (loadImageFile file) w h