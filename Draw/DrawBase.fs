module DrawBase

open System
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Media.Imaging

/// Adds a UIElement to given Canvas.
let private addElementToCanvas (canvas:Canvas) (elem:#UIElement) =
    canvas.Children.Add(elem) |> ignore

//let private addElementsToCanvas (canvas:Canvas) (elems:#UIElement list) =
//    elems |> List.map (addElementToCanvas canvas)

let addTextToCanvas canvas text x y w h =
    let textBlock = new TextBlock(Text=text, FontSize=h)
    //textBlock.Height <- h
    //textBlock.Width <- w
    Canvas.SetLeft(textBlock, x)
    Canvas.SetTop(textBlock, y)
    addElementToCanvas canvas textBlock

/// Loads image from file if it exists or the url otherwise
let loadImageFile file =
    //let path = Path.Combine(ImageLocations.imageLocDir, file)
    if File.Exists(file) then
        Basic.log "Loading image located at %A" file
        new BitmapImage(Uri(file, UriKind.Relative))
    else
        Basic.errMsg "Image located at '%A' was not found and could not be loaded" file
        Basic.errMsg "Returning empty BitmapImage"
        new BitmapImage()

/// Converts specified bitmap to an image
let convertFileToImage (bitmap:#BitmapSource) w h =
    new Image(Source = bitmap, Stretch = Stretch.Fill, Width = w, Height = h)

/// Given a file location, will attempt to create an image.
let createImage file w h =
    convertFileToImage (loadImageFile file) w h

/// Simply adds given Image to given Canvas
let addImage (canvas:Canvas) (image:#Image) x y =
    addElementToCanvas canvas image 
    |> ignore
    Canvas.SetLeft(image, x)
    Canvas.SetTop(image, y)

/// Given a file location, attempt to draw it to a canvas.
let drawImageFromLocation file canvas x y w h =
    let image = createImage file w h
    addImage canvas image x y

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

/// Creates a line that spans a specific width and height.
let createLineWidthHeight x y w h =
    createLine x y (x + w) (y + h)
/// Adds a single Line object to given Canvas.
let addLine (canvas:Canvas) (line:Line) = 
    addElementToCanvas canvas line
/// Adds multiple Line objects to given Canvas.
let addLines (canvas:Canvas) (lines:Line list) =
    lines 
    |> List.map (addLine canvas)
    |> ignore
/// Creates a single horizontal Staff line.
let createStaffLine x y w =
    createLineWidthHeight x y w 0.
/// Creates all 5 Staff lines (without barlines).
let createStaffLines x y w h =
    let lineSpacer = MusResources.NUM_STAFF_LINES - 1.
    let increment = h / lineSpacer
    [0. .. lineSpacer]
    |> List.map (fun i -> createStaffLine x (y + increment * i) w)
/// Create single barline.
let createBarline x y h =
    createLineWidthHeight x y 0. h
/// Draws staff Line objects on given Canvas with coords and width/height.
let drawStaffLines canvas x y w h =
    (createStaffLines x y w h)
    |> addLines canvas
/// Draws barline on given Canvas at given coords.
let drawBarline canvas x y h =
    (createBarline x y h)
    |> addLine canvas
/// Draws a full Measure on given Canvas at given coords.
let drawMeasure canvas x y w h =
    drawBarline canvas x y h
    drawStaffLines canvas x y w h
    drawBarline canvas (x + w) y h

/// Draws a Bass Clef.
let drawBassClef canvas x y w h = drawImageFromLocation ImageLocations.bassClefLocation canvas x y w h
/// Draws a Treble Clef.
let drawTrebleClef canvas x y w h = drawImageFromLocation ImageLocations.trebleClefLocation canvas x y w h 
/// Draws a Whole Note(head).
let drawWholeNotehead canvas x y w h = drawImageFromLocation ImageLocations.wholeNoteheadLocation canvas x y w h
/// Draws a Half Notehead (i.e., empty notehead) without a stem.
let drawHalfNotehead canvas x y w h = drawImageFromLocation ImageLocations.halfNoteheadLocation canvas x y w h
/// Draws a filled Notehead (for Quarter/Eighth/etc notes) without a stem.
let drawFilledNotehead canvas x y w h = drawImageFromLocation ImageLocations.filledNoteheadLocation canvas x y w h

/// Draws a note stem.
let drawStem canvas x y length =
    (createLineWidthHeight x y 0. length)
    |> addLine canvas
/// Draws a stem with an offset.
let drawStemDefaultOffsets canvas x y h stemLength =
    drawStem canvas (x + MusResources.stemXOffset) (y + h * MusResources.stemYOffsetMultiplier) stemLength

/// Draws a filled Notehead with a stem (no beaming).
let drawFilledNoteheadPitch canvas x y w h stemLength =
    drawFilledNotehead canvas x y w h
    drawStemDefaultOffsets canvas x y h stemLength 
/// Draws a half Notehead with a stem (i.e., a half note).
let drawHalfNoteheadPitch canvas x y w h stemLength =
    drawHalfNotehead canvas x y w h
    drawStemDefaultOffsets canvas x y h stemLength
/// Explicit implementation meant specifically for whole notes; `drawWholeNotehead` is implicit for drawing the whole note glyph
let drawWholeNoteheadPitch canvas x y w h =
    drawWholeNotehead canvas x y w h

/// Draw a single ledge line.
let drawLedgerLine canvas x y w =
    createLineWidthHeight x y w 0.
    |> addLine canvas

/// Draw TimeSig event
let drawTimeSig canvas n d x y w h =
    addTextToCanvas canvas n x y w h
    addTextToCanvas canvas d x (y+h) w h