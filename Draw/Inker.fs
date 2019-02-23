module Inker

open System
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Media.Imaging

type Inker(canvas:Canvas) =
    /// Canvas to draw objects on.
    member private this.canvas = canvas

    /// Adds a UIElement to given Canvas.
    member this.addElementToCanvas (elem:#UIElement) =
        this.canvas.Children.Add(elem) |> ignore
    
    /// Add a TextBlock to canvas at given location.
    /// @TODO does not work properly, troubleshoot offsetting
    member this.addTextToCanvas text x y w h =
        let textBlock = new TextBlock(Text=text, FontSize=h)
        //textBlock.Height <- h
        //textBlock.Width <- w
        Canvas.SetLeft(textBlock, x)
        Canvas.SetTop(textBlock, y)
        this.addElementToCanvas textBlock

    /// Loads image from file if it exists or the url otherwise
    member private __.loadImageFile file =
        //let path = Path.Combine(ImageLocations.imageLocDir, file)
        if File.Exists(file) then
            Basic.log "Loading image located at %A" file
            new BitmapImage(Uri(file, UriKind.Relative))
        else
            Basic.errMsg "Image located at '%A' was not found and could not be loaded" file
            Basic.errMsg "Returning empty BitmapImage"
            new BitmapImage()

    /// Converts specified bitmap to an image
    member inline private __.convertFileToImage (bitmap:#BitmapSource) w h =
        new Image(
            Source = bitmap, 
            Stretch = Stretch.Fill, 
            Width = w, 
            Height = h
        )

    /// Given a file location, will attempt to create an image.
    member private this.createImage file w h =
        this.convertFileToImage (this.loadImageFile file) w h

    /// Simply adds given Image to given Canvas
    member private this.addImage (image:#Image) x y =
        this.addElementToCanvas image 
        |> ignore
        Canvas.SetLeft(image, x)
        Canvas.SetTop(image, y)

    /// Given a file location, attempt to draw it to a canvas.
    member private this.drawImageFromLocation file x y w h =
        let image = this.createImage file w h
        this.addImage image x y

    /// Helper function to create a Line object.
    member inline private __.createLine x1 y1 x2 y2 =
        new Line(
            X1 = x1, 
            Y1 = y1, 
            X2 = x2, 
            Y2 = y2, 
            Stroke = Brushes.Black, 
            StrokeThickness = 2.
        )

    /// Creates a line that spans a specific width and height.
    member private this.createLineWidthHeight x y w h =
        this.createLine x y (x + w) (y + h)
    /// Adds a single Line object to given Canvas.
    member private this.addLine (line:Line) = 
        this.addElementToCanvas line
    /// Adds multiple Line objects to given Canvas.
    member inline private this.addLines (lines:Line list) =
        lines 
        |> List.map this.addLine
        |> ignore
    /// Creates a single horizontal Staff line.
    member private this.createStaffLine x y w =
        this.createLineWidthHeight x y w 0.
    /// Creates all 5 Staff lines (without barlines).
    member this.createStaffLines x y w h =
        let lineSpacer = MusResources.NUM_STAFF_LINES - 1.
        let increment = h / lineSpacer
        [0. .. lineSpacer]
        |> List.map (fun i -> this.createStaffLine x (y + increment * i) w)
    /// Create single barline.
    member private this.createBarline x y h =
        this.createLineWidthHeight x y 0. h
    /// Draws staff Line objects on given Canvas with coords and width/height.
    member this.drawStaffLines x y w h =
        (this.createStaffLines x y w h)
        |> this.addLines
    /// Draws barline on given Canvas at given coords.
    member private this.drawBarline x y h =
        (this.createBarline x y h)
        |> this.addLine
    /// Draws a full Measure on given Canvas at given coords.
    member this.drawMeasure x y w h =
        this.drawBarline x y h
        this.drawStaffLines x y w h
        this.drawBarline (x + w) y h

    /// Draws a Bass Clef.
    member this.drawBassClef x y w h = this.drawImageFromLocation ImageLocations.bassClefLocation x y w h
    /// Draws a Treble Clef.
    member this.drawTrebleClef x y w h = this.drawImageFromLocation ImageLocations.trebleClefLocation x y w h 
    /// Draws a Whole Note(head).
    member this.drawWholeNotehead x y w h = this.drawImageFromLocation ImageLocations.wholeNoteheadLocation x y w h
    /// Draws a Half Notehead (i.e., empty notehead) without a stem.
    member this.drawHalfNotehead x y w h = this.drawImageFromLocation ImageLocations.halfNoteheadLocation x y w h
    /// Draws a filled Notehead (for Quarter/Eighth/etc notes) without a stem.
    member this.drawFilledNotehead x y w h = this.drawImageFromLocation ImageLocations.filledNoteheadLocation x y w h

    /// Draws a note stem.
    member this.drawStem x y length =
        (this.createLineWidthHeight x y 0. length)
        |> this.addLine

    /// Draws a stem with an offset.
    member this.drawStemDefaultOffsets x y h stemLength =
        this.drawStem (x + MusResources.stemXOffset) (y + h * MusResources.stemYOffsetMultiplier) stemLength

    /// Draws a filled Notehead with a stem (no beaming).
    member this.drawFilledNoteheadPitch x y w h stemLength =
        this.drawFilledNotehead x y w h
        this.drawStemDefaultOffsets x y h stemLength 
    /// Draws a half Notehead with a stem (i.e., a half note).
    member this.drawHalfNoteheadPitch x y w h stemLength =
        this.drawHalfNotehead x y w h
        this.drawStemDefaultOffsets x y h stemLength
    /// Explicit implementation meant specifically for whole notes; `drawWholeNotehead` is implicit for drawing the whole note glyph
    member this.drawWholeNoteheadPitch x y w h =
        this.drawWholeNotehead x y w h

    /// Draw a single ledger line.
    member this.drawLedgerLine x y w =
        this.createLineWidthHeight x y w 0.
        |> this.addLine

    /// Draw a Quarter Rest
    member this.drawQuarterRest x y w h = this.drawImageFromLocation ImageLocations.quarterRestImageLocation x y w h 

    /// Draw a Half Rest
    member this.drawHalfRest x y w h = this.drawImageFromLocation ImageLocations.halfRestImageLocation x y w h 

    /// Draw a Whole Rest
    member this.drawWholeRest  x y w h = this.drawImageFromLocation ImageLocations.wholeRestImageLocation x y w h 

    /// Draw TimeSig event
    member this.drawTimeSig n d x y w h =
        this.addTextToCanvas n x y w h
        this.addTextToCanvas d x (y+h) w h