module Inker

open System.Windows
open System.Windows.Shapes
open System.Windows.Controls
open Drawable

/// Class which only draws what it is told to draw.
type Inker(canvas:Canvas) =

    /// Direct clear of canvas
    member __.clearCanvas () =
        canvas.Children.Clear()

    /// Adds a UIElement to given Canvas.
    member inline private __.addElementToCanvas (elem:#UIElement) =
        canvas.Children.Add(elem) 
        |> ignore
    
    /// Add a TextBlock to canvas at given location.
    /// @TODO does not work properly, troubleshoot offsetting
    member this.writeTextToCanvas text x y w h =
        let textBlock = new TextBlock(Text=text, FontSize=h)
        //textBlock.Height <- h
        //textBlock.Width <- w
        Canvas.SetLeft(textBlock, x)
        Canvas.SetTop(textBlock, y)
        this.addElementToCanvas textBlock

    /// Simply adds given Image to given Canvas
    member private this.addImage (image:#Image) x y =
        this.addElementToCanvas image 
        |> ignore
        Canvas.SetLeft(image, x)
        Canvas.SetTop(image, y)

    /// Given a file location, attempt to draw it to a canvas.
    member private this.drawImageFromLocation file x y w h =
        let image = createImage file w h
        this.addImage image x y

    /// Creates a line that spans a specific width and height.
    member private this.createLineWidthHeight x y w h =
        createLine x y (x + w) (y + h)

    /// Adds a single Line object to given Canvas.
    member inline private this.inkLine (line:Line) = 
        this.addElementToCanvas line

    /// Adds multiple Line objects to given Canvas.
    member inline private this.inkLines (lines:Line list) =
        lines 
        |> List.map this.inkLine
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
    member this.inkStaffLines x y w h =
        (this.createStaffLines x y w h)
        |> this.inkLines

    /// Draws barline on given Canvas at given coords.
    member private this.inkBarline x y h =
        (this.createBarline x y h)
        |> this.inkLine

    /// Draws a full Measure on given Canvas at given coords.
    member this.inkMeasure x y w h =
        this.inkBarline x y h
        this.inkStaffLines x y w h
        this.inkBarline (x + w) y h

    /// Draws a Bass Clef.
    member this.inkBassClef x y w h = this.drawImageFromLocation ImageLocations.bassClefLocation x y w h
    /// Draws a Treble Clef.
    member this.inkTrebleClef x y w h = this.drawImageFromLocation ImageLocations.trebleClefLocation x y w h 
    /// Draws a Whole Note(head).
    member this.inkWholeNotehead x y w h = this.drawImageFromLocation ImageLocations.wholeNoteheadLocation x y w h
    /// Draws a Half Notehead (i.e., empty notehead) without a stem.
    member this.inkHalfNotehead x y w h = this.drawImageFromLocation ImageLocations.halfNoteheadLocation x y w h
    /// Draws a filled Notehead (for Quarter/Eighth/etc notes) without a stem.
    member this.inkFilledNotehead x y w h = this.drawImageFromLocation ImageLocations.filledNoteheadLocation x y w h

    /// Draws a note stem.
    member this.inkStem x y length =
        (this.createLineWidthHeight x y 0. length)
        |> this.inkLine

    /// Draws a stem with an offset.
    member this.inkStemDefaultOffsets x y h stemLength =
        this.inkStem (x + MusResources.stemXOffset) (y + h * MusResources.stemYOffsetMultiplier) stemLength

    /// Draws a filled Notehead with a stem (no beaming).
    member this.inkFilledNoteheadPitch x y w h stemLength =
        this.inkFilledNotehead x y w h
        this.inkStemDefaultOffsets x y h stemLength 
    /// Draws a half Notehead with a stem (i.e., a half note).
    member this.inkHalfNoteheadPitch x y w h stemLength =
        this.inkHalfNotehead x y w h
        this.inkStemDefaultOffsets x y h stemLength
    /// Explicit implementation meant specifically for whole notes; `drawWholeNotehead` is implicit for drawing the whole note glyph
    member this.drawWholeNoteheadPitch x y w h =
        this.inkWholeNotehead x y w h

    /// Draw a single ledger line.
    member this.inkLedgerLine x y w =
        this.createLineWidthHeight x y w 0.
        |> this.inkLine
    
    /// Draw an eighth rest
    member this.inkEighthRest x y w h =
        this.drawImageFromLocation ImageLocations.eighthRestImageLocation x y w h

    /// Draw a Quarter Rest
    member this.inkQuarterRest x y w h =
        this.drawImageFromLocation ImageLocations.quarterRestImageLocation x y w h 

    /// Draw a Half Rest
    member this.inkHalfRest x y w h = 
        this.drawImageFromLocation ImageLocations.halfRestImageLocation x y w h 

    /// Draw a Whole Rest
    member this.inkWholeRest  x y w h = 
        this.drawImageFromLocation ImageLocations.wholeRestImageLocation x y w h 

    /// Draw TimeSig event
    member this.inkTimeSig n d x y w h =
        this.writeTextToCanvas n x y w h
        this.writeTextToCanvas d x (y+h) w h