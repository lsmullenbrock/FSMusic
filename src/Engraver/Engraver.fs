module Engraver

open System.Windows.Controls

open MusicTypes
open EventTypes
open DrawableTypes
open Inker



/// Engraver makes decisions and tells the Inker what to draw.
type Engraver(canvas:Canvas) =
    static let unpackGeom geom = geom.x, geom.y, geom.w, geom.h

    /// Engraver tells Inker what to do
    member private __.inker = new Inker(canvas)

    // May need adjustment
    member private __.stemLength = MusResources.stemLengthDefault
    member private __.dotSize = MusResources.dotSizeDefault

    /// Calls inker.clearCanvas()
    member this.clearCanvas () = this.inker.clearCanvas()

    /// Draws given pitch. Does not handle beaming.
    member private this.engravePitchEvent geometry (pitch:Pitch) = 
        let x, y, w, h = unpackGeom geometry
        match pitch.value with
        | Value.Whole ->
            this.inker.inkWholeNotehead x y w h
        | Value.Half ->
            this.inker.inkHalfNoteheadPitch x y w h this.stemLength
        | _ ->
            let stemY = y + h / 2.
            let widthOffset = w * 0.05
            match geometry.orientation with
            | UP -> 
                this.inker.inkStem (x + w - widthOffset) (stemY - this.stemLength) this.stemLength
            | DOWN ->
                this.inker.inkStem (x + widthOffset) stemY this.stemLength
            this.inker.inkFilledNoteheadPitch x y w h 
        match pitch.dotted with
        | true -> 
            this.inker.inkDot x y w this.dotSize
        | _ ->
            ()

    /// Draw given rest. @TODO: Implement. Add handling for 8th and shorter rests.
    member private this.engraveRestEvent geometry (rest:Rest) =
        let {x=x;y=y;w=w;h=h} = geometry
        match rest.value with
        | Value.Whole ->
            this.inker.inkWholeRest x y w h
        | Value.Half ->
            this.inker.inkHalfRest x y w h
        | Value.Quarter ->
            this.inker.inkQuarterRest x y w h
        | Value.Eighth ->
            this.inker.inkEighthRest x y w h
        | _ ->
            errMsg "engraveRestEvent does not currenlty handle %A Value rests! :(" rest.value
            ()
        match rest.dotted with
        | true ->
            this.inker.inkDot x (y + h / 4.) w this.dotSize
        | _ ->
            ()

    /// Engraves TimeSig at given location
    member private this.engraveTimeSigEvent geometry timeSig = 
        //let {x=x;y=y;w=w;h=h} = geom
        let x = geometry.x
        let w = geometry.w
        let h = geometry.h
        let y = geometry.y
        this.inker.inkTimeSig (timeSig.numerator) (timeSig.denominator|>int) x y w h

    ///@TODO: Implement
    member private this.engraveKeyEvent geometry (key:Key) = 
        let numAccidentals = getNumAccidentals key
        let accidentalType = getKeyAccidentalType key

        let buildAccidentals numAccs initY initX clef =
            let fSharpPosition =
                match clef with
                | Treble -> initY
                | Bass -> initY + MusResources.filledNoteheadHeightDefault
                | _ ->
                    errMsg "clef: %A hit in engraveKeyEvent.buildAccidentals()" clef
                    0.
            ()

        ()

    /// Engraves a given clef at a given location
    member private this.engraveClefEvent geometry clef =
        let x, y, w, h = unpackGeom geometry
        match clef with
        | Treble ->
            this.inker.inkTrebleClef x y w h
        | Bass ->
            this.inker.inkBassClef x y w h
        | NoClef ->
            errMsg "Engraver.engraveClefEvent hit NoClef match case given event: %A" clef
    /// Engraves a single ledger line at a givent location
    member private this.engraveLedgerLine geometry =
        let {x=x;y=y;w=w} = geometry
        this.inker.inkLedgerLine x y w 

    /// Engraves a tie
    member private this.engraveTie geometry =
        let x, y, w, h = unpackGeom geometry
        match geometry.orientation with
        | UP ->
            this.inker.inkUpTie x y w h
        | DOWN ->
            this.inker.inkDownTie x y w h

    /// Engraves a slur.
    member private this.engraveSlur geometry =
        //need to transform 
        let x, y, w, h = unpackGeom geometry
        match geometry.orientation with
        | UP ->
            this.inker.inkUpSlur x y w h
        | DOWN ->
            this.inker.inkDownSlur x y w h

    /// Engrave an accidental.
    member private this.engraveAlteration alt geometry =
        let x, y, w, h = unpackGeom geometry
        match alt with
        | Alteration.Flat ->
            this.inker.inkFlat x y w h
        | Alteration.Natural ->
            this.inker.inkNatural x y w h
        | Alteration.Sharp ->
            this.inker.inkSharp x y w h
        | _ ->
            errMsg "Unhandled Alteration: %A in Engraver.engraveAlteration" alt

    /// Engrave IndependentEvent at given MusGeom
    member private this.engraveIndpEvent (indpEvent:IndependentEvent) (g:MusGeom) =
        match indpEvent with
        | PitchEvent p ->
            this.engravePitchEvent g p
        | RestEvent r ->
            this.engraveRestEvent g r
        | KeyEvent k ->
            this.engraveKeyEvent g k
        | ClefEvent c ->
            this.engraveClefEvent g c
        | TimeSigEvent t ->
            this.engraveTimeSigEvent g t
        | ErrorEvent e ->
            errMsg "Attempted to draw ErrorEvent: %A" e

    member private this.engraveDepEvent (depEvent:DependentEvent) (g:MusGeom) =
        match depEvent.dType with
        | LedgerLine ->
            this.engraveLedgerLine g
        | Tie ->
            this.engraveTie g
        | Slur ->
            this.engraveSlur g
        | Accidental a ->
            this.engraveAlteration a g

    /// (Attempts to) draw correct event.
    member private this.engraveEvent (dEvent:DrawableEvent) =
        let g = dEvent.geom
        match dEvent.event.mEvent with
        | IndependentEvent i ->
            this.engraveIndpEvent i g
        | DependentEvent d ->
            this.engraveDepEvent d g

    /// Draws given DrawableMeasure to given canvas, but does not draw any events.
    member this.engraveMeasure (dMeasure:DrawableMeasure) =
        let x, y, w, h = unpackGeom dMeasure.geom
        this.inker.inkMeasure x y w h

    /// Draws a DrawableMeasure and its contents to a given Canvas.
    member this.engraveMeasureAndEvents (dMeasure:DrawableMeasure) =
        this.engraveMeasure dMeasure
        List.iter this.engraveEvent dMeasure.dEvents

    /// Draws a DrawableStaff and its contents.
    member this.engraveStaff (dStaff:DrawableStaff) =
        dStaff.measures
        |> List.iter this.engraveMeasureAndEvents

