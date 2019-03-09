module Engraver

open System.Windows.Controls

open MusicBase
open Drawable
open Inker

/// Essentially a wrapper for the Inker, the Engraver makes decisions and tells the Inker what to draw.
type Engraver(canvas:Canvas) =
    /// Engraver tells Inker what to do
    member private __.inker = new Inker(canvas)

    // May need adjustment
    member private __.stemLength = MusResources.stemLengthDefault
    member private __.dotSize = MusResources.dotSizeDefault

    /// Calls inker.clearCanvas()
    member this.clearCanvas () = this.inker.clearCanvas()

    /// Draws given pitch. Does not handle beaming.
    member private this.engravePitchEvent (geometry:MusGeom) (pitch:Pitch) = 
        let {x=x;y=y;w=w;h=h} = geometry
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
            Basic.errMsg "engraveRestEvent does not currenlty handle %A Value rests! :(" rest.value
            ()
        match rest.dotted with
        | true ->
            this.inker.inkDot x (y + h / 4.) w this.dotSize
        | _ ->
            ()

    /// Engraves TimeSig at given location
    member private this.engraveTimeSigEvent geom timeSig = 
        let {x=x;y=y;w=w;h=h} = geom
        this.inker.inkTimeSig (timeSig.numerator|>string) (timeSig.denominator|>int|>string) x y w h

    ///@TODO: Implement
    member private this.engraveKeyEvent geometry keyEvent = 
        Basic.errMsg "Engraver.engraveKeyEvent is not currently implemented"
        ()

    /// Engraves a given clef at a given location
    member private this.engraveClefEvent geometry clef =
        let {x=x;y=y;w=w;h=h} = geometry
        match clef with
        | Treble ->
            this.inker.inkTrebleClef x y w h
        | Bass ->
            this.inker.inkBassClef x y w h
        | NoClef ->
            Basic.errMsg "Engraver.engraveClefEvent hit NoClef match case given event: %A" clef
    /// Engraves a single ledger line at a givent location
    member private this.engraveLedgerLine geometry =
        let {x=x;y=y;w=w} = geometry
        this.inker.inkLedgerLine x y w 

    /// Engraves a tie
    member private this.engraveTie geometry =
        let {x=x;y=y;w=w;h=h} = geometry
        match geometry.orientation with
        | UP ->
            this.inker.inkUpTie x y w h
        | DOWN ->
            this.inker.inkDownTie x y w h

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
            Basic.errMsg "Attempted to draw ErrorEvent: %A" e

    member private this.engraveDepEvent (depEvent:DependentEvent) (g:MusGeom) =
        match depEvent.dType with
        | LedgerLine ->
            this.engraveLedgerLine g
        | Tie ->
            this.engraveTie g
        | _ ->
            Basic.errMsg "DependentEvent %A could not be engraved" depEvent
            ()

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
        let {x=x;y=y;h=h;w=w} = dMeasure.geom
        this.inker.inkMeasure x y w h

    /// Draws a DrawableMeasure and its contents to a given Canvas.
    member this.engraveMeasureAndEvents (dMeasure:DrawableMeasure) =
        this.engraveMeasure dMeasure
        List.map this.engraveEvent dMeasure.dEvents
        |> ignore

