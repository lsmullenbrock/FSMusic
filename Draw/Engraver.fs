module Engraver

open System.Windows.Controls

open MusicBase
open Drawable
open DrawBase

/// Draws given pitch. @TODO: Add handling for eighths/beaming/etc
let engravePitchEvent canvas (geometry:MusGeom) (pitch:Pitch) = 
    let {x=x;y=y;w=w;h=h} = geometry
    match pitch.value with
    | Value.Whole ->
        drawWholeNotehead canvas x y w h
    | Value.Half ->
        drawHalfNoteheadPitch canvas x y w h MusResources.stemLengthDefault
    | _ ->
        drawFilledNoteheadPitch canvas x y w h MusResources.stemLengthDefault

/// Draw given rest. @TODO: Implement. Add handling for 16th and shorter rests.
let engraveRestEvent (canvas:Canvas) geometry (rest:Rest) =
    Basic.errMsg "Need to implement drawing for this!"
    let {x=x;y=y;w=w;h=h} = geometry
    match rest.value with
    | Value.Whole ->
        ()
    | Value.Half ->
        ()
    | Value.Eighth ->
        ()
    | _ ->
        Basic.errMsg "drawRestEvent does not currenlty handle %A Value rests! :(" rest.value
        ()

let engraveTimeSigEvent canvas geometry timeSig = 
    let {x=x;y=y;w=w;h=h} = geometry
    drawTimeSig canvas (timeSig.numerator|>string) (timeSig.denominator|>int|>string) x y w h

///@TODO: Implement
let engraveKeyEvent canvas geometry keyEvent = ()

///@TODO: Implement
let engraveClefEvent canvas geometry clef =
    let {x=x;y=y;w=w;h=h} = geometry
    match clef with
    | Treble ->
        drawTrebleClef canvas x y w h
    | Bass ->
        drawBassClef canvas x y w h
    | NoClef ->
        Basic.errMsg "Attempted to draw a NoClef"

let engraveLedgerLine canvas geometry =
    let {x=x;y=y;w=w} = geometry
    drawLedgerLine canvas x y w 
    

/// Should be handed a Canvas to then generate an easier-to-use function. 
/// (Attempts to) draw correct event.
let engraveEventCata canvas (dEvent:DrawableEvent) : unit =
    let g = dEvent.geom
    match dEvent.event with
    | PitchEvent p ->
        engravePitchEvent canvas g p
    | LedgerLineEvent ->
        engraveLedgerLine canvas g
    | RestEvent r ->
        engraveRestEvent canvas g r
    | KeyEvent k ->
        engraveKeyEvent canvas g k
    | ClefEvent c ->
        engraveClefEvent canvas g c
    | TimeSigEvent t ->
        engraveTimeSigEvent canvas g t
    | ErrorEvent e ->
        Basic.errMsg "Attempted to draw ErrorEvent: %A" e

/// Draws given DrawableMeasure to given canvas, but does not draw any events.
let engraveMeasure canvas dMeasure =
    let {x=x;y=y;h=h;w=w} = dMeasure.geom
    drawMeasure canvas x y w h

/// Draws a DrawableMeasure and its contents to a given Canvas.
let engraveMeasureAndEvents canvas dMeasure =
    engraveMeasure canvas dMeasure
    List.map (engraveEventCata canvas) (dMeasure.dEvents)
    |> ignore