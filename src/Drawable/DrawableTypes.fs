module DrawableTypes

open MusicTypes
open EventID

/// Used for both stem direction as well as slur direction
type Orientation = UP | DOWN

/// Holds geometry data for drawble objects
type MusGeom =
    { x: float
      y: float
      w: float
      h: float
      mutable orientation: Orientation }

/// Couples Geometry type with MeasureEvent type.
type DrawableEvent =
    { event: MeasureEvent
      geom: MusGeom }

/// Measure that can be drawn at a Geometry coordinate.
type DrawableMeasure =
    { dEvents: DrawableEvent list
      geom: MusGeom }

/// DrawableStaff is a record of DrawableMeasure list and MusGeom
type DrawableStaff =
    { measures: DrawableMeasure list
      geom: MusGeom }

//default sizes
let defaultGeomTuple = (0., 0., 0., 0.)
let defaultGeom = {x=0.;y=0.;w=0.;h=0.;orientation=UP}
let defaultDrawableMeasure = {dEvents=[];geom=defaultGeom}
let defaultDrawableStaff = {measures=[];geom=defaultGeom}

(*
    These assignments are kept for easy refactoring later/decoupling now
*)
//pitches
let wholeNoteWidth, wholeNoteHeight = MusResources.wholeNoteheadWidthDefault, MusResources.wholeNoteheadHeightDefault
let halfNoteWidth, halfNoteHeight = MusResources.halfNoteheadWidthDefault, MusResources.halfNoteheadHeightDefault
let filledNoteWidth, filledNoteHeight = MusResources.filledNoteheadWidthDefault, MusResources.filledNoteheadHeightDefault
let ledgerLineWidth = MusResources.ledgerLineWidth
let pitchYSpacing = MusResources.pitchYSpacing
let measureLineSpacing = MusResources.measureLineSpacing
//rests
let eighthRestWidth, eighthRestHeight = MusResources.eighthRestWidthDefault, MusResources.eightRestHeightDefault
let quarterRestWidth, quarterRestHeight = MusResources.quarterRestWidthDefault, MusResources.quarterRestHeightDefault
let halfRestWidth, halfRestHeight = MusResources.halfRestWidthDefault, MusResources.halfRestHeightDefault
let wholeRestWidth, wholeRestHeight = MusResources.wholeRestWidthDefault, MusResources.wholeRestHeightDefault
//ties
let tieHeight = MusResources.tieHeightDefault

//clefs
let bassClefWidth, bassClefHeight = MusResources.bassClefWidthDefault, MusResources.bassClefHeightDefault
let trebleClefWidth, trebleClefHeight = MusResources.trebleClefWidthDefault, MusResources.trebleClefHeightDefault
let trebleClefYOffset = MusResources.trebleClefYOffset

//alterations/accidentals
let flatWidth, flatHeight = MusResources.flatWidthDefault, MusResources.flatHeightDefault
let sharpWidth, sharpHeight = MusResources.sharpWidthDefault, MusResources.sharpHeightDefault
let naturalWidth, naturalHeight = MusResources.naturalWidthDefault, MusResources.naturalHeightDefault

//timesig
let timeSigWidth, timeSigHeight = MusResources.timeSigWidthDefault, MusResources.timeSigHeightDefault
//kerning
let kerning = MusResources.kerning

/// Helper func to create Geometry from x, y, w, h
let createGeom x y w h orientation : MusGeom = {x=x;y=y;w=w;h=h;orientation=orientation}

/// Wraps event into a DrawableEvent with a defaultGeom of {0;0;0;0}
let createDrawableEvent event = {event=event; geom=defaultGeom}

/// Creates single LedgerLineEvent at given location
let wrapLedgerLineIntoDrawable target x y w =
    let event = createLedgerLine target defaultEventID
    {event=event;geom=createGeom x y w 0. UP}

/// Creates multiple ledger lines and sets their MusGeoms.
let createDrawableLedgerLines (measure:DrawableMeasure) (p:DrawableEvent) pitchTop =
    let halfLineSpacing = measureLineSpacing / 2.
    let mTop = measure.geom.y
    let mBottom = mTop + measure.geom.h
    let x = p.geom.x - p.geom.w / 4.
    let w = ledgerLineWidth

    let calcLines delta max init =
        let numLines = delta / measureLineSpacing
        [1. .. numLines + max]
        |> List.map(fun lineNum -> init + (max * lineNum * measureLineSpacing))
        |> List.map(fun y -> wrapLedgerLineIntoDrawable p.event x y w)

    let result = 
        // Below staff
        if pitchTop > mBottom then
            let diff = pitchTop - halfLineSpacing - mBottom
            calcLines diff 1. mBottom
            //let numLines = diff / measureLineSpacing
            //[1. .. numLines + 1.]
            //|> List.map(fun lineNum -> mBottom + lineNum * measureLineSpacing)
            //|> List.map(fun y -> wrapLedgerLineIntoDrawable p.event x y w)
        //Above staff
        else if pitchTop < mTop then
            let diff = mTop + halfLineSpacing - pitchTop
            calcLines diff (-1.) mTop
            //let numLines = diff / measureLineSpacing
            //[1. .. numLines - 1.]
            //|> List.map(fun lineNum -> mTop - lineNum * measureLineSpacing)
            //|> List.map(fun y -> wrapLedgerLineIntoDrawable p.event x y w)
        //No ledger lines needed
        else
            []
    //return
    result