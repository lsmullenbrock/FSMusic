module Drawable

open MusicBase
/// Temporary fix
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
let defaultGeom = {x=0.;y=0.;w=0.;h=0.;orientation=UP}
let defaultDrawableMeasure = {dEvents=[];geom=defaultGeom}
let defaultDrawableStaff = {measures=[];geom=defaultGeom}

(*
    These assignments are kept in case later refactoring is needed for scaling/etc
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
//clefs
let bassClefWidth, bassClefHeight = MusResources.bassClefWidthDefault, MusResources.bassClefHeightDefault
let trebleClefWidth, trebleClefHeight = MusResources.trebleClefWidthDefault, MusResources.trebleClefHeightDefault
let trebleClefYOffset = MusResources.trebleClefYOffset
//timesig
let timeSigWidth, timeSigHeight = MusResources.timeSigWidthDefault, MusResources.timeSigHeightDefault
//kerning
let kerning = MusResources.kerning

/// Helper func to create Geometry from x, y, w, h
let createGeom x y w h : MusGeom = {x=x;y=y;w=w;h=h;orientation=UP}

/// Wraps event into a DrawableEvent
let createDrawableEvent event = {event=event; geom=defaultGeom}

/// Used to calculate value interval in staff lines and spaces between two pitches.
/// Negative distance means p1 is below p2 on the staff.
let inline staffInterval p1 p2 = (distFromC0 p1) - (distFromC0 p2)

/// Creates single LedgerLineEvent at given location
let createDrawableLedgerLine x y w =
    {event=LedgerLineEvent;geom=createGeom x y w 0.}

/// Creates multiple ledger lines.
let createLedgerLines (measure:DrawableMeasure) (p:DrawableEvent) pitchTop =
    let mTop = measure.geom.y
    let mBottom = mTop + measure.geom.h
    let x = p.geom.x - p.geom.w / 4.
    let w = ledgerLineWidth

    let result = 
        // Below staff
        if pitchTop > mBottom then
            let diff = pitchTop - mBottom
            let numLines = diff / measureLineSpacing
            [0. ..numLines + 1.]
            |> List.map(fun lineNum -> mBottom + lineNum * measureLineSpacing)
            |> List.map(fun y -> createDrawableLedgerLine x y w)
        //Above staff
        else if pitchTop < mTop then
            let diff = mTop - pitchTop
            let numLines = diff / measureLineSpacing
            [1. ..numLines]
            |> List.map(fun lineNum -> mTop - lineNum * measureLineSpacing)
            |> List.map(fun y -> createDrawableLedgerLine x y w)
        //No ledger lines needed
        else
            []
    //return
    result

/// Creates a DrawableMeasureEvent from a regular MeasureEvent
let private setDEventSize dEvent =
    let w, h =
        match dEvent.event with
        | PitchEvent p ->
            match p.value with
            | Value.Whole ->
                wholeNoteWidth, wholeNoteHeight
            | Value.Half ->
                halfNoteWidth, halfNoteHeight
            | _ ->
                filledNoteWidth, filledNoteHeight
        | LedgerLineEvent ->
            ledgerLineWidth, 0.
        | RestEvent r ->
            match r.value with
            | Value.Eighth ->
                eighthRestWidth, eighthRestHeight
            | Value.Quarter ->
                quarterRestWidth, quarterRestHeight
            | Value.Half ->
                halfRestWidth, halfRestHeight
            | Value.Whole ->
                wholeRestWidth, wholeRestHeight
            | _ ->
                Basic.log "%A unhandled by createDrawableMeasureEvent currently" r
                0., 0.
        | ClefEvent c ->
            match c with
            | Treble ->
                trebleClefWidth, trebleClefHeight
            | Bass ->
                bassClefWidth, bassClefHeight
            | NoClef ->
                Basic.errMsg "NoClef handed to setDEventSize"
                0., 0.
        | KeyEvent k ->
            Basic.log "%A unhandled by createDrawableMeasureEvent currently" k
            0., 0.
        | TimeSigEvent _ ->
            timeSigWidth, timeSigHeight
        | TieEvent _ ->
            Basic.errMsg "Cannot set w/h of TieEvent yet"
            0., 0.
        | ErrorEvent e ->
            Basic.errMsg "Error encountered! Error message: %A" (e.ToString())
            0., 0.
    //return
    {dEvent with geom = {dEvent.geom with w=w; h=h}}

/// Maps setDEventSize over a list
let private setAllDEventSizes measure =
    measure.dEvents 
    |> List.map setDEventSize
    |> fun dList -> {measure with dEvents=dList}

/// Attempts to assign x-coords to a DrawableEvent list
let assignEventXCoords (measure:DrawableMeasure) =
    let initialX = measure.geom.x
    let rec xLoop resultList prevXPos prevWidth kerning (eventList:DrawableEvent list) =
        match eventList with
        | hd::tl ->
            let newX = 
                // If the last event has no width, no adjustment/kerning needed
                if prevWidth = 0. then
                    prevXPos
                else 
                    prevXPos + prevWidth + kerning
            let result = {hd with geom={hd.geom with x=newX}}
            xLoop (resultList@[result]) result.geom.x result.geom.w kerning tl 
        | _ ->
            resultList
    //return
    xLoop [] initialX 0. kerning measure.dEvents
    |> fun dList -> {measure with dEvents=dList}

/// Attempts to assign y-coords to a DrawableEvent list
let assignEventYCoords prevClef (measure:DrawableMeasure) =
    let initialY = measure.geom.y
    //let measureWidth = measure.geom.x
    let measureMidpointY = (measure.geom.y + measure.geom.h) / 2.
    let mutable currentClef = prevClef
    let mutable ledgerLines : DrawableEvent list = []

    /// Checks if clef should be updated
    /// @Refactor ?
    let checkClefUpdate c =
        if c <> currentClef then 
            currentClef <- c

    /// Gets the Y coord of a pitch
    let getPitchYCoords initY curClef pitch =
        let staffDist = float<|staffInterval defaultPitch pitch
        let dist = staffDist * pitchYSpacing
        let midCY =
            match curClef with
            | Treble -> //count UP from MidC
                initY + pitchYSpacing * 9.
            | Bass -> //count DOWN from MidC
                initY - pitchYSpacing * 3.
            | NoClef ->
                Basic.errMsg "Attempted to assign a Y coord to a Pitch with a None clef!"
                Basic.errMsg "Returning midpointY of measure as middle C coord"
                initY + pitchYSpacing * 6.
        //return
        midCY + dist

    /// Assigns y Coords to DEvents
    let rec yLoop resultList initY (eventList:DrawableEvent list) =
        match eventList with
        | hd::tl ->
            let y =
                match hd.event with
                | PitchEvent p ->
                    let pY = getPitchYCoords initY currentClef p
                    //check if pitch is above or below midpoint
                    if pY >= measureMidpointY then
                        hd.geom.orientation <- UP
                    else 
                        hd.geom.orientation <- DOWN
                    //assign ledger lines
                    ledgerLines <- ledgerLines@(createLedgerLines measure hd pY)
                    pY
                | LedgerLineEvent ->
                    hd.geom.y
                | RestEvent r ->
                    match r.value with
                    | Value.Half ->
                        measureMidpointY + measureLineSpacing - halfRestHeight
                    | Value.Eighth ->
                        measureMidpointY + measureLineSpacing / 2.5
                    | _ -> 
                        measureMidpointY
                | KeyEvent _ ->
                    Basic.errMsg "Need to implement KeyEvent assignYCoords in DrawMusic!"
                    initY
                | ClefEvent c -> 
                    checkClefUpdate c
                    match c with
                    | Treble ->
                        initY - trebleClefYOffset
                    | Bass ->
                        initY
                    | _ ->
                        Basic.errMsg "Uncovered clef hit in assignYCoords.yLoop: %A" c
                        initY
                | TimeSigEvent _ ->
                    initY - 20.
                | TieEvent _ ->
                    Basic.errMsg "Need to implement TieEvent in assignYCoords"
                    0.
                | ErrorEvent e ->
                    Basic.errMsg "ErrorEvent %A can't be assigned a location." e
                    0.
            yLoop (resultList@[{hd with geom={hd.geom with y = y}}]) initY tl
        | _ ->
            resultList
    //return
    yLoop [] initialY measure.dEvents
    |> fun dList -> {measure with dEvents = dList@ledgerLines} //ledger lines can be appended to the end because they're already assigned geometries

/// Aligns events according to where they fall in the measure.
let assignGeometries clef =
    setAllDEventSizes
    >> assignEventXCoords
    >> (assignEventYCoords clef)

/// Creates DrawableMeasure out of given Measure.
let createDrawableMeasure initialClef (measure:Measure) x y w h =
    let dEvents = measure |> List.map createDrawableEvent
    let resultMeasure = {dEvents=dEvents; geom={x=x;y=y;w=w;h=h;orientation=UP}} |> (assignGeometries initialClef)
    let finalX = 
        List.last resultMeasure.dEvents
        |> fun e -> e.geom.x + e.geom.w
    if resultMeasure.geom.x + resultMeasure.geom.w <= finalX then
        let newWidth = finalX - resultMeasure.geom.x + kerning
        //return
        {resultMeasure with geom={resultMeasure.geom with w = newWidth}}
    else
        //return
        resultMeasure

/// Tries to find the lastmost clef in a measure, returns an option (Some cleff/None)
let getPrevClef (measure:Measure) = measure |> List.tryFindBack(isClef)

/// Ensures given Staff is not empty or is malformed, etc.
let verifyStaff (staff:Staff) = 
        if staff.Length = 0  then
            Basic.errMsg "No measures given for createDrawableStaff"
            false
        else if staff.Head.Length = 0 then
            Basic.errMsg "First measure is empty given to createDrawableStaff"
            false
        else if not (isClef staff.Head.Head) then
            Basic.errMsg "First item in Staff must be Clef"
            false
        else
            true

/// Helper func to create DrawableStaff
let private createVerifiedStaff (staff:Staff) x y w h : DrawableMeasure list =
        let mutable initialClef:Clef = staff.Head.Head |> unboxEvent
        //return
        [
            for measure in staff do
                let dMeasure = createDrawableMeasure initialClef measure x y (w / (staff.Length|>float)) h
                match (getPrevClef measure) with
                | Some c -> 
                    initialClef <- unboxEvent c
                | None ->
                    () //do nothing
                yield dMeasure
        ]

/// Creates DrawableStaff for a given Staff and assigns geometries 
let createDrawableStaff (staff:Staff) x y w h : DrawableStaff =
    let measures = 
        match verifyStaff staff with
        | true -> 
            createVerifiedStaff staff x y w h
        | _ -> 
            Basic.errMsg "verifyStaff failed for given staff: %A" staff
            []
    //return
    {measures=measures;geom=createGeom x y w h}
    