/// Eventually we need to add the concept of "precedence" with respect
/// to what we need to align first as DependentEvents rely on IndependentEvents
/// for their size, some of which may be in previous or later DrawableMeasures.
///
/// Will eventually set up a pipeline such that all IndependentEvents are sized and
/// aligned before DependentEvents in the entire staff.
///
/// Generate Materials -> process IndependentEvents -> process DependentEvents -> Draw results
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
//timesig
let timeSigWidth, timeSigHeight = MusResources.timeSigWidthDefault, MusResources.timeSigHeightDefault
//kerning
let kerning = MusResources.kerning

/// Helper func to create Geometry from x, y, w, h
let createGeom x y w h orientation : MusGeom = {x=x;y=y;w=w;h=h;orientation=orientation}

/// Wraps event into a DrawableEvent
let createDrawableEvent event = {event=event; geom=defaultGeom}


/// Creates single LedgerLineEvent at given location
let createDrawableLedgerLine target x y w =
    let event = createLedgerLine target 0
    {event=event;geom=createGeom x y w 0. UP}

/// Creates multiple ledger lines and sets their MusGeoms.
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
            [0. .. numLines + 1.]
            |> List.map(fun lineNum -> mBottom + lineNum * measureLineSpacing)
            |> List.map(fun y -> createDrawableLedgerLine p.event x y w)
        //Above staff
        else if pitchTop < mTop then
            let diff = mTop - pitchTop
            let numLines = diff / measureLineSpacing
            [1. .. numLines - 1.]
            |> List.map(fun lineNum -> mTop - lineNum * measureLineSpacing)
            |> List.map(fun y -> createDrawableLedgerLine p.event x y w)
        //No ledger lines needed
        else
            []
    //return
    result

(*
    IndependentEvent MusGeom assignment funcs
*)

let assignPitchWidthHeight (pitch:Pitch) =
    match pitch.value with
    | Value.Whole ->
        wholeNoteWidth, wholeNoteHeight
    | Value.Half ->
        halfNoteWidth, halfNoteHeight
    | _ ->
        filledNoteWidth, filledNoteHeight

let assignRestWidthHeight (rest:Rest) =
    match rest.value with
    | Value.Eighth ->
        eighthRestWidth, eighthRestHeight
    | Value.Quarter ->
        quarterRestWidth, quarterRestHeight
    | Value.Half ->
        halfRestWidth, halfRestHeight
    | Value.Whole ->
        wholeRestWidth, wholeRestHeight
    | _ ->
        Basic.log "%A unhandled by createDrawableMeasureEvent currently" rest
        0., 0.

let assignClefWidthHeight =
    function
    | Treble ->
        trebleClefWidth, trebleClefHeight
    | Bass ->
        bassClefWidth, bassClefHeight
    | NoClef ->
        Basic.errMsg "NoClef handed to setDEventSize"
        0., 0.

/// Implement
let assignKeyWidthHeight (k:Key) =
    Basic.log "%A unhandled by createDrawableMeasureEvent currently" k
    0., 0.

let assignErrorWidthHeight (e:Basic.MusError) =
    Basic.errMsg "Error encountered! Error message: %A" (e.ToString())
    0., 0.
/// Sets Width/Height properties of given IndependentEvent
let private setIndpEventWidthHeight (dEvent:DrawableEvent) =
    match dEvent.event.mEvent with
    | IndependentEvent i ->
        let w, h =
            match i with
            | PitchEvent p ->
                assignPitchWidthHeight p
            | RestEvent r ->
                assignRestWidthHeight r
            | ClefEvent c ->
                assignClefWidthHeight c
            | KeyEvent k ->
                assignKeyWidthHeight k
            | TimeSigEvent _ ->
                timeSigWidth, timeSigHeight
            | ErrorEvent e ->
                assignErrorWidthHeight e
        //return
        {dEvent with geom = {dEvent.geom with w=w; h=h}}
    | _ -> 
        // DependentEvent cases just need to be passed through
        dEvent

let private setMultipleIndpEventSizes (measure:DrawableMeasure) =
    measure.dEvents
    |> List.map setIndpEventWidthHeight
    |> fun result -> {measure with dEvents = result}
        
/// Attempts to assign x-coords to a DrawableEvent list
let private setIndpEventXCoords (measure:DrawableMeasure) =
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
            let result = {hd with geom = {hd.geom with x=newX}}
            xLoop (resultList@[result]) result.geom.x result.geom.w kerning tl 
        | _ ->
            resultList
    //return
    xLoop [] initialX 0. kerning measure.dEvents
    |> fun dList -> {measure with dEvents=dList}

/// Gets the Y-position of Middle C given a clef.
let private getMidCYCoord initY clef =
    match clef with
    | Treble -> //count UP from MidC
        initY + pitchYSpacing * 9.
    | Bass -> //count DOWN from MidC
        initY - pitchYSpacing * 3.
    | NoClef ->
        Basic.errMsg "Attempted to assign a Y coord to a Pitch with a None clef!"
        Basic.errMsg "Returning midpointY of measure as middle C coord"
        initY + pitchYSpacing * 6.

/// Gets the Y coord of a pitch
let private getPitchYCoords initY curClef pitch =
    let staffDist = float<|staffInterval defaultPitch pitch
    let dist = staffDist * pitchYSpacing
    let midCY = getMidCYCoord initY curClef
    //return
    midCY + dist

/// Spits out the Y-Coords of a rest given a measure's Y-midpoint.
let private getRestYCoords measureMidpointY (rest:Rest) =
    match rest.value with
    | Value.Half ->
        measureMidpointY + measureLineSpacing - halfRestHeight
    | Value.Eighth ->
        measureMidpointY + measureLineSpacing / 2.5
    | _ -> 
        measureMidpointY

/// Gets KeySig
let private getKeySig () = ()

/// Gets clef's Y-coords.
let private getClefYCoords initY clef =
    match clef with
    | Treble ->
        initY - trebleClefYOffset
    | Bass ->
        initY
    | _ ->
        Basic.errMsg "Uncovered clef hit in assignYCoords.yLoop: %A" clef
        initY

/// Gets TimeSig's Y-coords
let private getTimeSigYCoords initY =
    initY - 20.

/// Attempts to assign y-coords to a DrawableEvent list
let setIndpEventYCoords prevClef (measure:DrawableMeasure) =
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

    /// Calcs actual Y coord of IndependentEvent
    let calcIndpEventY initY (dEvent:DrawableEvent) indpEvent =
        match indpEvent with
        | PitchEvent p ->
            let pY = getPitchYCoords initY currentClef p
            //check if pitch is above or below midpoint
            if pY >= measureMidpointY then
                dEvent.geom.orientation <- UP
            else 
                dEvent.geom.orientation <- DOWN
            //assign ledger lines
            ledgerLines <- ledgerLines@(createLedgerLines measure dEvent pY)
            pY
        | RestEvent r ->
            getRestYCoords measureMidpointY r
        | KeyEvent _ ->
            Basic.errMsg "Need to implement KeyEvent assignYCoords in DrawMusic!"
            initY
        | ClefEvent c -> 
            checkClefUpdate c
            getClefYCoords initY c
        | TimeSigEvent _ ->
            getTimeSigYCoords initY
        | ErrorEvent e ->
            Basic.errMsg "ErrorEvent %A can't be assigned a location." e
            0.

    /// Only assign Y Coords to IndependentEvents
    let assignEvent initY dEvent =
        match dEvent.event.mEvent with
        | IndependentEvent i ->
            let y = calcIndpEventY initY dEvent i
            //return
            {dEvent with geom={dEvent.geom with y=y}}
        | _ ->
            //DependentEvents assigned later
            //return
            dEvent
    
    /// Assign multiple Y Coords to IndependentEvents
    let assignFold initY eventList =
        List.fold (fun acc elem -> acc@[assignEvent initY elem]) [] eventList

    //return
    assignFold initialY measure.dEvents
    |> fun dList -> {measure with dEvents = dList@ledgerLines} //ledger lines can be appended to the end because they're already assigned geometries

(*
    DependentEvent MusGeom funcs
*)
/// Returns a DrawableEvent given an EventID
let private findEventByID measure eID = 
    List.find (fun e -> e.event.eID = eID) measure.dEvents

/// Returns a MusGeom given an EventID
let private getEventGeomByID measure eID = 
    findEventByID measure eID
    |> (fun e -> e.geom)

let private calcTieGeom measure tie =
    let originGeom = getEventGeomByID measure tie.targets.[0].eID
    let targetGeom = getEventGeomByID measure tie.targets.[1].eID
    let x = originGeom.x + originGeom.w
    let y, orientation =  // needs to face opposite way of stem
        match originGeom.orientation with
        | UP -> 
            originGeom.y + originGeom.h, 
            DOWN
        | DOWN -> 
            originGeom.y - originGeom.h / 2.,
            UP
    let w = targetGeom.x - x
    let h = tieHeight
    //return
    (createGeom x y w h orientation)

let private calcSlurGeom measure slur =
    let drawableTargets =
        slur.targets
        |> List.map(fun t -> t.eID)
        |> List.sort //just in case
        |> List.map(fun e -> findEventByID measure e)

    //individually figure out each coord
    //lowest eID will be the leftmost and thus have the correct
    //starting coords
    let x, y =
        drawableTargets
        |> List.sortBy(fun t -> t.geom.x)
        |> List.head
        |> fun t -> t.geom.x, t.geom.y


    //width will be the rightmost, and thus the highest x minus the lowest x
    let w =
        drawableTargets
        |> List.maxBy(fun t -> t.geom.x)
        |> fun t -> t.geom.x - x
    
    //we will have to rotate the slur image to actually get the correct
    //alignment
    //height will be the heighest y minus the starting y
    //negative y indicates DOWN orientation
    let h =
        let highest =
            drawableTargets
            |> List.maxBy(fun t -> t.geom.y + t.geom.h)
            |> fun t -> t.geom.y + t.geom.h
        (highest - y)

    let orientation = UP
    //return
    (createGeom x y w h orientation)
    
/// All DependentEvent coords can be set at once because we already know all of it.
let private setDependentEventGeom (measure:DrawableMeasure) (dEvent:DrawableEvent) =
    match dEvent.event.mEvent with
    | DependentEvent d ->
        let result:DrawableEvent =
            match d.dType with
            | LedgerLine -> 
                //already assigned a geometry
                dEvent
            | Slur ->
                let geom = calcSlurGeom measure d
                {dEvent with geom = geom}
            | Tie ->
                let geom = calcTieGeom measure d
                {dEvent with geom = geom}
            //| _ ->
            //    Basic.errMsg "DependentEvent %A could not be assigned a MusGeom" d
            //    dEvent
        //return
        result
    | _ ->
        // IndependentEvent cases already taken care of (!)
        dEvent

/// Sets geoms for DependentEvents
let private setMultipleDependentEventGeoms measure =
    measure.dEvents
    |> List.map (setDependentEventGeom measure)
    |> fun result -> {measure with dEvents = result}

/// Sets geometries for IndependentEvents
let private setIndpEventGeoms clef =
    setMultipleIndpEventSizes
    >> setIndpEventXCoords
    >> (setIndpEventYCoords clef)

/// Aligns events according to where they fall in the measure.
let assignGeometries clef =
    (setIndpEventGeoms clef)
    >> setMultipleDependentEventGeoms

/// Creates DrawableMeasure out of given Measure.
let createDrawableMeasure initialClef (measure:Measure) x y w h =
    let dEvents = measure |> List.map createDrawableEvent
    let resultMeasure = {dEvents=dEvents; geom=createGeom x y w h UP} |> (assignGeometries initialClef)
    //test to see if the measure is wide enough or not
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

/// Ensures given Staff is not empty or is malformed, etc.
let verifyStaff (staff:Staff) = 
        let check = 
            if staff.IsEmpty  then
                Basic.errMsg "Empty staff given for createDrawableStaff"
                false
            else if staff.Head.IsEmpty then
                Basic.errMsg "First measure is empty in staff given to createDrawableStaff"
                false
            else
                true
        //return
        check

/// Helper func to create DrawableStaff
let private createVerifiedStaff (staff:Staff) x y w h : DrawableMeasure list =
        let mutable currentClef:Clef =
            staff
            |> List.head
            |> tryFindFirstClef
            |> function
                | Some c ->
                    c
                | _ ->
                    Basic.errMsg "No first clef found, assuming Treble"
                    Treble
        //return
        [
            for measure in staff do
                let dMeasure = createDrawableMeasure currentClef measure x y (w / (staff.Length|>float)) h
                match (tryFindPrevClef measure) with
                | Some c -> 
                    currentClef <- c
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
    {measures=measures;geom=createGeom x y w h UP}
    