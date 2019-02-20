module Drawable

open MusicBase

/// Holds geometry data for drawble objects
type MusGeom =
    { x: float
      y: float
      w: float
      h: float }
/// Couples Geometry type with MeasureEvent type.
type DrawableEvent =
    { event: MeasureEvent
      geom: MusGeom }
/// Measure that can be drawn at a Geometry coordinate.
type DrawableMeasure =
    { dEvents: DrawableEvent list
      geom: MusGeom }

//default sizes
let private defaultGeom = {x=0.;y=0.;w=0.;h=0.}
//pitches
let wholeNoteWidth, wholeNoteHeight = MusResources.wholeNoteheadWidthDefault, MusResources.wholeNoteheadHeightDefault
let halfNoteWidth, halfNoteHeight = MusResources.halfNoteheadWidthDefault, MusResources.halfNoteheadHeightDefault
let filledNoteWidth, filledNoteHeight = MusResources.filledNoteheadWidthDefault, MusResources.filledNoteheadHeightDefault
let ledgerLineWidth = MusResources.ledgerLineWidth
let pitchYSpacing = MusResources.pitchYSpacing
let measureLineSpacing = MusResources.measureLineSpacing
//clefs
let bassClefWidth, bassClefHeight = MusResources.bassClefWidthDefault, MusResources.bassClefHeightDefault
let trebleClefWidth, trebleClefHeight = MusResources.trebleClefWidthDefault, MusResources.trebleClefHeightDefault
let trebleClefYOffset = MusResources.trebleClefYOffset
//timesig
let timeSigWidth, timeSigHeight = MusResources.timeSigWidthDefault, MusResources.timeSigHeightDefault
//kerning
let kerning = MusResources.kerning

/// Helper func to create Geometry from x, y, w, h
let createGeom x y w h : MusGeom = {x=x;y=y;w=w;h=h}

/// Wraps event into a DrawableEvent
let createDrawableEvent event = {event=event; geom=defaultGeom}

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
        | ErrorEvent e ->
            Basic.errMsg "Error encountered! Error message: %A" (e.ToString())
            0., 0.
    //return
    {dEvent with geom={x=0.;y=0.;w=w;h=h}}

/// Maps setDEventSize over a list
let private setAllDEventSizes measure =
    measure.dEvents 
    |> List.map setDEventSize
    |> fun dList -> {measure with dEvents=dList}

/// Attempts to assign x-coords to a DrawableEvent list
let assignXCoords measure =
    let initialX = measure.geom.x
    let kerning = MusResources.kerning //@DECOUPLE
    let rec xLoop resultList prevXPos prevWidth kerning (eventList:DrawableEvent list) =
        match eventList with
        | hd::tl ->
            let currentX = prevXPos + prevWidth + kerning
            let currentGeom = {hd.geom with x = currentX}
            let result = {hd with geom = currentGeom}
            xLoop (resultList@[result]) currentX hd.geom.w kerning tl 
        | _ ->
            resultList
    //return
    xLoop [] initialX 0. kerning measure.dEvents
    |> fun dList -> {measure with dEvents=dList}

/// Attempts to assign y-coords to a DrawableEvent list
let assignYCoords measure =
    let initialY = measure.geom.y
    //let measureWidth = measure.geom.x
    let measureMidpointY = (measure.geom.y + measure.geom.h) / 2.
    let mutable currentClef = Clef.NoClef
    let mutable ledgerLines : DrawableEvent list = []

    /// Checks if clef should be updated
    let checkClefUpdate c =
        if c <> currentClef then 
            currentClef <- c

    /// Used to calculate value interval in staff lines/spaces between two pitches.
    /// Negative distance means p1 is below p2.
    let inline staffInterval p1 p2 =
        let p1Position = (ordinal p1.note) + p1.octave * 7 + 1
        let p2Position = (ordinal p2.note) + p2.octave * 7 + 1
        let dist = p1Position - p2Position
        dist

    /// Gets the Y coord of a pitch
    let getPitchYCoords initY curClef pitch =
        let staffDist = float<|staffInterval defaultPitch pitch
        printfn "staffDist=%A" staffDist
        //printfn "pitch = %A\n defaultPitch = %A\n staffDist=%A" pitch defaultPitch staffDist
        let dist = staffDist * pitchYSpacing
        printfn "dist=%A" dist
        match curClef with
        | Treble -> //count UP from MidC
            let midCY = initY + pitchYSpacing * 9.
            midCY + dist
        | Bass -> //count DOWN from MidC
            let midCY = initY - pitchYSpacing * 3.
            midCY + dist
        | NoClef ->
            Basic.errMsg "Attempted to assign a Y coord to a Pitch with a None clef!"
            initY
    
    (*
    from MEvent.kt

    if (y > parent!!.bottomY + h / 2) { // below the staff
        var currentY = parent!!.bottomY
        while (currentY < y - h / 2) {
            currentY += SizeConst.measureSpaceYIncrement
            temp.add(currentY)
        }
    } else if (y < parent!!.y) { // above the staff
        var currentY = parent!!.y
        while (currentY > y + h) {
            currentY -= SizeConst.measureSpaceYIncrement
            temp.add(currentY)
        }
    }
    *)

    let createDrawableLedgerLine x y w =
        {event=LedgerLineEvent;geom=createGeom x y w 0.}

    /// Creates ledger line.
    let createLedgerLines measure (p:DrawableEvent) pitchTop =
        let mutable temp:DrawableEvent list = []
        let mTop = measure.geom.y
        let mBottom = mTop + measure.geom.h
        let x = p.geom.x - p.geom.w / 4.
        let w = ledgerLineWidth
        // Below staff
        if pitchTop > mBottom then
            let mutable currentY = mBottom
            while currentY < pitchTop + measureLineSpacing do
                temp <- temp@[createDrawableLedgerLine x currentY w]
                currentY <- currentY + measureLineSpacing
        //above staff
        else if pitchTop < mTop then
            let mutable currentY = mTop
            while currentY > pitchTop do
                temp <- temp@[createDrawableLedgerLine x currentY w]
                currentY <- currentY - measureLineSpacing
        //return
        temp
                
    /// Assigns y Coords to DEvents
    let rec yLoop resultList initY (eventList:DrawableEvent list) =
        match eventList with
        | hd::tl ->
            let y =
                match hd.event with
                | PitchEvent p ->
                    let pY = getPitchYCoords initY currentClef p
                    //assign ledger lines
                    ledgerLines <- ledgerLines@(createLedgerLines measure hd pY)
                    pY
                | LedgerLineEvent ->
                    hd.geom.y
                | RestEvent r ->
                    Basic.errMsg "Need to implement RestEvent assignYCoords in DrawMusic!"
                    measureMidpointY
                | KeyEvent k ->
                    Basic.errMsg "Need to implement KeyEvent assignYCoords in DrawMusic!"
                    initY
                | ClefEvent c -> 
                    checkClefUpdate c
                    match c with
                    | Treble ->
                        initY - trebleClefYOffset
                    | _ ->
                        initY
                | TimeSigEvent _ ->
                    Basic.errMsg "Need to implement TimeSigEvent assignYCoords in DrawMusic!"
                    initY - 20.
                | ErrorEvent e ->
                    Basic.errMsg "ErrorEvent %A can't be assigned a location." e
                    0.
            let geom = {hd.geom with y = y}
            let result = {hd with geom = geom}
            yLoop (resultList@[result]) initY tl
        | _ ->
            resultList
    //return
    yLoop [] initialY measure.dEvents
    |> fun dList -> {measure with dEvents = dList@ledgerLines}

/// Aligns events according to where they fall in the measure.
let assignGeometries =
    setAllDEventSizes
    >> assignXCoords
    >> assignYCoords

/// Creates DrawableMeasure out of given Measure.
let createDrawableMeasure measure x y w h =
    let dEvents = measure.events |> List.map createDrawableEvent
    let resultMeasure = {dEvents=dEvents; geom={x=x;y=y;w=w;h=h}} |> assignGeometries
    let finalX = 
        List.last resultMeasure.dEvents
        |> fun e -> e.geom.x + e.geom.w
    if resultMeasure.geom.x + resultMeasure.geom.w <= finalX then
        let newWidth = finalX - resultMeasure.geom.x + kerning
        {resultMeasure with geom={resultMeasure.geom with w=newWidth}}
    else
        resultMeasure