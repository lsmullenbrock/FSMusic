module DrawIndependentEvent

open MusicTypes
open EventTypes
open DrawableTypes

(*
    IndependentEvent MusGeom assignment funcs
*)
/// Assign Pitch MusGeom w/h
let assignPitchWidthHeight (pitch:Pitch) =
    match pitch.value with
    | Value.Whole ->
        wholeNoteWidth, wholeNoteHeight
    | Value.Half ->
        halfNoteWidth, halfNoteHeight
    | _ ->
        filledNoteWidth, filledNoteHeight

/// Assign Rest MusGeom w/h
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
        errMsg "%A unhandled by createDrawableMeasureEvent currently" rest
        0., 0.

/// Assign Clef MusGeom w/h
let assignClefWidthHeight =
    function
    | Treble ->
        trebleClefWidth, trebleClefHeight
    | Bass ->
        bassClefWidth, bassClefHeight
    | NoClef ->
        errMsg "NoClef handed to setDEventSize"
        0., 0.

/// Implement
let assignKeyWidthHeight (k:Key) =
    errMsg "%A unhandled by createDrawableMeasureEvent currently" k
    0., 0.

/// Default for ErrorEvents is 0.0/0.0 w/h
let assignErrorWidthHeight (e:MusError) =
    errMsg "ErrorEvent hit in DrawIndependentEvent.assignErrorWidthHeight -> Error message: %A" (e.ToString())
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

/// Takes a measure and sets sizes for IndependentEvents in a measure
let setMultipleIndpEventSizes (measure:DrawableMeasure) =
    measure.dEvents
    |> List.map setIndpEventWidthHeight
    |> fun result -> {measure with dEvents = result}

let private getAlterationWidth alt =
    match alt with
    | Alteration.Flat ->
        flatWidth
    | Alteration.Natural ->
        naturalWidth    
    | Alteration.Sharp ->
        sharpWidth
    | _ ->
        errMsg "Unknown alteration: %A in getAlterationWidth" alt
        0.

let private getAccidentalXBuffer (event:DrawableEvent) =
    match event.event.mEvent with
    | IndependentEvent i ->
        match i with
        | PitchEvent p ->
            match p.alteration with
            | Some a ->
                getAlterationWidth a
            | _ ->
                0.
        | _ ->
            0.
    | _ ->
        0.

/// Attempts to assign x-coords to a DrawableEvent list
let setIndpEventXCoords (measure:DrawableMeasure) =
    let initialX = measure.geom.x
    let rec xLoop (resultList:DrawableEvent list) prevXPos prevWidth kerning (eventList:DrawableEvent list) =
        match eventList with
        | hd::tl ->
            let newX = 
                prevXPos
                + prevWidth
                + getAccidentalXBuffer hd //add extra buffer if accidental present
                + kerning
            let result = {hd with geom = {hd.geom with x = newX}}
            xLoop (resultList@[result]) result.geom.x result.geom.w kerning tl 
        | _ ->
            resultList
    //return
    xLoop [] initialX 0. kerning measure.dEvents
    |> fun dEvents -> {measure with dEvents = dEvents}

/// Gets the Y-position of Middle C given a clef.
let private getMidCYCoord initY clef =
    match clef with
    | Treble -> //count UP from MidC
        initY + pitchYSpacing * 9.
    | Bass -> //count DOWN from MidC
        initY - pitchYSpacing * 3.
    | NoClef ->
        errMsg "Attempted to assign a Y coord to a Pitch with a None clef!"
        errMsg "Returning midpointY of measure as middle C coord"
        initY + pitchYSpacing * 6.

/// Gets the Y coord of a pitch
let private getPitchYCoords initY curClef pitch =
    let staffDist = float (staffInterval defaultPitch pitch)
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

/// Gets clef Y-coords.
let private getClefYCoords initY clef =
    match clef with
    | Treble ->
        initY - trebleClefYOffset
    | Bass ->
        initY
    | _ ->
        errMsg "Uncovered clef hit in assignYCoords.yLoop: %A" clef
        initY

/// Gets TimeSig's Y-coords
let private getTimeSigYCoords initY =
    initY

/// Attempts to assign y-coords to a DrawableEvent list
let setIndpEventYCoords prevClef (measure:DrawableMeasure) =
    let initialY = measure.geom.y
    //let measureWidth = measure.geom.x
    let measureMidpointY = (measure.geom.y + (measure.geom.h) / 2.)
    let mutable currentClef = prevClef
    let mutable newDependents : DrawableEvent list = []

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
            newDependents <- newDependents@(createDrawableLedgerLines measure dEvent pY)
            match p.alteration with
            | Some _ ->
                let event = 
                    extractAccidentalFromMeasureEvent dEvent.event
                    |> createDrawableEvent
                newDependents <- newDependents@[event]
            | _ -> 
                () //nothing to do
            //return
            pY
        | RestEvent r ->
            getRestYCoords measureMidpointY r
        | KeyEvent _ ->
            errMsg "Need to implement KeyEvent assignYCoords in DrawMusic!"
            initY
        | ClefEvent c ->
            checkClefUpdate c
            getClefYCoords initY c
        | TimeSigEvent _ ->
            getTimeSigYCoords initY
        | ErrorEvent e ->
            errMsg "ErrorEvent %A can't be assigned a location." e
            0.

    /// Only assign Y Coords to IndependentEvents
    let assignEvent initY dEvent =
        match dEvent.event.mEvent with
        | IndependentEvent i ->
            let y = calcIndpEventY initY dEvent i
            //return
            {dEvent with geom = {dEvent.geom with y = y}}
        | _ ->
            //DependentEvents assigned later
            //return
            dEvent
    
    /// Assign multiple Y Coords to IndependentEvents
    let assignFold initY eventList =
        List.fold (fun acc elem -> acc@[(assignEvent initY elem)]) [] eventList

    //return
    (assignFold initialY measure.dEvents)
    |> fun dEvents -> {measure with dEvents = dEvents@newDependents} //ledger lines can be appended to the end because they're already assigned geometries