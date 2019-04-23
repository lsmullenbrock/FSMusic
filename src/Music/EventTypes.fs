module EventTypes
open MusicTypes
open EventID

/// Wrapper for objects that appear in a measure
type IndependentEvent =
    | PitchEvent of Pitch
    | RestEvent of Rest
    | KeyEvent of Key
    | TimeSigEvent of TimeSig
    | ClefEvent of Clef
    | ErrorEvent of MusError

/// Types that are attached to objects that appear in a measure
type DependentEventType =
    | Tie
    | Slur
    | LedgerLine
    | Accidental of Alteration

/// Wrapper type so that Pitches, Rests, Clefs and so on can be packed into a Measure together.
///
/// The EventID here is mutable because it must be assigned later on.
type MeasureEvent =
    { mEvent: MEvent 
      eID: EventID }
and MEvent =
    | IndependentEvent of IndependentEvent
    | DependentEvent of DependentEvent
and DependentEvent =
    { dType: DependentEventType
      targets: MeasureEvent list }

/// Simple container type
type Measure = MeasureEvent list
/// Simple container type
type Staff = Measure list

/// This function is mandatory for generating new events!
/// @TODO Possible to find a way to force this as only way to create MeasureEvents?
let inline createMeasureEvent mEvent =
    { mEvent = mEvent; eID = EventIDManager.Instance.generateID() }

/// Helper func to convert given item to a IndependentEvent.
/// Returns an ErrorEvent if type is not wrapable into MeasureEvent.
let createIndpEvent (item:obj) : MeasureEvent =
    let event = 
        (match item with
        | :? Pitch as p ->
            PitchEvent p
        | :? Rest as r ->
            RestEvent r
        | :? Key as k ->
            KeyEvent k
        | :? TimeSig as t ->
            TimeSigEvent t
        | :? Clef as c ->
            ClefEvent c
        | _ ->
            let msg = sprintf "Cannot create MeasureEvent out of given item: %A" item
            errMsg "%A" msg
            (ErrorEvent msg)
        ) |> IndependentEvent
    //return
    createMeasureEvent event

let inline private createDepEvent dType targets =
    createMeasureEvent (DependentEvent {dType=dType;targets=targets})

let inline createLedgerLineEvent target =
    createDepEvent LedgerLine [target]

let inline createTieEvent origin target =
    createDepEvent Tie [origin; target]

let inline createSlurEvent targets =
    createDepEvent Slur targets

let inline createAccidentalEvent target alt =
    createDepEvent (Accidental alt) [target]

let extractAccidentalFromPitch (p:Pitch) =
    match p.alteration with
    | Some a ->
        Accidental a
    | _ ->
        errMsg "In extractAccidentalFromPitch: Attempted to generate Accidental from None from %A" p
        Accidental (Alteration.Natural)

let extractAccidentalFromMeasureEvent target =
    let accidental =
        match target.mEvent with
        | IndependentEvent i ->
            match i with
            | PitchEvent p ->
                extractAccidentalFromPitch p
            | _ ->
                errMsg "extractAccidental called on %A" target.mEvent
                Accidental Alteration.Natural
        | _ ->
            errMsg "extractAccidental called on %A" target.mEvent
            Accidental Alteration.Natural
    //return
    (createDepEvent accidental [target])

/// Gets the number of Accidentals (Alterations) in a Key
/// Negative numbers indicate flats, positive numbers indicate sharps.
/// Thus, D Major = 2, D minor = -1, etc.
/// Does not handle double flats or sharps currently, so no Fb major, B# major (etc) as of yet.
let getNumAccidentals (keySig:Key) =
    (match keySig.root with
    | Note.C -> 0
    | Note.D -> 2
    | Note.E -> 4
    | Note.F -> -1
    | Note.G -> 1
    | Note.A -> 3
    | Note.B -> 5
    | _ -> 
        errMsg "Unexpecting value hit in match keySig.root in buildDrawableKeysig: %A" keySig.root
        0 ) 
    + //continue
    (match keySig.alteration with
    | Alteration.Flat -> -7
    | Alteration.Natural -> 0
    | Alteration.Sharp -> 7
    | _ -> 
        errMsg "Unexpecting value hit in match keySig.alteration in buildDrawableKeysig: %A" keySig.alteration
        0 )
    + //continue
    (match keySig.quality with
    | Quality.Major -> 0
    | Quality.Minor -> -3 )
    |>  function //check for too many accidentals before returning
        | n when n > 7 || n < -7 ->
            errMsg "Number of sharps/flats exceeds 7 in keysig: %A" keySig
            errMsg "Returning default of C Major/A minor (0)"
            //return
            0
        | n ->
            //return
            n

/// Simply calculates what accidental will be used in a keysig.
let getKeyAccidentalType key =
    match (getNumAccidentals key) with
    | n when n < 0 -> Alteration.Flat
    | n when n > 0 -> Alteration.Sharp
    | _ -> Alteration.Natural

/// Helper for ClefEvents
let isClefEvent(event:MeasureEvent) =
    match event.mEvent with
    | IndependentEvent i ->
        match i with
        | ClefEvent _ ->
            true
        | _ ->
            false
    | _ ->
        false

/// Get clef embedded in MeasureEvent
let tryGetClefFromEvent(event:MeasureEvent) =
    let result = 
        match event.mEvent with
        | IndependentEvent i ->
            match i with
            | ClefEvent c ->
                Some c
            | _ -> 
                None
        | _ ->
            None
    //return
    result


/// Tries to find the lastmost clef in a measure, returns an option (Some cleff/None)
let tryFindPrevClef (measure:Measure) = 
    measure 
    |> List.tryFindBack(isClefEvent) 
    |> function
        | Some m ->
            tryGetClefFromEvent m
        | _ ->
            None

/// Tries to find the first clef in measure
let tryFindFirstClef (measure:Measure) =
    measure 
    |> List.tryFind(isClefEvent) 
    |> function
        | Some m ->
            tryGetClefFromEvent m
        | _ ->
            None

/// Adds single event to given measure and assign it a unique ID
let inline addEvent (measure:Measure) (event:MeasureEvent) = measure@[event]

/// Adds more than one event to a measure and assigns them all IDs
let addMultipleEvents measure events = 
    List.fold addEvent measure events


/// Add single measure to Staff
let inline addMeasure staff measure : Staff = staff@[measure]
/// Add multiple measures to staff
let addMultipleMeasures staff measures : Staff = List.fold addMeasure staff measures

(*
    Default events/etc
*)
let defaultRestEvent = createIndpEvent defaultRest
let defaultPitchEvent = createIndpEvent defaultPitch
let defaultClefEvent = createIndpEvent defaultClef
let defaultKeyEvent = createIndpEvent defaultKey
let defaultTimeSigEvent = createIndpEvent defaultTimeSig

let defaultMeasure:Measure = []

let defaultStaff:Staff = [defaultMeasure]
