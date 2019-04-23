module MusicTypes
open EventID

/// Octave to be paired with Pitch
type Octave = int

/// Indicates if pitch/rest is dotted.
type Dotted = bool

/// Notes are arranged on the 12-tone "clock" such that
/// C natural is a the 0 (i.e., the '12') position and
/// the notes go up in number clockwise around such that
/// C# is at 1, D is at 2, and so on until B is at 11.
/// Notes are altered with the Alteration type to produce
/// a coherent, full Pitch.
type Note =
    | C = 0
    | D = 2
    | E = 4
    | F = 5
    | G = 7
    | A = 9
    | B = 11

/// Indicates if a pitch is sharp/flat/etc.
type Alteration = 
    | Flat      = -1
    | Natural   = 0
    | Sharp     = 1

/// For pitches/rests
type Value =
    | SixtyFourth   = 64
    | ThirtySecond  = 32
    | Sixteenth     = 16
    | Eighth        = 8
    | Quarter       = 4
    | Half          = 2
    | Whole         = 1

/// Union for Key Quality.
type Quality =
    | Major
    | Minor

/// Basic timesig type.
type TimeSig =
    { numerator: int
      denominator: Value }

/// Clef union.
/// The "NoClef" clef is currently used for preventing drawing errors.
type Clef =
    | Treble
    | Bass
    | NoClef

/// Basic Pitch type.
type Pitch =
    { note: Note
      alteration: Alteration option
      octave: Octave
      value: Value
      dotted: Dotted }

/// Rest types are only a silent duration.
type Rest =
    { value: Value
      dotted: Dotted }

/// Denotes the actual Key a segment of music is "in".
/// Clef is needed to be known for proper arrangement of accidentals.
type Key =
    { root: Note
      alteration: Alteration
      quality: Quality
      clef: Clef }

/// Clef is necessary for proper placement of accidentals in the Engraver.
let inline createKey root alteration quality clef =
    { root = root; alteration = alteration; quality = quality; clef = clef }

/// Helper func to generate a Rest.
let inline createRest value dotted =
    { value = value; dotted = dotted }

/// Helper func to generate a Pitch.
let inline createPitch note alteration octave value dotted =
    { note = note; alteration = alteration; octave = octave; value = value; dotted = dotted }

/// Helper func to generate a TimeSig.
let inline createTimeSig numerator denominator =
    { numerator = numerator; denominator = denominator }

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

/// This function is mandatory for generating new events!
/// @TODO Possible to find a way to force this as only way to create MeasureEvents?
let createMeasureEvent mEvent =
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
    createMeasureEvent event

let inline private createDepEvent dType targets =
    let event = {dType=dType;targets=targets} |> DependentEvent
    createMeasureEvent event

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
    | Quality.Minor -> -3)
    |> fun n -> //check for too many accidentals
        if n > 7 || n < -7 then
            errMsg "Number of sharps/flats exceeds 7 in keysig: %A" keySig
            errMsg "Returning default of C Major/A minor (0)"
            //return
            0
        else
            //return
            n

let getKeyAccidentalType key =
    let numAccidentals = getNumAccidentals key
    match numAccidentals with
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

/// Type is meant to be simple/"dumb".
type Measure = MeasureEvent list

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

/// Staves are simply Measure lists
type Staff = Measure list
/// Add single measure to Staff
let inline addMeasure staff measure : Staff = staff@[measure]
/// Add multiple measures to staff
let addMultipleMeasures staff measures : Staff = List.fold addMeasure staff measures
/// Calculates interval from C0 (lowest Midi note)
let distFromC0 p = (ordinal p.note) + p.octave * 7 + 1
/// Used to calculate value interval in staff lines and spaces between two pitches.
/// Negative distance means p1 is below p2 on the staff.
let inline staffInterval p1 p2 = (distFromC0 p1) - (distFromC0 p2)
/// Used to calculate the actual (non-qualified, i.e. not major nor minor etc) 
/// interval between two pitches.
///
/// Negative result means p2 is below p1.
let inline interval p1 p2 =
    let dist = staffInterval p1 p2
    if dist >= 0 then
        dist + 1
    else
        dist - 1
(*
    Default events/etc
*)
let defaultRest = createRest Value.Quarter false
let defaultPitch = createPitch Note.C None 4 Value.Quarter false
let defaultClef = Clef.Treble
let defaultKey = { root = Note.C; alteration = Alteration.Natural; quality = Quality.Major; clef = Treble }
let defaultTimeSig = createTimeSig 4 Value.Quarter

let defaultRestEvent = createIndpEvent defaultRest
let defaultPitchEvent = createIndpEvent defaultPitch
let defaultClefEvent = createIndpEvent defaultClef
let defaultKeyEvent = createIndpEvent defaultKey
let defaultTimeSigEvent = createIndpEvent defaultTimeSig

let defaultMeasure:Measure = []

let defaultStaff:Staff = [defaultMeasure]
