module MusicBase
/// Octave to be paired with Pitch
type Octave = int
/// Indicates if pitch/rest is dotted.
type Dotted = bool
/// Indicates if pitch/rest is tied.
type Tied = bool
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
/// Value is NOT considered the same thing as a note's actual time duration.
/// Value is an Enum, whereas duration is how much time/space the event actually takes.
/// "Duration" here denotes Value (e.g., Quarter) with some modifier
/// (e.g., a dot or double dot, which extends its actual played duration).
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
      tied: Tied
      value: Value
      dotted: Dotted }

/// Tie, may need refactor?
type Tie = 
    { origin: Pitch
      target: Pitch }

/// Rest types are only a silent duration.
type Rest =
    { value: Value
      dotted: Dotted }
/// Denotes the actual Key a segment of music is "in".
type Key =
    { root: Note
      alteration: Alteration
      quality: Quality }
/// Helper func to generate a Rest.
let createRest value dotted =
    { value = value; dotted = dotted }
/// Helper func to generate a Pitch.
let createPitch note alteration octave value tied dotted =
    { note = note; alteration = alteration; octave = octave; value = value; tied = tied; dotted = dotted }
/// Helper func to generate a TimeSig.
let createTimeSig numerator denominator =
    { numerator = numerator; denominator = denominator }
/// Wrapper type so that Pitches, Rests, Clefs and so on can be packed into a Measure together.
type MeasureEvent =
    | PitchEvent of Pitch
    | TieEvent of Tie
    | LedgerLineEvent
    | RestEvent of Rest
    | KeyEvent of Key
    | ClefEvent of Clef
    | TimeSigEvent of TimeSig
    | ErrorEvent of Basic.MusError
/// Helper func to convert given item to a MeasureEvent.
/// Returns an ErrorEvent if type is not wrapable into MeasureEvent.
let createEvent (item:obj) =
    match item with
    | :? Pitch as p ->
        PitchEvent p
    | :? Tie as t ->
        TieEvent t
    | :? Rest as r ->
        RestEvent r
    | :? Clef as c ->
        ClefEvent c
    | :? Key as k ->
        KeyEvent k
    | :? TimeSig as t ->
        TimeSigEvent t
    | _ -> 
        Basic.errMsg "%A cannot be cast into MeasureEvent! Returning an ErrorEvent instead." item
        ErrorEvent (Basic.MusErr (sprintf "Error in createEvent with item: %A" item))
/// (Tries to) create(s) multiple events from a list of obj's.
let createMultipleEvents (items:obj list) = List.map createEvent items
/// Helper func to unbox event into basic type.
/// Must be careful to properly cast recieving let binding/etc.
let unboxEvent (event:MeasureEvent) = unbox event

/// Helper func for clefs
let isClef (item:obj) =
    match item with
    | :? Clef -> true
    | _ -> false

/// Type is meant to be simple/"dumb".
type Measure = MeasureEvent list 
/// Adds single event to given measure
let addEvent (measure:Measure) (event:MeasureEvent) = measure@[event]
/// Adds more than one event to a measure
let addMultipleEvents measure events = List.fold addEvent measure events

/// Staves are simply Measure lists
type Staff = Measure list
/// Add single measure to Staff
let addMeasure staff measure : Staff = staff@[measure]
/// Add multiple measures to staff
let addMultipleMeasures staff measures : Staff = List.fold addMeasure staff measures
/// Calculates interval from C0 (lowest Midi note)
let distFromC0 p = (ordinal p.note) + p.octave * 7 + 1
/// Used to calculate the actual interval between two pitches.
/// Negative result means p2 is below p1.
let inline interval p1 p2 =
    let p1Position = distFromC0 p1
    let p2Position = distFromC0 p2
    let dist = p1Position - p2Position
    if dist >= 0 then
        dist + 1
    else
        dist - 1

//Default events/etc
let defaultRest = createRest Value.Quarter false
let defaultPitch = createPitch Note.C None 4 Value.Quarter false false
let defaultClef = Clef.Treble
let defaultKey = { root = Note.C; alteration = Alteration.Natural; quality = Quality.Major }
let defaultTimeSig = createTimeSig 4 Value.Quarter

let defaultRestEvent = createEvent defaultRest
let defaultPitchEvent = createEvent defaultPitch
let defaultClefEvent = createEvent defaultClef
let defaultKeyEvent = createEvent defaultKey
let defaultTimeSigEvent = createEvent defaultTimeSig

let defaultMeasure:Measure = []

let defaultStaff:Staff = [defaultMeasure]
