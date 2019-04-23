module MusicTypes

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

/// Calculates interval from C0 (lowest Midi note)
let inline distFromC0 p = (ordinal p.note) + p.octave * 7 + 1

/// Used to calculate value interval in staff lines and spaces between two pitches.
/// Negative distance means p1 is below p2 on the staff.
let inline staffInterval p1 p2 = (distFromC0 p1) - (distFromC0 p2)

/// Used to calculate the actual (non-qualified, i.e. not major nor minor etc) 
/// interval between two pitches.
///
/// Negative result means p2 is below p1.
let interval p1 p2 =
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