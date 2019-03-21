[<RequireQualifiedAccess>]
///Resources and constants for the project.
module MusResources
//Main consts
let mainWindowWidth = 1024.
let mainWindowHeight = 720.

[<Literal>]
// 
let SIZE_MULTIPLIER = 1.

//padding
let leftPadding, rightPadding = mainWindowHeight/15., mainWindowHeight/15.

//measure
[<Literal>]
let NUM_STAFF_LINES = 5.
let measureWidthDefault = (mainWindowWidth - leftPadding - rightPadding) * SIZE_MULTIPLIER
let measureHeightDefault = 100. * SIZE_MULTIPLIER

//measure spacings
let measureLineSpacing = measureHeightDefault / (NUM_STAFF_LINES - 1.)
let pitchYSpacing = measureLineSpacing / 2.

//TimeSig
let timeSigWidthDefault = 25. * SIZE_MULTIPLIER
let timeSigHeightDefault = measureHeightDefault / 2.

//clefs
let bassClefWidthDefault = measureHeightDefault * 0.75 * SIZE_MULTIPLIER
let bassClefHeightDefault = measureHeightDefault * SIZE_MULTIPLIER

let trebleClefWidthDefault = measureHeightDefault * 0.65
let trebleClefHeightDefault = measureHeightDefault * 1.75

let trebleClefYOffset = measureHeightDefault / 3.5

//stems
let stemXOffset = 1. * SIZE_MULTIPLIER
let stemYOffsetMultiplier = 0.75 * SIZE_MULTIPLIER
let stemLengthDefault = 75. * SIZE_MULTIPLIER

//dot
let dotSizeDefault = measureHeightDefault * 0.05

//noteheads
let wholeNoteheadWidthDefault = measureHeightDefault * 0.35
let wholeNoteheadHeightDefault = measureHeightDefault * 0.25

let halfNoteheadWidthDefault = measureHeightDefault * 0.30
let halfNoteheadHeightDefault = 25. * SIZE_MULTIPLIER

let filledNoteheadWidthDefault = measureHeightDefault * 0.30
let filledNoteheadHeightDefault = 25. * SIZE_MULTIPLIER

//ledger line
let ledgerLineWidth = filledNoteheadWidthDefault * 1.50

//rests
let eighthRestWidthDefault = filledNoteheadWidthDefault * 0.5
let eightRestHeightDefault = filledNoteheadWidthDefault

let quarterRestWidthDefault = filledNoteheadWidthDefault * 0.75
let quarterRestHeightDefault = filledNoteheadHeightDefault * 2.

let halfRestWidthDefault = halfNoteheadWidthDefault * 1.25
let halfRestHeightDefault = halfNoteheadHeightDefault * 0.4

let wholeRestWidthDefault = halfRestWidthDefault
let wholeRestHeightDefault = halfRestHeightDefault

//ties
let tieHeightDefault = filledNoteheadHeightDefault / 1.5

//spacing
let kerning = measureWidthDefault * 0.03


