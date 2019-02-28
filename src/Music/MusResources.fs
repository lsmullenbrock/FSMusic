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
let leftPadding, rightPadding = mainWindowHeight/10., mainWindowHeight/10.

//measure
[<Literal>]
let NUM_STAFF_LINES = 5.
let measureWidthDefault = (mainWindowHeight - leftPadding - rightPadding) * SIZE_MULTIPLIER
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

let trebleClefWidthDefault = measureHeightDefault * 0.65 * SIZE_MULTIPLIER
let trebleClefHeightDefault = measureHeightDefault * 1.75 * SIZE_MULTIPLIER

let trebleClefYOffset = measureHeightDefault / 3.5

//stems
let stemXOffset = 1. * SIZE_MULTIPLIER
let stemYOffsetMultiplier = 0.75 * SIZE_MULTIPLIER
let stemLengthDefault = 75. * SIZE_MULTIPLIER

//noteheads
let wholeNoteheadWidthDefault = measureHeightDefault * 0.35 * SIZE_MULTIPLIER
let wholeNoteheadHeightDefault = measureHeightDefault * 0.25 * SIZE_MULTIPLIER

let halfNoteheadWidthDefault = measureHeightDefault * 0.30 * SIZE_MULTIPLIER
let halfNoteheadHeightDefault = 25. * SIZE_MULTIPLIER

let filledNoteheadWidthDefault = measureHeightDefault * 0.30 * SIZE_MULTIPLIER
let filledNoteheadHeightDefault = 25. * SIZE_MULTIPLIER

//ledger line
let ledgerLineWidth = filledNoteheadWidthDefault * 1.50 //multiplier already built-in

//rests
let wholeRestWidthDefault = measureHeightDefault * 0.35 * SIZE_MULTIPLIER
let wholeRestHeightDefault = measureHeightDefault * 0.15 * SIZE_MULTIPLIER

let halfRestWidthDefault = wholeRestWidthDefault
let halfRestHeightDefault = wholeRestHeightDefault

let quarterRestWidthDefault = measureHeightDefault * 0.25 * SIZE_MULTIPLIER
let quarterRestHeightDefault = measureHeightDefault * 0.5 * SIZE_MULTIPLIER

let eighthRestWidthDefault = measureHeightDefault * 0.05 * SIZE_MULTIPLIER
let eightRestHeightDefault = measureHeightDefault * 0.10 * SIZE_MULTIPLIER

//spacing
let kerning = measureWidthDefault * 0.025 * SIZE_MULTIPLIER


