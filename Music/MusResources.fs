[<RequireQualifiedAccess>]
///Resources and constants for the project.
module MusResources
//Main consts
let mainWindowWidth = 1024.
let mainWindowHeight = 720.

[<Literal>]
let defaultSizeMultiplier = 1.

//measure
[<Literal>]
let NUM_STAFF_LINES = 5.
let measureWidthDefault = 500. * defaultSizeMultiplier
let measureHeightDefault = 100. * defaultSizeMultiplier

//measure spacings
let measureLineSpacing = measureHeightDefault / (NUM_STAFF_LINES - 1.)
let pitchYSpacing = measureLineSpacing / 2.

//TimeSig
let timeSigWidthDefault = 25. * defaultSizeMultiplier
let timeSigHeightDefault = measureHeightDefault / 2.

//clefs
let bassClefWidthDefault = measureHeightDefault * 0.75 * defaultSizeMultiplier
let bassClefHeightDefault = measureHeightDefault * defaultSizeMultiplier

let trebleClefWidthDefault = measureHeightDefault * 0.65 * defaultSizeMultiplier
let trebleClefHeightDefault = measureHeightDefault * 1.75 * defaultSizeMultiplier

let trebleClefYOffset = measureHeightDefault / 3.5

//stems
let stemXOffset = 1. * defaultSizeMultiplier
let stemYOffsetMultiplier = 0.75 * defaultSizeMultiplier
let stemLengthDefault = 75. * defaultSizeMultiplier

//noteheads
let wholeNoteheadWidthDefault = measureHeightDefault * 0.35 * defaultSizeMultiplier
let wholeNoteheadHeightDefault = measureHeightDefault * 0.25 * defaultSizeMultiplier

let halfNoteheadWidthDefault = measureHeightDefault * 0.30 * defaultSizeMultiplier
let halfNoteheadHeightDefault = 25. * defaultSizeMultiplier

let filledNoteheadWidthDefault = measureHeightDefault * 0.30 * defaultSizeMultiplier
let filledNoteheadHeightDefault = 25. * defaultSizeMultiplier

//ledger line
let ledgerLineWidth = filledNoteheadWidthDefault * 1.50 //multiplier already built-in

//rests
let wholeRestWidthDefault = measureHeightDefault * 0.35 * defaultSizeMultiplier
let wholeRestHeightDefault = measureHeightDefault * 0.15 * defaultSizeMultiplier

let halfRestWidthDefault = wholeRestWidthDefault
let halfRestHeightDefault = wholeRestHeightDefault

let quarterRestWidthDefault = measureHeightDefault * 0.25 * defaultSizeMultiplier
let quarterRestHeightDefault = measureHeightDefault * 0.35 * defaultSizeMultiplier

let eighthRestWidthDefault = measureHeightDefault * 0.05 * defaultSizeMultiplier
let eightRestHeightDefault = measureHeightDefault * 0.10 * defaultSizeMultiplier

//spacing
let kerning = measureWidthDefault * 0.025 * defaultSizeMultiplier


