[<RequireQualifiedAccess>]
module Glyphs

let inline private createImg i = DrawingUtils.createImage i

//pitches
let private wholeNoteWidth, wholeNoteHeight = MusResources.wholeNoteheadWidthDefault, MusResources.wholeNoteheadHeightDefault
let private halfNoteWidth, halfNoteHeight = MusResources.halfNoteheadWidthDefault, MusResources.halfNoteheadHeightDefault
let private filledNoteWidth, filledNoteHeight = MusResources.filledNoteheadWidthDefault, MusResources.filledNoteheadHeightDefault
//rests
let private eighthRestWidth, eighthRestHeight = MusResources.eighthRestWidthDefault, MusResources.eightRestHeightDefault
let private quarterRestWidth, quarterRestHeight = MusResources.quarterRestWidthDefault, MusResources.quarterRestHeightDefault
let private halfRestWidth, halfRestHeight = MusResources.halfRestWidthDefault, MusResources.halfRestHeightDefault
let private wholeRestWidth, wholeRestHeight = MusResources.wholeRestWidthDefault, MusResources.wholeRestHeightDefault
//clefs
let private bassClefWidth, bassClefHeight = MusResources.bassClefWidthDefault, MusResources.bassClefHeightDefault
let private trebleClefWidth, trebleClefHeight = MusResources.trebleClefWidthDefault, MusResources.trebleClefHeightDefault

//alterations/accidentals
let private flatWidth, flatHeight = MusResources.flatWidthDefault, MusResources.flatHeightDefault
let private sharpWidth, sharpHeight = MusResources.sharpWidthDefault, MusResources.sharpHeightDefault
let private naturalWidth, naturalHeight = MusResources.naturalWidthDefault, MusResources.naturalHeightDefault

module Clefs =
    let Treble () = createImg GlyphLocations.trebleClefLocation trebleClefWidth trebleClefHeight
    let Bass () = createImg GlyphLocations.bassClefLocation bassClefWidth bassClefHeight

module Noteheads =
    let Whole () = createImg GlyphLocations.wholeNoteheadLocation wholeNoteWidth wholeNoteHeight
    let Half () = createImg GlyphLocations.halfNoteheadLocation halfNoteWidth halfNoteHeight
    let Filled () = createImg GlyphLocations.filledNoteheadLocation filledNoteWidth filledNoteHeight

module Accidentals =
    let Sharp () = createImg GlyphLocations.sharpImageLocation sharpWidth sharpHeight
    let Natural () = createImg GlyphLocations.naturalImageLocation naturalWidth naturalHeight
    let Flat () = createImg GlyphLocations.flatImageLocation flatWidth flatHeight

module Rests =
    let Whole () = createImg GlyphLocations.wholeRestImageLocation wholeRestWidth wholeRestHeight
    let Half () = createImg GlyphLocations.halfRestImageLocation halfRestWidth halfRestHeight
    let Quarter () = createImg GlyphLocations.quarterRestImageLocation quarterRestWidth quarterRestHeight
    let Eighth () = createImg GlyphLocations.eighthRestImageLocation eighthRestWidth eighthRestHeight

/// Refactor --
module Lines =
    let Slur_Up () = createImg GlyphLocations.slurUpImageLocation
    let Slur_Down () = createImg GlyphLocations.slurDownImageLocation