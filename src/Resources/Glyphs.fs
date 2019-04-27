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
    let Treble () = createImg GlyphLocations.Clefs.trebleClefLocation trebleClefWidth trebleClefHeight
    let Bass () = createImg GlyphLocations.Clefs.bassClefLocation bassClefWidth bassClefHeight

module Noteheads =
    let Whole () = createImg GlyphLocations.Noteheads.wholeNoteheadLocation wholeNoteWidth wholeNoteHeight
    let Half () = createImg GlyphLocations.Noteheads.halfNoteheadLocation halfNoteWidth halfNoteHeight
    let Filled () = createImg GlyphLocations.Noteheads.filledNoteheadLocation filledNoteWidth filledNoteHeight

module Accidentals =
    let Sharp () = createImg GlyphLocations.Accidentals.sharpImageLocation sharpWidth sharpHeight
    let Natural () = createImg GlyphLocations.Accidentals.naturalImageLocation naturalWidth naturalHeight
    let Flat () = createImg GlyphLocations.Accidentals.flatImageLocation flatWidth flatHeight

module Rests =
    let Whole () = createImg GlyphLocations.Rests.wholeRestImageLocation wholeRestWidth wholeRestHeight
    let Half () = createImg GlyphLocations.Rests.halfRestImageLocation halfRestWidth halfRestHeight
    let Quarter () = createImg GlyphLocations.Rests.quarterRestImageLocation quarterRestWidth quarterRestHeight
    let Eighth () = createImg GlyphLocations.Rests.eighthRestImageLocation eighthRestWidth eighthRestHeight

/// Refactor --
module Lines =
    let Slur_Up () = createImg GlyphLocations.Lines.slurUpImageLocation
    let Slur_Down () = createImg GlyphLocations.Lines.slurDownImageLocation