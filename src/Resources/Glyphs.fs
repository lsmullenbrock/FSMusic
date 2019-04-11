[<RequireQualifiedAccess>]
module Glyphs

open System.Windows.Media

let inline private createImage i = DrawingUtils.createImage i

//pitches
let private wholeNoteWidth, wholeNoteHeight = MusResources.wholeNoteheadWidthDefault, MusResources.wholeNoteheadHeightDefault
let private halfNoteWidth, halfNoteHeight = MusResources.halfNoteheadWidthDefault, MusResources.halfNoteheadHeightDefault
let private filledNoteWidth, filledNoteHeight = MusResources.filledNoteheadWidthDefault, MusResources.filledNoteheadHeightDefault
let private ledgerLineWidth = MusResources.ledgerLineWidth
let private pitchYSpacing = MusResources.pitchYSpacing
let private measureLineSpacing = MusResources.measureLineSpacing
//rests
let private eighthRestWidth, eighthRestHeight = MusResources.eighthRestWidthDefault, MusResources.eightRestHeightDefault
let private quarterRestWidth, quarterRestHeight = MusResources.quarterRestWidthDefault, MusResources.quarterRestHeightDefault
let private halfRestWidth, halfRestHeight = MusResources.halfRestWidthDefault, MusResources.halfRestHeightDefault
let private wholeRestWidth, wholeRestHeight = MusResources.wholeRestWidthDefault, MusResources.wholeRestHeightDefault
//ties
let private tieHeight = MusResources.tieHeightDefault
//clefs
let private bassClefWidth, bassClefHeight = MusResources.bassClefWidthDefault, MusResources.bassClefHeightDefault
let private trebleClefWidth, trebleClefHeight = MusResources.trebleClefWidthDefault, MusResources.trebleClefHeightDefault
let private trebleClefYOffset = MusResources.trebleClefYOffset

//alterations/accidentals
let private flatWidth, flatHeight = MusResources.flatWidthDefault, MusResources.flatHeightDefault
let private sharpWidth, sharpHeight = MusResources.sharpWidthDefault, MusResources.sharpHeightDefault
let private naturalWidth, naturalHeight = MusResources.naturalWidthDefault, MusResources.naturalHeightDefault

module Clefs =
    let Treble () = createImage GlyphLocations.trebleClefLocation trebleClefWidth trebleClefHeight
    let Bass () = createImage GlyphLocations.bassClefLocation bassClefWidth bassClefHeight

module Noteheads =
    let Whole () = createImage GlyphLocations.wholeNoteheadLocation wholeNoteWidth wholeNoteHeight
    let Half () = createImage GlyphLocations.halfNoteheadLocation halfNoteWidth halfNoteHeight
    let Filled () = createImage GlyphLocations.filledNoteheadLocation filledNoteWidth filledNoteHeight

module Accidentals =
    let Sharp () = createImage GlyphLocations.sharpImageLocation sharpWidth sharpHeight
    let Natural () = createImage GlyphLocations.naturalImageLocation naturalWidth naturalHeight
    let Flat () = createImage GlyphLocations.flatImageLocation flatWidth flatHeight

module Rests =
    let Whole () = createImage GlyphLocations.wholeRestImageLocation wholeRestWidth wholeRestHeight
    let Half () = createImage GlyphLocations.halfRestImageLocation halfRestWidth halfRestHeight
    let Quarter () = createImage GlyphLocations.quarterRestImageLocation quarterRestWidth quarterRestHeight
    let Eighth () = createImage GlyphLocations.eighthRestImageLocation eighthRestWidth eighthRestHeight

/// Refactor --
module Lines =
    let Slur_Up () = createImage GlyphLocations.slurUpImageLocation
    let Slur_Down () = createImage GlyphLocations.slurDownImageLocation