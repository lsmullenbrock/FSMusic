[<RequireQualifiedAccess>]
module GlyphLocations

[<Literal>]
let currentLoc = __SOURCE_DIRECTORY__ + "/Glyphs/"

module Clefs =
    [<Literal>]
    let clefDir = currentLoc + "Clefs/"
    [<Literal>]
    let bassClefLocation = clefDir + "bass_clef.png"
    [<Literal>]
    let trebleClefLocation = clefDir + "treble_clef.png"

module Noteheads =
    [<Literal>]
    let noteHeadDir = currentLoc + "Noteheads/"
    [<Literal>]
    let wholeNoteheadLocation = noteHeadDir + "whole_note.png"
    [<Literal>]
    let halfNoteheadLocation = noteHeadDir + "open_notehead.png"
    [<Literal>]
    let filledNoteheadLocation = noteHeadDir + "filled_notehead.png"

module Accidentals =
    [<Literal>]
    let accidentalDir = currentLoc + "Accidentals/"
    [<Literal>]
    let sharpImageLocation = accidentalDir + "sharp.png"
    [<Literal>]
    let flatImageLocation = accidentalDir + "flat.png"
    [<Literal>]
    let naturalImageLocation = accidentalDir + "natural.png"

module Rests =
    [<Literal>]
    let restDir = currentLoc + "Rests/"
    [<Literal>]
    let eighthRestImageLocation = restDir + "eighth_rest.png"
    [<Literal>]
    let quarterRestImageLocation = restDir + "quarter_rest.png"
    [<Literal>]
    let halfRestImageLocation = restDir + "half_rest.png"
    [<Literal>]
    let wholeRestImageLocation = restDir + "whole_rest.png"

module Lines =
    [<Literal>]
    let lineDir = currentLoc + "Lines/"
    [<Literal>]
    let slurUpImageLocation = lineDir + "slur_up.png"
    [<Literal>]
    let slurDownImageLocation = lineDir + "slur_down.png"