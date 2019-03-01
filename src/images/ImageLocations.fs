[<RequireQualifiedAccess>]
module ImageLocations

[<Literal>]
let currentLoc = __SOURCE_DIRECTORY__ + "/"

//Clefs
[<Literal>]
let bassClefLocation = currentLoc + "bass_clef.png"
[<Literal>]
let trebleClefLocation = currentLoc + "treble_clef.png"

//Noteheads
[<Literal>]
let wholeNoteheadLocation = currentLoc + "whole_note.png"
[<Literal>]
let halfNoteheadLocation = currentLoc + "open_notehead.png"
[<Literal>]
let filledNoteheadLocation = currentLoc + "filled_notehead.png"

//Accidentals
[<Literal>]
let sharpImageLocation = currentLoc + "sharp.png"
[<Literal>]
let flatImageLocation = currentLoc + "flat.png"
[<Literal>]
let naturalImageLocation = currentLoc + "natural.png"

//Rests
[<Literal>]
let eighthRestImageLocation = currentLoc + "eighth_rest.png"
[<Literal>]
let quarterRestImageLocation = currentLoc + "quarter_rest.png"
[<Literal>]
let halfRestImageLocation = currentLoc + "half_rest.png"
[<Literal>]
let wholeRestImageLocation = currentLoc + "whole_rest.png"