[<RequireQualifiedAccess>]
module Fonts

let private w, h = MusResources.timeSigWidthDefault, MusResources.timeSigHeightDefault

let inline private createImage i = DrawingUtils.createImage i

/// Images generated on load
module DefaultDigits =
    let Zero () = createImage FontLocations.Default.zeroLoc w h
    let One () = createImage FontLocations.Default.oneCenterLoc w h
    let Two () = createImage FontLocations.Default.twoLoc w h
    let Three () = createImage FontLocations.Default.threeLoc w h
    let Four () = createImage FontLocations.Default.fourLoc w h
    let Five () = createImage FontLocations.Default.fiveLoc w h
    let Six () = createImage FontLocations.Default.sixLoc w h
    let Seven () = createImage FontLocations.Default.sevenLoc w h
    let Eight () = createImage FontLocations.Default.eightLoc w h
    let Nine () = createImage FontLocations.Default.nineLoc w h
