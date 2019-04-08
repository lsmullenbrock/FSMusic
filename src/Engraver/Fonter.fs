[<RequireQualifiedAccess>]
module Fonter

let inline private createImage i = EngraverUtils.createImage i

let getDigitImage n w h =
    (match n with
    | 0 -> FontLocations.Default.zeroLoc
    | 1 -> FontLocations.Default.oneCenterLoc
    | 2 -> FontLocations.Default.twoLoc
    | 3 -> FontLocations.Default.threeLoc
    | 4 -> FontLocations.Default.fourLoc
    | 5 -> FontLocations.Default.fiveLoc
    | 6 -> FontLocations.Default.sixLoc
    | 7 -> FontLocations.Default.sevenLoc
    | 8 -> FontLocations.Default.eightLoc
    | 9 -> FontLocations.Default.nineLoc
    | _ ->
        errMsg "Unhandled numeral handed to Fonter.getNumeralImage: %A" n
        errMsg "Handing back FontLocations.Default.zeroLoc"
        FontLocations.Default.zeroLoc
    ) |> fun i -> createImage i w h 

