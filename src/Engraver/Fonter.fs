[<RequireQualifiedAccess>]
module Fonter

let getDigitImage n =
    match n with
    | 0 -> Fonts.DefaultDigits.Zero ()
    | 1 -> Fonts.DefaultDigits.One ()
    | 2 -> Fonts.DefaultDigits.Two ()
    | 3 -> Fonts.DefaultDigits.Three ()
    | 4 -> Fonts.DefaultDigits.Four ()
    | 5 -> Fonts.DefaultDigits.Five ()
    | 6 -> Fonts.DefaultDigits.Six ()
    | 7 -> Fonts.DefaultDigits.Seven ()
    | 8 -> Fonts.DefaultDigits.Eight ()
    | 9 -> Fonts.DefaultDigits.Nine ()
    | _ ->
        errMsg "Unhandled numeral handed to Fonter.getNumeralImage: %A" n
        errMsg "Handing back FontLocations.Default.zeroLoc"
        Fonts.DefaultDigits.Zero ()

