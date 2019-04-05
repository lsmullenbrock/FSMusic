module FontLocations

[<Literal>]
let fontDir = __SOURCE_DIRECTORY__ + "/Fonts"

/// Default fonts/numerals
module Default =
    [<Literal>]
    let private defaultDir = fontDir + "/Default"

    [<Literal>]
    let nineLoc = defaultDir + "nine_default.png"
    [<Literal>]
    let eightLoc = defaultDir + "eight_default.png"
    [<Literal>]
    let sevenLoc = defaultDir + "seven_default.png"
    [<Literal>]
    let sixLoc = defaultDir + "six_default.png"
    [<Literal>]
    let fiveLoc = defaultDir + "five_default.png"
    [<Literal>]
    let fourLoc = defaultDir + "four_default.png"
    [<Literal>]
    let threeLoc = defaultDir + "three_default.png"
    [<Literal>]
    let twoLoc = defaultDir + "two_default.png"
    [<Literal>]
    let oneRightLoc = defaultDir + "one_default_right_justified.png"
    [<Literal>]
    let oneCenterLoc = defaultDir + "one_default_centered.png"
    [<Literal>]
    let zeroLoc = defaultDir + "zero_default.png"
