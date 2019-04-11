module FontLocations

[<Literal>]
let fontFolder = "/Fonts/"

let fontsLocation = __SOURCE_DIRECTORY__ + fontFolder

/// Default fonts/numerals
module Default =
    let private defaultDir = fontsLocation + "Default/"
    let nineLoc = defaultDir + "nine_default.png"
    let eightLoc = defaultDir + "eight_default.png"
    let sevenLoc = defaultDir + "seven_default.png"
    let sixLoc = defaultDir + "six_default.png"
    let fiveLoc = defaultDir + "five_default.png"
    let fourLoc = defaultDir + "four_default.png"
    let threeLoc = defaultDir + "three_default.png"
    let twoLoc = defaultDir + "two_default.png"
    let oneRightLoc = defaultDir + "one_default_right_justified.png"
    let oneCenterLoc = defaultDir + "one_default_centered.png"
    let zeroLoc = defaultDir + "zero_default.png"
