[<RequireQualifiedAccess>]
module Basic

/// Generic logging func
let log = printfn

/// Generic err reporting func
let errMsg = printfn

/// Properly implement error handing later?
type MusError = string