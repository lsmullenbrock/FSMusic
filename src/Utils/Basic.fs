[<AutoOpen>]
module Basic

/// Generic logging func
let log = printfn

/// Generic err reporting func
let errMsg = printfn

/// @TODO: Make robust
type MusError = string
