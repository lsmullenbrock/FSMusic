[<AutoOpen>]
module Basic

open System

/// Generic logging func
let log = printfn

/// Generic err reporting func
let errMsg = printfn

/// @TODO: Make robust
type MusError = string

/// Translation of Java's enum.ordinal() for .NET	
/// Taken from https://stackoverflow.com/a/54594879/7578019
let inline ordinal (e: 'a when 'a : enum<'b> and 'a : equality) =
    let values = Enum.GetValues(e.GetType())
    Array.IndexOf(values, e) + 1 