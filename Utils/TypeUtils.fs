[<AutoOpen>]
///General utilities for general use.
module TypeUtils

open System

/// Translation of Java's enum.ordinal() for .NET
/// Taken from https://stackoverflow.com/a/54594879/7578019
let inline ordinal (e: 'a when 'a : enum<'b> and 'a : equality) =
    let values = Enum.GetValues(e.GetType())
    Array.IndexOf(values, e) + 1

let rec cast<'a> (lst:obj list) =
    match lst with
    | hd::tl ->
        match hd with
        | :? 'a as a -> a::(cast tl)
        | _ -> cast tl
    | _ -> []
