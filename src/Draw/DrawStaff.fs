module DrawStaff

open DrawableTypes
open MusicBase
open DrawMeasure

/// Ensures given Staff is not empty or is malformed, etc.
let private verifyStaff (staff:Staff) = 
    let check = 
        if staff.IsEmpty  then
            errMsg "Empty staff given for createDrawableStaff"
            false
        else if staff.Head.IsEmpty then
            errMsg "First measure is empty in staff given to createDrawableStaff"
            false
        else
            true
    //return
    check

/// Helper func to create DrawableStaff
let private createDrawableStaff (staff:Staff) x y w h : DrawableMeasure list =
    let mutable currentClef:Clef =
        staff
        |> List.head
        |> tryFindFirstClef
        |> function
            | Some c ->
                c
            | _ ->
                errMsg "No first clef found in staff %A, assuming Treble" staff
                Treble
    //return
    [
        for measure in staff do
            let dMeasure = createDrawableMeasure currentClef measure x y (w / (staff.Length|>float)) h
            match (tryFindPrevClef measure) with
            | Some c -> 
                currentClef <- c
            | None ->
                () //do nothing
            yield dMeasure
    ]

/// Creates DrawableStaff for a given Staff, which is verified, and assigns geometries
/// to its measures/events
let createVerifiedDrawableStaff (staff:Staff) x y w h =
    let measures = 
        match verifyStaff staff with
        | true -> 
            createDrawableStaff staff x y w h
        | _ -> 
            errMsg "verifyStaff failed for given staff: %A" staff
            []
    //return
    {measures=measures;geom=createGeom x y w h UP}