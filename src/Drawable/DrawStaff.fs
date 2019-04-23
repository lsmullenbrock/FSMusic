module DrawStaff

open DrawableTypes
open MusicTypes
open EventTypes
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

/// Helper func to create DrawableStaff. 
/// If no clef found, defaults to Treble
let private createDrawableStaff (staff:Staff) x y w h : DrawableMeasure list =
    let mutable curX = x
    let mutable curY = y
    let mutable currentClef : Clef =
        staff
        |> List.head
        |> tryFindFirstClef
        |> function
            | Some c ->
                c
            | None ->
                errMsg "No first clef found in staff %A, assuming Treble" staff
                Treble
    //return as list comprehension
    [
        for measure in staff do
            let mutable dMeasure = createDrawableMeasure currentClef measure curX curY w h
            curX <- dMeasure.geom.x + dMeasure.geom.w
            
            //start new line if x+w > margin space
            //i.e., a "carriage return" or "newline" for music
            if (curX + dMeasure.geom.w) > (MusResources.mainWindowWidth) then
                curY <- curY + MusResources.measureHeightDefault * 2.
                dMeasure <- createDrawableMeasure currentClef measure x curY w h // "return" to original x position
                curX <- dMeasure.geom.x + dMeasure.geom.w
            else
                ()

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