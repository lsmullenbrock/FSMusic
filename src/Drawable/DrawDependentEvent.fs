module DrawDependentEvent

open MusicTypes
open DrawableTypes

(*
    DependentEvent MusGeom funcs
*)
/// Returns a DrawableEvent from a measure given an EventID 
let private findEventByID measure eID = 
    List.find (fun e -> e.event.eID = eID) measure.dEvents

/// Returns a DrawableEvent from a DrawableStaff given an EventID
let private findEventInStaff (staff:DrawableStaff) eID = 
    [for measure in staff.measures do yield findEventByID measure eID]
    |> List.head

/// Returns a MusGeom given an EventID
let private getEventGeomByID measure eID =
    findEventByID measure eID
    |> fun e -> e.geom

/// 
let private calcTieGeom measure tie =
    /// TODO: Make this safe!
    let originGeom = getEventGeomByID measure tie.targets.[0].eID
    let targetGeom = getEventGeomByID measure tie.targets.[1].eID
    let x = originGeom.x + originGeom.w
    let y, orientation =  // needs to face opposite way of stem
        match originGeom.orientation with
        | UP -> 
            originGeom.y + originGeom.h, 
            DOWN
        | DOWN -> 
            originGeom.y - originGeom.h / 2.,
            UP
    let w = targetGeom.x - x
    let h = tieHeight
    //return
    (createGeom x y w h orientation)

let private calcSlurGeom measure slur =
    let drawableTargets =
        slur.targets
        |> List.map(fun t -> t.eID)
        |> List.sort //just in case
        |> List.map(fun e -> findEventByID measure e)

    //individually figure out each coord
    //lowest eID will be the leftmost and thus have the correct
    //starting coords
    let x, y =
        drawableTargets
        |> List.sortBy(fun t -> t.geom.x)
        |> List.head
        |> fun t -> t.geom.x, t.geom.y

    //width will be the rightmost, and thus the highest x minus the lowest x
    let w =
        drawableTargets
        |> List.maxBy(fun t -> t.geom.x)
        |> fun t -> t.geom.x - x

    //we will have to rotate the slur image to actually get the correct
    //alignment
    //height will be the heighest y minus the starting y
    //negative y indicates DOWN orientation
    let h =
        let highest =
            drawableTargets
            |> List.maxBy(fun t -> t.geom.y + t.geom.h)
            |> fun t -> t.geom.y + t.geom.h
        (highest - y)

    let orientation = UP
    //return
    (createGeom x y w h orientation)

let private calcAlterationGeom measure acc =
    let targetGeom = getEventGeomByID measure acc.targets.[0].eID
    let x, y, w, h =
        match acc.dType with
        | Accidental a ->
            match a with
            | Alteration.Flat ->
                let x = targetGeom.x - kerning
                let y = targetGeom.y - targetGeom.h / 2.
                let w = flatWidth
                let h = flatHeight
                x, y, w, h
            | Alteration.Natural ->
                let x = targetGeom.x - kerning / 1.25
                let y = targetGeom.y - targetGeom.h / 4.
                let w = naturalWidth
                let h = naturalHeight
                x, y, w, h
            | Alteration.Sharp ->
                let x = targetGeom.x - kerning
                let y = targetGeom.y - targetGeom.h / 4.
                let w = sharpWidth
                let h = sharpHeight
                x, y, w, h
            | _ ->
                errMsg "Illegal Alteration: %A hit in calcAlterationGeom -> acc.dType -> match a with ..." a
                defaultGeomTuple
        | _ -> 
            errMsg "Unmatched case in calcAlterationGeom -> match acc.dType with hit by: %A" acc.dType
            defaultGeomTuple
    (createGeom x y w h UP)

/// All DependentEvent coords can be set at once because we already know all of it.
let setDependentEventGeom (measure:DrawableMeasure) (dEvent:DrawableEvent) =
    match dEvent.event.mEvent with
    | DependentEvent d ->
        let result:DrawableEvent =
            match d.dType with
            | LedgerLine -> 
                //already assigned a geometry
                dEvent
            | Slur ->
                let geom = calcSlurGeom measure d
                {dEvent with geom = geom}
            | Tie ->
                let geom = calcTieGeom measure d
                {dEvent with geom = geom}
            | Accidental _ ->
                let geom = calcAlterationGeom measure d
                {dEvent with geom = geom}
            //| _ ->
            //    errMsg "DependentEvent %A could not be assigned a MusGeom" d
            //    dEvent
        //return
        result
    | _ ->
        // IndependentEvent cases already taken care of (!)
        dEvent
