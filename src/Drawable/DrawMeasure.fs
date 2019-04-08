module DrawMeasure

open DrawableTypes
open MusicTypes
open DrawIndependentEvent
open DrawDependentEvent

/// Sets geoms for DependentEvents
let private setMultipleDependentEventGeoms measure =
    measure.dEvents
    |> List.map (setDependentEventGeom measure)
    |> fun result -> {measure with dEvents = result}

/// Sets geometries for IndependentEvents
let private setIndpEventGeoms clef =
    setMultipleIndpEventSizes
    >> setIndpEventXCoords
    >> (setIndpEventYCoords clef)

/// Aligns events according to where they fall in the measure.
let assignGeometries clef =
    (setIndpEventGeoms clef)
    >> setMultipleDependentEventGeoms

/// Creates DrawableMeasure out of given Measure.
let createDrawableMeasure initialClef (measure:Measure) x y w h =
    let dEvents = measure |> List.map createDrawableEvent
    let resultMeasure = {dEvents=dEvents; geom=createGeom x y w h UP} |> (assignGeometries initialClef)
    //test to see if the measure is wide enough or not
    let finalX = 
        List.maxBy (fun (e : DrawableEvent) -> e.geom.x + e.geom.w) resultMeasure.dEvents
        |> fun e -> e.geom.x + e.geom.w + MusResources.kerning
    if resultMeasure.geom.x + resultMeasure.geom.w < finalX then
        let newWidth = finalX - resultMeasure.geom.x
        //return
        {resultMeasure with geom = {resultMeasure.geom with w = newWidth}}
    else
        //return
        resultMeasure
