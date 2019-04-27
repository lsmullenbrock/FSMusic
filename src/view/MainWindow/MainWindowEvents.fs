module MainWindowEvents

open System.Windows

open EventTypes
open MusicTypes
open Engraver

//-----------------------------------TESTS---------------------------------------
let private test (engraver:Engraver) =
    engraver.clearCanvas()

    let e0 = createIndpEvent Treble
    let e1 = defaultTimeSigEvent
    let e2 = createIndpEvent defaultPitch
    let e3 = createIndpEvent {defaultPitch with note=Note.D}
    let e4 = createIndpEvent {defaultPitch with note=Note.E}
    let e5 = createIndpEvent {defaultPitch with note=Note.F; alteration=Some Alteration.Sharp}
    let e6 = createIndpEvent {defaultPitch with note=Note.G; alteration=Some Alteration.Sharp}
    let e7 = createIndpEvent {defaultPitch with note=Note.A}
    let e8 = createIndpEvent {defaultPitch with note=Note.B}
    let e9 = createIndpEvent {defaultPitch with octave = 5}

    let m1 = addMultipleEvents defaultMeasure [e0;e1;e2;e3;e4;e5]
    let m2 = addMultipleEvents defaultMeasure [e6;e7;e8;e9]

    let staff1 = DrawStaff.createVerifiedDrawableStaff [m1;m2] MusResources.leftPadding MusResources.topPadding MusResources.measureWidthDefault MusResources.measureHeightDefault

    engraver.engraveStaff staff1
//-------------------------------------------------------------------------------

let clickEvent_about (aboutWindow:unit->Window) _ =
    aboutWindow().ShowDialog()
    |> ignore

let clickEvent_clear (engraver:Engraver) _ = 
    engraver.clearCanvas()

let clickEvent_testCanvas (engraver:Engraver) _ =
    test engraver
    //necessary for huge image mem usage
    for _ in [1..10] do
        System.GC.WaitForPendingFinalizers()
        System.GC.Collect()

let clickEvent_close (window:Window) _ = 
    window.Close()

