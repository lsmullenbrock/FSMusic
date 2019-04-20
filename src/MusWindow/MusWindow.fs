module MusWindow

open System.Windows
open System.Windows.Controls

open MusicTypes
open Engraver


/// Helper func to make adding elements easier.
let addControlsToPanel (panel:#Panel) controlList = 
    (List.map(panel.Children.Add >> ignore) controlList) 
    |> ignore

//-----------------------------------TESTS---------------------------------------
let test (engraver:Engraver) =
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

let makeWindow width height title icon =
    new Window(
        Width = width,
        Height = height,
        Title = title,
        Icon = (DrawingUtils.loadImageFile icon),
        WindowStartupLocation = WindowStartupLocation.CenterScreen
    )

let createButton w h margin text clickEvent toolTipText =
    let newButton = 
        new Button(
            Content = text,
            Height = h,
            Width = w,
            Margin = margin
        )
    newButton.Click.Add clickEvent
    if toolTipText <> "" then
        newButton.ToolTip <- toolTipText
    //return
    newButton

let makeDefaultWindow width height =
    let window = makeWindow width height "Music Engraver Test" GlyphLocations.Accidentals.flatImageLocation
    
    let canvas = new Canvas()
    let engraver = new Engraver(canvas)

    let buttonHeight = 20.
    let buttonWidth = width / 10.
    let buttonMargin = Thickness 5.
    
    let defaultMakeBtn = createButton buttonWidth buttonHeight buttonMargin

    let clickEvent_testCanvas _ =
        log "clickEvent_testCanvas clicked"
        test engraver

    let drawExerciseButton = defaultMakeBtn "Draw Exercise" (clickEvent_testCanvas) "Generate and Draw new exercise."
    drawExerciseButton.IsDefault <- true

    let clickEvent_clear _ = 
        engraver.clearCanvas()
    let clearButton = defaultMakeBtn "Clear Canvas" (clickEvent_clear) "Clear the Canvas."
    clearButton.IsCancel <- true

    let clickEvent_help _ = 
        MessageBox.Show("Click the Draw Exercise button to draw notes.")
        |> ignore
    let helpButton = defaultMakeBtn "Help" (clickEvent_help) "Click for a Help dialogue."
    
    let clickEvent_close _ = 
        window.Close()
    let closeButton = defaultMakeBtn "Close" (clickEvent_close) "Close this window."

    let clickEvent_GC _ =
        for _ in [1..10] do
            System.GC.WaitForPendingFinalizers()
            System.GC.Collect()
        log "GC.Collect() x 10"
    let garbageCollectButton = defaultMakeBtn "GC" (clickEvent_GC) "Force garbage collection."

    let mainButtonPanel = new StackPanel(Orientation = Orientation.Horizontal)
    addControlsToPanel mainButtonPanel [drawExerciseButton; clearButton;helpButton; garbageCollectButton; closeButton]

    let mainPanel = new StackPanel(Orientation = Orientation.Vertical)

    let (mainControls:Panel list) = [mainButtonPanel; canvas]
    addControlsToPanel mainPanel mainControls

    //let textBlock = TextBlock(Text="TEST")
    //textBlock.FontSize <- 50.
    //textBlock.HorizontalAlignment <- HorizontalAlignment.Stretch
    //textBlock.VerticalAlignment <- VerticalAlignment.Center

    //Canvas.SetLeft(textBlock, 100.)
    //Canvas.SetTop(textBlock, 100.)
    //canvas.Children.Add(textBlock) |> ignore
    
    //let grid = Grid()
    //grid.Children.Add(textBlock) |> ignore
    //canvas.Children.Add(grid) |> ignore

    window.Content <- mainPanel

    //return
    window