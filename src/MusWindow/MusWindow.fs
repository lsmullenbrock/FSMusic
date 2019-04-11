module MusWindow

open System.Windows
open System.Windows.Controls

open MusicTypes
open Engraver


/// Helper func to make adding elements easier.
let addControlsToPanel (panel:#Panel) controlList = 
    (List.map(fun b -> panel.Children.Add b |> ignore) controlList) 
    |> ignore

//-----------------------------------TESTS---------------------------------------
let test (engraver:Engraver) =
    engraver.clearCanvas()

    let e0 = createIndpEvent Treble
    let e1 = defaultTimeSigEvent
    let e2 = createIndpEvent {defaultPitch with alteration = Some Alteration.Sharp}
    let e3 = createIndpEvent {defaultPitch with note = Note.B; octave= 3; alteration = Some Alteration.Flat}
    let e4 = createIndpEvent {defaultPitch with note = Note.A; octave = 3; alteration = Some Alteration.Natural}

    let e5 = createIndpEvent Bass
    let e6 = {numerator = 3; denominator = Value.Quarter} |> createIndpEvent
    let e7 = createIndpEvent {defaultPitch with note = Note.E; octave = 3; alteration = Some Alteration.Flat}
    
    let m1 = addMultipleEvents defaultMeasure [e0;e1;e2;e3;e4]
    let m2 = addMultipleEvents defaultMeasure [e5;e6;e7]

    let staff1 = DrawStaff.createVerifiedDrawableStaff [m1;m2;m1;m2] MusResources.leftPadding MusResources.topPadding MusResources.measureWidthDefault MusResources.measureHeightDefault

    //let dMeasure1 = createDrawableMeasure Treble m1 100. 100. MusResources.measureWidthDefault MusResources.measureHeightDefault
    engraver.engraveStaff staff1
//-------------------------------------------------------------------------------

let makeWindow width height =
    let window = 
        new Window(
            Width = width,
            Height = height,
            Title = "MusWindow Test",
            Icon = (DrawingUtils.loadImageFile GlyphLocations.flatImageLocation),
            WindowStartupLocation = WindowStartupLocation.CenterScreen
        )
    
    let canvas = new Canvas()
    let engraver = new Engraver(canvas)

    let buttonHeight = 20.
    let buttonWidth = width / 10.
    let buttonMargin = Thickness 5.
    
    let createButton text clickEvent toolTipText =
        let newButton = 
            new Button(
                Content = text,
                Height = buttonHeight,
                Width = buttonWidth,
                Margin = buttonMargin
            )
        newButton.Click.Add clickEvent
        if toolTipText <> "" then
            newButton.ToolTip <- toolTipText
        //return
        newButton

    let clickEvent_testCanvas _ =
        log "clickEvent_testCanvas clicked"
        test engraver

    let drawExerciseButton = createButton "Draw Exercise" (clickEvent_testCanvas) "Generate and Draw new exercise."
    drawExerciseButton.IsDefault <- true

    let clickEvent_clear _ = 
        engraver.clearCanvas()
    let clearButton = createButton "Clear Canvas" (clickEvent_clear) "Clear the Canvas."
    clearButton.IsCancel <- true

    let clickEvent_help _ = 
        MessageBox.Show("Click the Draw Exercise button to draw notes.")
        |> ignore
    let helpButton = createButton "Help" (clickEvent_help) "Click for a Help dialogue."
    
    let clickEvent_close _ = 
        window.Close()
    let closeButton = createButton "Close" (clickEvent_close) "Close this window."

    let clickEvent_GC _ =
        for _ in [1..10] do
            System.GC.WaitForPendingFinalizers()
            System.GC.Collect()
        log "GC.Collect() x 10"
    let garbageCollectButton = createButton "GC" (clickEvent_GC) "Force garbage collection."

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