module MusWindow

open System.Windows
open System.Windows.Controls

open MusicBase
open Engraver
open Drawable
open EventID

/// Helper func to make adding elements easier.
let addControlsToPanel (panel:#Panel) controlList = 
    (List.map(fun b -> panel.Children.Add b |> ignore) controlList) 
    |> ignore

//-----------------------------------TESTS---------------------------------------
let test (engraver:Engraver) =
    engraver.clearCanvas()

    let e0 = createIndpEvent Treble defaultEventID
    let e1 = defaultTimeSigEvent
    let e2 = createIndpEvent defaultPitch defaultEventID
    let e3 = createIndpEvent {defaultPitch with note = Note.D} defaultEventID
    let e4 = createIndpEvent {defaultPitch with note = Note.A; octave = 3} defaultEventID
    
    let m1 = addMultipleEvents defaultMeasure [e0;e1;e2;e3;e4]
    let dMeasure1 = createDrawableMeasure Treble m1 100. 100. MusResources.measureWidthDefault MusResources.measureHeightDefault
    engraver.engraveMeasureAndEvents dMeasure1
//-------------------------------------------------------------------------------

let makeWindow width height =
    let window = 
        new Window(
            Width = width, 
            Height = height,
            Title = "MusWindow Test",
            Icon = (DrawUtils.loadImageFile ImageLocations.flatImageLocation),
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
        Basic.log "clickEvent_testCanvas clicked"
        test engraver

    let drawExerciseButton = createButton "Draw Exercise" (clickEvent_testCanvas) "Genderate and Draw new reading exercise."
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

    let mainButtonPanel = new StackPanel(Orientation = Orientation.Horizontal)
    addControlsToPanel mainButtonPanel [drawExerciseButton; clearButton;helpButton; closeButton]

    let mainPanel = new StackPanel(Orientation = Orientation.Vertical)

    let (mainControls:Panel list) = [mainButtonPanel; canvas]
    addControlsToPanel mainPanel mainControls

    window.Content <- mainPanel

    //return
    window