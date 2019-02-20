module MusWindow

open System.Windows
open System.Windows.Controls

open DrawBase
open MusicBase
open Engraver
open Drawable
open System.Windows.Controls.Primitives
open System

//-----------------------------------TESTS---------------------------------------
let test (canvas:Canvas) =
    canvas.Children.Clear()

    let e1 = (Clef.Bass |> createEvent)
    let e2 = defaultTimeSigEvent
    let e3 = defaultPitchEvent
    let e4 = (Clef.Treble |> createEvent)
    let e5 = defaultPitchEvent
    let e6 = createEvent (createPitch Note.D None 5 Value.Quarter false false)
    let e7 = createEvent (createPitch Note.D None 3 Value.Quarter false false)
    
    let m1 = addMultipleEvents defaultMeasure [e1;e2;e7;e3;e4;e5;e6]
    let dMeasure1 = createDrawableMeasure m1 50. 50. MusResources.measureWidthDefault MusResources.measureHeightDefault
    engraveMeasureAndEvents canvas dMeasure1

//let test (canvas:Canvas) =
//    canvas.Children.Clear()
//    drawBassClef canvas 100. 100. MusResources.bassClefWidthDefault MusResources.bassClefHeightDefault
//    drawMeasure canvas 100. 100. MusResources.measureWidthDefault MusResources.measureHeightDefault
//    drawFilledNoteheadPitch canvas 200. 100. MusResources.filledNoteheadWidthDefault MusResources.filledNoteheadHeightDefault MusResources.stemLengthDefault
//    drawHalfNoteheadPitch canvas 200. 100. MusResources.filledNoteheadWidthDefault MusResources.filledNoteheadHeightDefault MusResources.stemLengthDefault
//    drawWholeNoteheadPitch canvas 200. 100. MusResources.wholeNoteheadWidthDefault MusResources.wholeNoteheadHeightDefault

//-------------------------------------------------------------------------------

/// Helper func to make adding elements easier.
let addControlsToPanel (panel:#Panel) controlList = 
    (List.map(fun b -> panel.Children.Add b |> ignore) controlList) 
    |> ignore


let makeWindow width height =
    let window = 
        new Window(
            Width = width, 
            Height = height,
            Title = "MusWindow Test",
            Icon = (loadImageFile ImageLocations.flatImageLocation)
        )

    let canvas = new Canvas()

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
        printfn "Button clicked."
        test canvas
    let drawExerciseButton = createButton "Draw Exercise" (clickEvent_testCanvas) "Genderate and Draw new reading exercise."
    drawExerciseButton.IsDefault <- true

    let clickEvent_clear _ = 
        canvas.Children.Clear()
    let clearButton = createButton "Clear Canvas" (clickEvent_clear) "Clear the Canvas."
    clearButton.IsCancel <- true

    let clickEvent_help _ = 
        MessageBox.Show("Hello")
        |> ignore
    let helpButton = createButton "Help" (clickEvent_help) "Click for a Help dialogue."
    
    let clickEvent_close _ = 
        window.Close()
    let closeButton = createButton "Close" (clickEvent_close) "Close this window."

    let mainButtonPanel = new StackPanel(Orientation = Orientation.Horizontal)
    addControlsToPanel mainButtonPanel [drawExerciseButton; clearButton;helpButton; closeButton]

    let mainPanel = new StackPanel(Orientation = Orientation.Vertical)

    let label = 
        new Label(
            Content = 0,
            Width = 50.,
            FlowDirection = FlowDirection.RightToLeft
        )

    let slider = 
        new Slider(
            Minimum = 0.,
            Maximum = 100.,
            Width = width / 5.,
            TickFrequency = 10.,
            Margin = buttonMargin,
            TickPlacement = TickPlacement.BottomRight,
            FlowDirection = FlowDirection.LeftToRight
        )

    slider.ValueChanged.Add(fun _ -> label.Content <- (int slider.Value))

    let sliderPanel = new StackPanel(Orientation = Orientation.Horizontal)
    let (sList:UIElement list) = [label; slider]
    addControlsToPanel sliderPanel sList

    let (mainControls:Panel list) = [sliderPanel; mainButtonPanel; canvas]
    addControlsToPanel mainPanel mainControls

    window.Content <- mainPanel

    //return
    window