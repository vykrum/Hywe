module Hywe.Main

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open Bridge
open Page
open NodeCode
open CodeNode
open PolygonEditor

type Model =
    {
        Sequence: string
        stx1 : string
        Tree : SubModel
        Derived : DerivedData
        IsHyweaving: bool
        PolygonEditor: PolygonEditorModel
        //boundary: BoundaryModel
    }

type Message =
    | SetSqnIndex of int
    | SetStx1 of string
    | TreeMsg of SubMsg
    | StartHyweave
    | RunHyweave
    | FinishHyweave
    | PolygonEditorMsg of PolygonEditorMessage
    | PolygonEditorUpdated of PolygonEditorModel
    //| BoundaryMsg of BoundaryMsg

// Default Input
let initialTree = NodeCode.initModel ()
let initialSequence = allSqns.[11]
let initialOutput = NodeCode.getOutput initialTree initialSequence
let initModel =
    {
        Sequence = initialSequence
        stx1 = initialOutput
        Tree = initialTree
        Derived = deriveData initialOutput
        IsHyweaving = false
        PolygonEditor = PolygonEditor.initModel
    }

let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetSqnIndex i ->
        { model with Sequence = indexToSqn i }, Cmd.none

    | SetStx1 value -> 
        { model with stx1 = value }, Cmd.none

    | StartHyweave ->
        { model with IsHyweaving = true },
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 50
                return ()
            }) () (fun _ -> RunHyweave)

    | RunHyweave ->
        let updatedStx1 = NodeCode.getOutput model.Tree model.Sequence
        let newModel = {
            model with
                stx1 = updatedStx1
                Derived = deriveData updatedStx1
        }
        newModel,
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 100
                return ()
            }) () (fun _ -> FinishHyweave)


    | FinishHyweave ->
        { model with IsHyweaving = false }, Cmd.none

    | TreeMsg subMsg ->
        let updatedTree = updateSub subMsg model.Tree
        let newOutput = NodeCode.getOutput updatedTree model.Sequence
        {
            model with 
                Tree = updatedTree
                stx1 = newOutput
        }, Cmd.none

    | PolygonEditorMsg subMsg ->
        model, Cmd.OfAsync.perform (fun () -> PolygonEditor.update js subMsg model.PolygonEditor) () PolygonEditorUpdated

    | PolygonEditorUpdated newPEModel ->
        { model with PolygonEditor = newPEModel }, Cmd.none

// Interface
let view model dispatch (js: IJSRuntime) =      
    // Nested Coxels Data
    let cxCxl1 = model.Derived.cxCxl1
    let cxlAvl = model.Derived.cxlAvl
    let cxClr1 = model.Derived.cxClr1
    concat {
        // Header
        //hyweHeader

        // Introduction
        //hyweIntro

        // Hywe
        div{
            // Parent container
            attr.style "display: flex;flex-direction: column;align-items: center;width: 100%;padding: 0 20px;box-sizing: border-box;"

            // Space Flow Chart
            div {
                attr.style "width:auto; max-width:100%; margin-top:5px; overflow-x:auto; text-align:center;"
                viewTreeEditor model.Tree (TreeMsg >> dispatch)
            }

            // Hywe Syntax Input
            textarea {
                attr.id "syntax"
                attr.``class`` "textarea"
                attr.style "width: 100%;height: 50px;font-size: 12px; color: #808080;box-sizing: border-box; margin-top: 5px;"
                attr.readonly true
                text (NodeCode.getOutput model.Tree model.Sequence)
            }

            // Sequence Selector
            div {
                attr.style "width:auto; max-width:100%; margin-top:8px;"
                sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
            }

            // Hyweave button
            button {
                let hyweaveDisabled = model.IsHyweaving
                attr.``class`` "button1"
                attr.disabled hyweaveDisabled
                attr.style "width: 100%; margin-top: 0px;"
                on.click (fun _ -> dispatch StartHyweave)

                match model.IsHyweaving with
                | true ->
                    span { attr.``class`` "spinner" }
                    text " h y W E A V E i n g . . ."
                | false ->
                    text "h y W E A V E"
            }

            // Hywe SVG
            div {
                attr.id "hywe-svg-container"
                attr.``class`` "flex-container"
                attr.style "flex-wrap:wrap; justify-content:center; max-width:100%; overflow-x:auto;"
                nstdCxlsWrp cxCxl1 cxClr1 10
            }

            // Hywe Table — full width
            div {
                attr.id "hywe-table-wrapper"
                attr.style "width: 100vw; margin-top: 5px; margin-left: calc(-20px); margin-right: calc(-20px); box-sizing: border-box;"
                viewHyweTable cxCxl1 cxClr1 cxlAvl
            }
            // Polygon Editor
            div{
                attr.``style`` "width: 100%; margin-top: 10px;"
                PolygonEditor.view model.PolygonEditor (PolygonEditorMsg >> dispatch)
            }

        }
    }

            (* div{
                attr.``style`` "width: 100%;"
                PolygonEditor.view model.PolygonEditor (PolygonEditorMsg >> dispatch) js

                //Boundary.view model.boundary (BoundaryMsg >> dispatch)
            }

           // Download Button — placed *after* table and centered
            div {
                attr.``style`` "width: 800px;"
                div {
                    attr.``style`` "width: 800px; flex-basis: 100%; margin-top: 10px;"

                    match model.opt1 with
                    | Some Beegin ->
                        div {
                            attr.``style`` "width: 95%; margin: auto; padding: 10px;"
                            viewTreeEditor model.Tree (TreeMsg >> dispatch)
                        }
                    | _ -> empty()
                }
                div {
                    attr.id "hywe-svg-container"
                    attr.``class`` "flex-container"
                    attr.``style`` "flex-wrap: wrap; justify-content: center;"

                    // SVG content
                    nstdCxlsWrp cxCxl1 cxClr1 model.scp1
                }
                div {
                    viewHyweTable cxCxl1 cxClr1 cxlAvl
                }
                button {
                    attr.``class`` "button3"
                    attr.``style`` "margin-top: 10px;"
                    on.click (fun _ -> js.InvokeVoidAsync("downloadCombinedSvg") |> ignore)
                    "Download Tree SVG"
                }

            }

           div{
                viewCodeGraphFromString model.stx1
            }*)  
 
// Bolero component handling state updates and rendering the user interface
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.none)
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> view model dispatch this.JSRuntime)


