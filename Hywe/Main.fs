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
open Boundary

type Model =
    {
        opt1 : Beeset option
        stx1 : string
        stx2 : string
        Tree : SubModel
        Derived : DerivedData
        IsHyweaving: bool
        PolygonEditor: PolygonEditorModel
        //boundary: BoundaryModel
    }

type Message =
    | SetOpt1 of Beeset
    | SetStx1 of string
    | TreeMsg of SubMsg
    | StartHyweave
    | RunHyweave
    | FinishHyweave
    | PolygonEditorMsg of PolygonEditorMessage
    | PolygonEditorUpdated of PolygonEditorModel
    //| BoundaryMsg of BoundaryMsg

// Default Input
let initModel =
    {
        opt1 = None
        stx1 = stxInstr 
        stx2 = stx2Ini
        Tree = NodeCode.initModel ()
        Derived = deriveData stx2Ini
        IsHyweaving = false
        PolygonEditor = Boundary.initModel
        //boundary = Boundary.initModel
    }

let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetOpt1 value -> 
        let newStx =
            match value with
            | Beewhich -> stxInstr
            | Beegin -> NodeCode.getOutput model.Tree
            | Beespoke -> beedroom

        let newModel = {
            model with
                opt1 = Some value
                stx1 = newStx
                stx2 = newStx
        }

        // Reset Hyweave if Bee-which is selected
        let finalModel =
            match value with
            | Beewhich -> { newModel with IsHyweaving = false }
            | _ -> newModel

        finalModel, Cmd.none

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
        let updatedStx1 =
            match model.opt1 with
            | Some Beegin -> NodeCode.getOutput model.Tree
            | _ -> model.stx1

        let newModel = {
            model with
                stx1 = updatedStx1
                stx2 = updatedStx1
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
        match model.opt1 with
        | Some Beegin ->
            let newOutput = NodeCode.getOutput updatedTree
            {
                model with 
                    Tree = updatedTree
                    stx1 = newOutput
                    stx2 = newOutput 
            }, Cmd.none
        | _ ->
            { model with Tree = updatedTree }, Cmd.none

    | PolygonEditorMsg subMsg ->
        model, Cmd.OfAsync.perform (fun () -> Boundary.update js subMsg model.PolygonEditor) () PolygonEditorUpdated

    | PolygonEditorUpdated newPEModel ->
        { model with PolygonEditor = newPEModel }, Cmd.none


(*    | BoundaryMsg subMsg ->
        let updatedBoundary = Boundary.update subMsg model.boundary
        { model with boundary = updatedBoundary }, Cmd.none*)
// Interface
let view model dispatch (js: IJSRuntime) =      
    concat {
        // Nested Coxels Data
        let cxCxl1 = model.Derived.cxCxl1
        let cxlAvl = model.Derived.cxlAvl
        let cxClr1 = model.Derived.cxClr1
        //div{pageTitle}
        //div{pageIntro}
        // Hywe
        div{
            attr.``style`` "flex-wrap: wrap;justify-content: center;display: flex;flex-direction: row;"
            // Dropdown
            label {
                attr.``for`` "options"
            }
            beeSelect model.opt1 (fun beeset -> dispatch (SetOpt1 beeset))

            // Formatting Instructions
            a {
                attr.style "padding: 17px 5px;"
                attr.``class`` "button3"
                attr.href "https://github.com/vykrum/Hywe/wiki/Hywe-Syntax"
                attr.target "blank"
                "?"
            }

            // Space Flow Chart
            div {
                attr.style "width: 100%; flex-basis: 100%; margin-top: 10px;"

                match model.opt1 with
                | Some Beegin ->
                    div {
                        attr.``style`` "width: 95%; margin: auto; padding: 10px;"
                        viewTreeEditor model.Tree (TreeMsg >> dispatch)
                    }
                | _ -> empty()
            }

            // Hywe Syntax Input
            textarea {
                attr.id "syntax"
                attr.``class`` "textarea"
                attr.``style`` $"width : 95%%;
                                margin-left: 20px;
                                margin-right: 20px;
                                height:50px;
                                font-size: 12px;
                                color: #808080;"

                match model.opt1 with
                | Some Beegin ->
                    attr.readonly true
                    text (NodeCode.getOutput model.Tree)
                | _ ->
                    bind.change.string model.stx1 (fun a -> dispatch (SetStx1 a))
                    text model.stx1
            }

            // Hyweave
            button {
                let hyweaveDisabled =
                    model.IsHyweaving || model.opt1 = None || model.opt1 = Some Beewhich
                attr.``class`` "button1"
                attr.disabled hyweaveDisabled
                attr.``style`` "
                    width: 88%;
                    margin-left: 1%;
                    margin-right: 1%;
                    margin-top: 5px;"
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
                attr.``style`` "flex-wrap: wrap; justify-content: center;"

                // SVG content
                nstdCxlsWrp cxCxl1 cxClr1 10
            }

            // Hywe Table — full width
            div {
                attr.id "hywe-table"
                attr.``style`` "width: 100%; margin-top: 20px;"

                viewHyweTable cxCxl1 cxClr1 cxlAvl
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

            }*)

(*            div{
                viewCodeGraphFromString model.stx2
            }*)  
        }
    } 
    
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


