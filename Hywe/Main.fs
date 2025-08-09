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
        LastValidTree: SubModel
        ParseError: bool
        Derived : DerivedData
        IsHyweaving: bool
        PolygonEditor: PolygonEditorModel
        ActiveTab: EditorTab
        LastEditorTab: EditorTab
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
    | SetActiveTab of EditorTab
    | ToggleEditorMode
    | ToggleBoundary

// Default Input
let initialTree = NodeCode.initModel beeyond
let initialSequence = allSqns.[11]
let initialOutput = NodeCode.getOutput initialTree initialSequence
let initModel =
    {
        Sequence = initialSequence
        stx1 = initialOutput
        Tree = initialTree
        ParseError = false
        LastValidTree = initialTree
        Derived = deriveData initialOutput
        IsHyweaving = false
        PolygonEditor = PolygonEditor.initModel 
        ActiveTab = Editor false
        LastEditorTab = Editor false
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
        let updatedStx1 =
            match model.ActiveTab with
            | Editor true -> model.stx1 
            | _ -> NodeCode.getOutput model.Tree model.Sequence
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
        model,
        Cmd.OfAsync.perform
            (PolygonEditor.update js subMsg)
            model.PolygonEditor
            PolygonEditorUpdated

    | PolygonEditorUpdated newPolygonModel ->
        { model with PolygonEditor = newPolygonModel }, Cmd.none

    | SetActiveTab tab ->
    { model with ActiveTab = tab }, Cmd.none

    | ToggleEditorMode ->
        match model.ActiveTab with
        | Editor adv ->
            if adv then
                // Going from Code -> Node
                try
                    let processed = CodeNode.preprocessCode model.stx1
                    let tree = CodeNode.parseOutput processed
                    let laidOut = NodeCode.layoutTree tree 0 (ref 100.0)
                    let subModel = { Root = laidOut; HideInstructions=model.Tree.HideInstructions }
                    let newOutput = NodeCode.getOutput subModel model.Sequence
                    {
                        model with
                            Tree = subModel
                            stx1 = newOutput
                            LastValidTree = subModel
                            ActiveTab = Editor false
                            LastEditorTab = Editor false
                            ParseError = false
                    }, Cmd.none
                                with _ ->
                                    // Parsing failed
                                    {
                                        model with
                                            Tree = model.LastValidTree
                                            ParseError = true
                                    }, Cmd.none
            else
                // Node -> Code
                {
                    model with
                        stx1 = NodeCode.getOutput model.Tree model.Sequence
                        LastValidTree = model.Tree
                        ActiveTab = Editor true
                        LastEditorTab = Editor true
                        ParseError = false
                }, Cmd.none

        | Boundary ->
            // Exit Boundary, toggle last editor mode
            let toggled =
                match model.LastEditorTab with
                | Editor adv -> Editor (not adv)
                | _ -> Editor false
            { model with ActiveTab = toggled; LastEditorTab = toggled }, Cmd.none

    | ToggleBoundary ->
        match model.ActiveTab with
        | Boundary ->
            // Return to last editor
            { model with ActiveTab = model.LastEditorTab }, Cmd.none
        | Editor _ ->
            // Enter boundary, remember current editor
            { model with LastEditorTab = model.ActiveTab; ActiveTab = Boundary }, Cmd.none

// Interface
let view model dispatch (js: IJSRuntime) =      
    // Nested Coxels Data
    concat {
        // --- Top buttons ---
        div {
            attr.style "display:flex; gap:5px; align-self:flex-start; padding-left:10px; padding-right:10px;"

            let isCode =
                match model.LastEditorTab with
                | Editor adv -> adv
                | _ -> false

            // --- Boundary toggle ---
            button {
                attr.``class`` ("hywe-toggle-btn" + if model.ActiveTab = Boundary then " active" else "")
                on.click (fun _ -> dispatch ToggleBoundary)
                text "Bind"
            }

            // --- Node/Code toggle ---
            button {
                attr.``class`` ("hywe-toggle-btn" + if model.ActiveTab <> Boundary then " active" else "")
                on.click (fun _ ->
                    if model.ActiveTab = Boundary then
                        // Exit boundary, keep last Node/Code
                        dispatch ToggleBoundary
                    else
                        // Toggle Node <-> Code
                        dispatch ToggleEditorMode
                )
                text (if isCode then "Code" else "Node")
            }
        }

        // --- Active Panel ---
        match model.ActiveTab with
        | Boundary ->
            div {
                attr.id "hywe-polygon-editor"
                attr.style "width:100%; height:auto; margin:5px;"
                PolygonEditor.view  model.PolygonEditor (PolygonEditorMsg >> dispatch) js
            }

        | Editor adv ->
            if adv then
                div {
                    attr.id "hywe-input-syntax"
                    attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 5px 10px;"
                    // Syntax Editor
                    textarea {
                        attr.``class`` "hyweSyntax"
                        on.input (fun e -> dispatch (SetStx1 (unbox<string> e.Value)))
                        text model.stx1
                    }
                }
            else
                div {
                    attr.id "hywe-input-interactive"
                    attr.style "width: 100%; display: flex; flex-direction: column; align-items: stretch; box-sizing: border-box; padding: 0 10px; gap: 5px;"

                    // Tree editor
                    div {
                        attr.style "flex:1; overflow-y:hidden;"
                        viewTreeEditor model.Tree (TreeMsg >> dispatch)
                    }

                    // Sequence selector
                    div {
                        attr.id "hywe-sequence-selector"
                        attr.style "width:auto; max-width:100%; margin-top:5px;"
                        sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                    }
                }

        // --- Hyweave, SVG, Table ---
        if model.ActiveTab <> Boundary then
            div{
                attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px;"
                concat {
                // Hyweave button
                button {
                    let hyweaveDisabled = model.IsHyweaving
                    attr.id "hywe-hyweave"
                    attr.``class`` "hyWeaveButton"
                    attr.disabled hyweaveDisabled
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
                    nstdCxlsWrp model.Derived.cxCxl1 model.Derived.cxClr1 10
                }

                // Hywe Table
                div {
                    attr.id "hywe-table-wrapper"
                    attr.style "width: 100vw; margin-top: 5px; margin-left: calc(-20px); margin-right: calc(-20px); box-sizing: border-box;"
                    viewHyweTable model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxlAvl
                }
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