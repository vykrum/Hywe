module Hywe.Main

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open Bridge
open Page
open NodeCode
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

// --- Polygon export consumer  ---
let mutable private latestOuterStr   : string = ""
let mutable private latestIslandsStr : string = ""
let mutable private latestAbsStr     : string = "1"
let mutable private latestEntryStr   : string = "0,0"
let mutable private latestWidth    : int = 40
let mutable private latestHeight   : int = 40
let mutable private latestPublished  : bool = false

// Keep PolygonEditor state in sync
let private syncPolygonState (p: PolygonEditorModel) =
    let outer, islands, absolute, entry, w, h = PolygonEditor.exportPolygonStrings p
    let w', h',entry', outer',islands' =
        if not p.UseBoundary then
            0, 0 ,"0,0","",""  
        else
            w, h, entry, outer, islands
    latestOuterStr   <- outer'
    latestIslandsStr <- islands'
    latestAbsStr     <- absolute
    latestEntryStr   <- entry'
    latestWidth      <- w'
    latestHeight     <- h'
    latestPublished  <- true

// Default Input
let elv = 0
let initialTree = NodeCode.initModel beeyond
let initialSequence = allSqns.[11]
let initialOutput = NodeCode.getOutput 
                        initialTree 
                        initialSequence 
                        latestWidth 
                        latestHeight 
                        latestAbsStr 
                        latestEntryStr 
                        latestOuterStr 
                        latestIslandsStr
let initModel =
    {
        Sequence = initialSequence
        stx1 = initialOutput
        Tree = initialTree
        ParseError = false
        LastValidTree = initialTree
        Derived = deriveData initialOutput 0
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
            | _ -> NodeCode.getOutput 
                    model.Tree 
                    model.Sequence 
                    latestWidth 
                    latestHeight 
                    latestAbsStr 
                    latestEntryStr 
                    latestOuterStr 
                    latestIslandsStr
        let newModel = {
            model with
                stx1 = updatedStx1
                Derived = deriveData updatedStx1 elv
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
        let newOutput = NodeCode.getOutput 
                            updatedTree 
                            model.Sequence 
                            latestWidth 
                            latestHeight 
                            latestAbsStr 
                            latestEntryStr 
                            latestOuterStr 
                            latestIslandsStr
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
        syncPolygonState newPolygonModel
        { model with PolygonEditor = newPolygonModel }, Cmd.none

    | SetActiveTab tab ->
    { model with ActiveTab = tab }, Cmd.none

    | ToggleEditorMode ->
        match model.ActiveTab, model.LastEditorTab with
        // Code -> Node
        | Editor true, Editor _ ->
            let maybeSubModel =
                model.stx1
                |> CodeNode.preprocessCode
                |> fun processed ->
                    try Some (CodeNode.parseOutput processed)
                    with _ -> None
                |> Option.map (fun tree ->
                    let laidOut = NodeCode.layoutTree tree 0 (ref 100.0)
                    { Root = laidOut; HideInstructions = model.Tree.HideInstructions }
                )

            let newModel =
                maybeSubModel
                |> Option.map (fun subModel ->
                    let newOutput =
                        NodeCode.getOutput
                            subModel
                            model.Sequence
                            latestWidth
                            latestHeight
                            latestAbsStr
                            latestEntryStr
                            latestOuterStr
                            latestIslandsStr

                    { model with
                        Tree = subModel
                        stx1 = newOutput
                        LastValidTree = subModel
                        ActiveTab = Editor false
                        LastEditorTab = Editor false
                        ParseError = false
                    }
                )
                |> Option.defaultValue
                    { model with
                        Tree = model.LastValidTree
                        ParseError = true
                    }

            newModel, Cmd.none

        // Node -> Code
        | Editor false, Editor _ ->
            let newOutput =
                NodeCode.getOutput
                    model.Tree
                    model.Sequence
                    latestWidth
                    latestHeight
                    latestAbsStr
                    latestEntryStr
                    latestOuterStr
                    latestIslandsStr

            { model with
                stx1 = newOutput
                LastValidTree = model.Tree
                ActiveTab = Editor true
                LastEditorTab = Editor true
                ParseError = false
            }, Cmd.none

        // Editor tab but last editor tab is Boundary (fallback)
        | Editor _, Boundary ->
            // Treat same as Node -> Code fallback
            let newOutput =
                NodeCode.getOutput
                    model.Tree
                    model.Sequence
                    latestWidth
                    latestHeight
                    latestAbsStr
                    latestEntryStr
                    latestOuterStr
                    latestIslandsStr

            { model with
                stx1 = newOutput
                LastValidTree = model.Tree
                ActiveTab = Editor true
                LastEditorTab = Editor true
                ParseError = false
            }, Cmd.none

        // Boundary tab
        | Boundary, _ ->
            model, Cmd.none

    | ToggleBoundary ->
        match model.ActiveTab with
        | Boundary ->
            // Leaving Boundary → back to Editor
            let updatedStx1 = NodeCode.getOutput 
                                model.Tree 
                                model.Sequence 
                                latestWidth 
                                latestHeight 
                                latestAbsStr 
                                latestEntryStr 
                                latestOuterStr 
                                latestIslandsStr
            syncPolygonState model.PolygonEditor
            { 
                model with 
                    ActiveTab = model.LastEditorTab 
                    stx1 = updatedStx1
            }, Cmd.none

        | Editor _ ->
            // Enter Boundary, remember current editor
            let updatedStx1 = NodeCode.getOutput 
                                model.Tree 
                                model.Sequence 
                                latestWidth 
                                latestHeight 
                                latestAbsStr 
                                latestEntryStr 
                                latestOuterStr 
                                latestIslandsStr
            { 
                model with 
                    LastEditorTab = model.ActiveTab
                    ActiveTab = Boundary
                    stx1 = updatedStx1
            }, Cmd.none

// Interface
let view model dispatch (js: IJSRuntime) =
    let isCode =
        match model.LastEditorTab with
        | Editor adv -> adv
        | _ -> false

    let activeTabClass tab =
        match model.ActiveTab, tab with
        | Boundary, Boundary -> "hywe-toggle-btn active"
        | Editor _, Editor _ when model.ActiveTab <> Boundary -> "hywe-toggle-btn active"
        | _ -> "hywe-toggle-btn"

    let nodeCodeButtonText =
        match model.LastEditorTab with
        | Editor true -> "Refine"
        | _ -> "Define"

    concat {
        // --- Top buttons ---
        div {
            attr.style "display:flex; gap:5px; align-self:flex-start; padding-left:10px; padding-right:10px;"

            // Boundary toggle
            button {
                attr.``class`` (activeTabClass Boundary)
                on.click (fun _ -> dispatch ToggleBoundary)
                text "Confine"
            }

            // Node/Code toggle
            button {
                attr.``class`` (activeTabClass (Editor isCode))
                on.click (fun _ ->
                    match model.ActiveTab with
                    | Boundary -> dispatch ToggleBoundary
                    | Editor _ -> dispatch ToggleEditorMode
                )
                text nodeCodeButtonText
            }
        }

        // --- Active Panel ---
        match model.ActiveTab with
        | Boundary ->
            div {
                attr.id "hywe-polygon-editor"
                attr.style "width:100%; height:auto; margin:5px;"
                PolygonEditor.view model.PolygonEditor (PolygonEditorMsg >> dispatch) js
            }
        | Editor adv ->
            match adv with
            | true ->
                div {
                    attr.id "hywe-input-syntax"
                    attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 5px 10px;"
                    textarea {
                        attr.``class`` "hyweSyntax"
                        on.input (fun e -> dispatch (SetStx1 (unbox<string> e.Value)))
                        text model.stx1
                    }
                }
            | false ->
                div {
                    attr.id "hywe-input-interactive"
                    attr.style "width: 100%; display: flex; flex-direction: column; align-items: stretch; box-sizing: border-box; padding: 0 10px; gap: 5px;"

                    div {
                        attr.style "flex:1; overflow-y:hidden;"
                        viewTreeEditor model.Tree (TreeMsg >> dispatch)
                    }

                    div {
                        attr.id "hywe-sequence-selector"
                        attr.style "width:auto; max-width:100%; margin-top:5px;"
                        sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                    }
                }

        // --- Hyweave, SVG, Table ---
        match model.ActiveTab with
        | Boundary -> ()
        | Editor _ ->
            div {
                attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px;"
                concat {
                    button {
                        attr.id "hywe-hyweave"
                        attr.``class`` "hyWeaveButton"
                        attr.disabled model.IsHyweaving
                        on.click (fun _ -> dispatch StartHyweave)
                        match model.IsHyweaving with
                        | true ->
                            span { attr.``class`` "spinner" }
                            text " h y W E A V E i n g . . ."
                        | false -> text "h y W E A V E"
                    }

                    div {
                        attr.id "hywe-svg-container"
                        attr.``class`` "flex-container"
                        attr.style "flex-wrap:wrap; justify-content:center; max-width:100%; overflow-x:auto;"
                        svgCoxels model.Derived.cxCxl1 model.Derived.cxOuIl elv model.Derived.cxClr1 10
                    }

                    div {
                        attr.style "width:95%; max-width:1200px; align-items: center; justify-content:center; margin:auto; aspect-ratio: 3/2; background:transparent;"
                        canvas {
                            attr.id "hywe-extruded-polygon"
                            attr.style "width:95%; align-items: center; justify-content:center; height:100%; display:block; border:none;"
                        }
                        async {
                            do! extrudePolygons js "hywe-extruded-polygon" model.Derived.cxCxl1 model.Derived.cxClr1 3.0 0
                        }
                        |> Async.StartImmediate
                    }
                }

                div {
                    attr.id "hywe-table-wrapper"
                    attr.style "width: 100vw; margin-top: 5px; margin-left: calc(-20px); margin-right: calc(-20px); box-sizing: border-box;"
                    viewHyweTable model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxlAvl
                }
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
