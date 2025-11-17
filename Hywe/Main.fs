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

// Types
type ActivePanel =
    | EditorPanel
    | BoundaryPanel

type EditorMode =
    | Interactive
    | Syntax

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
        ActivePanel: ActivePanel
        EditorMode: EditorMode
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
    | SetActivePanel of ActivePanel
    | ToggleEditorMode
    | ToggleBoundary

// Polygon export consumer
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
    let w', h', entry', outer', islands' =
        if not p.UseBoundary then
            0, 0, "0,0", "", ""
        else
            w, h, entry, outer, islands
    latestOuterStr   <- outer'
    latestIslandsStr <- islands'
    latestAbsStr     <- absolute
    latestEntryStr   <- entry'
    latestWidth      <- w'
    latestHeight     <- h'
    latestPublished  <- true

// Defaults / init 
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
        ActivePanel = EditorPanel
        EditorMode = Interactive
    }

// Update
let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetSqnIndex i ->
        { model with Sequence = indexToSqn i }, Cmd.none

    | SetStx1 value ->
        { model with stx1 = value }, Cmd.none

    | StartHyweave ->
        let model2 = { model with ActivePanel = EditorPanel; IsHyweaving = true }
        model2,
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 50
                return ()
            }) () (fun _ -> RunHyweave)

    | RunHyweave ->
        let updatedStx1 =
            match model.EditorMode with
            | Syntax -> model.stx1
            | Interactive ->
                NodeCode.getOutput
                    model.Tree
                    model.Sequence
                    latestWidth
                    latestHeight
                    latestAbsStr
                    latestEntryStr
                    latestOuterStr
                    latestIslandsStr

        let newModel = { model with stx1 = updatedStx1; Derived = deriveData updatedStx1 elv }
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
        { model with Tree = updatedTree; stx1 = newOutput }, Cmd.none

    | PolygonEditorMsg subMsg ->
        model,
        Cmd.OfAsync.perform
            (PolygonEditor.update js subMsg)
            model.PolygonEditor
            PolygonEditorUpdated

    | PolygonEditorUpdated newPolygonModel ->
        syncPolygonState newPolygonModel
        { model with PolygonEditor = newPolygonModel }, Cmd.none

    | SetActivePanel panel ->
        { model with ActivePanel = panel }, Cmd.none

    | ToggleEditorMode ->
        match model.EditorMode with
        | Syntax ->
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

            match maybeSubModel with
            | Some subModel ->
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
                    EditorMode = Interactive
                    ParseError = false
                }, Cmd.none

            | None ->
                { model with Tree = model.LastValidTree; ParseError = true }, Cmd.none

        | Interactive ->
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
                EditorMode = Syntax
                ParseError = false
            }, Cmd.none

    | ToggleBoundary ->
        match model.ActivePanel with
        | BoundaryPanel ->
            // Leaving Boundary -> go back to EditorPanel
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
            { model with ActivePanel = EditorPanel; stx1 = updatedStx1 }, Cmd.none

        | EditorPanel ->
            // Enter Boundary: remember nothing special - just switch panel
            let updatedStx1 = NodeCode.getOutput
                                model.Tree
                                model.Sequence
                                latestWidth
                                latestHeight
                                latestAbsStr
                                latestEntryStr
                                latestOuterStr
                                latestIslandsStr
            { model with ActivePanel = BoundaryPanel; stx1 = updatedStx1 }, Cmd.none

// View helpers
let private viewNodeCodeButton (model: Model) (dispatch: Message -> unit) =
    let nodeCodeButtonText =
        match model.EditorMode with
        | Syntax -> "Node"
        | Interactive -> "Code"

    div {
        attr.style "display:flex; gap:5px; padding-left:10px; padding-right:10px;"
        button {
            attr.``class`` "hywe-toggle-btn"
            attr.style "margin-left:auto;"
            on.click (fun _ -> dispatch ToggleEditorMode)
            text nodeCodeButtonText
        }
    }

let private viewBoundaryOutputButton (model: Model) (dispatch: Message -> unit) =
    let buttonText =
        match model.ActivePanel with
        | EditorPanel -> "Modify Boundary Parameters"
        | BoundaryPanel -> "View Spatial Configuration"

    div {
        attr.style "display:flex; gap:5px; justify-content:center; padding-left:10px; padding-right:10px;"
        button {
            attr.``class`` "hywe-toggle-btn"
            on.click (fun _ -> dispatch ToggleBoundary)
            text buttonText
        }
    }


let private viewEditorPanel (model: Model) (dispatch: Message -> unit) =
    match model.EditorMode with
    | Syntax ->
        div {
            attr.id "hywe-input-syntax"
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 5px 10px;"
            textarea {
                attr.``class`` "hyweSyntax"
                on.input (fun e -> dispatch (SetStx1 (unbox<string> e.Value)))
                text model.stx1
            }
        }
    | Interactive ->
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

let private viewHyweButton (model: Model) (dispatch: Message -> unit) =
    div {
        attr.style "width: 100%; display:flex; justify-content:center; box-sizing:border-box; padding: 8px 10px;"
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
    }

let private viewHywePanel (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    match model.ActivePanel with
    | BoundaryPanel ->
        div {
            attr.id "hywe-polygon-editor"
            attr.style "width:100%; height:auto; margin:5px;"
            PolygonEditor.view model.PolygonEditor (PolygonEditorMsg >> dispatch) js
        }

    | EditorPanel ->
        div {
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px;"
            concat {
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
                    } |> Async.StartImmediate
                }
            }

            div {
                attr.id "hywe-table-wrapper"
                attr.style "width: 100vw; margin-top: 5px; margin-left: calc(-20px); margin-right: calc(-20px); box-sizing: border-box;"
                viewHyweTable model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxlAvl
            }
        }

// Main view
let view model dispatch (js: IJSRuntime) =
    concat {
        // Node Code button
        viewNodeCodeButton model dispatch

        // Editor Panel
        viewEditorPanel model dispatch

        // Hywe button
        viewHyweButton model dispatch

        // Boundary Toggle button
        viewBoundaryOutputButton model dispatch

        // Active Panel
        viewHywePanel model dispatch js
    }

// Bolero wiring
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.none)
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> view model dispatch this.JSRuntime)
