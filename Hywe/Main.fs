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

/// <summary> Specifies the currently visible configuration panel. </summary>
type ActivePanel =
    | EditorPanel
    | BoundaryPanel

/// <summary> Specifies the input methodology - flowchart or text </summary>
type EditorMode =
    | Interactive
    | Syntax

/// <summary> Central application state for the interface. </summary>
type Model =
    {
        Sequence: string
        stx1 : string
        Tree : SubModel
        LastValidTree: SubModel
        ParseError: bool
        Derived : DerivedData
        IsHyweaving: bool
        PolygonEditor: EditorState
        ActivePanel: ActivePanel
        EditorMode: EditorMode
    }

/// <summary> Messages representing all possible state changes in the main module. </summary>
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
    | SaveRequested
    | ImportRequested
    | FileImported of string

// Polygon export consumer
let mutable private latestOuterStr   : string = ""
let mutable private latestIslandsStr : string = ""
let mutable private latestAbsStr     : string = "1"
let mutable private latestEntryStr   : string = "5,5"
let mutable private latestWidth    : int = 30
let mutable private latestHeight   : int = 30
let mutable private latestPublished  : bool = false

/// <summary> Synchronizes the PolygonEditor state with the local export cache. </summary>
/// <param name="p"> The current PolygonEditorModel. </param>
let private syncPolygonState 
    (p: PolygonEditorModel) =
    let outer, islands, absolute, entry, w, h = PolygonEditor.exportPolygonStrings p
    
    let w', h', entry', outer', islands' =
        match p.UseBoundary with
        | false -> 0, 0, "0,0", "", ""
        | true  -> w, h, entry, outer, islands
        
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
        PolygonEditor = Stable PolygonEditor.initModel
        ActivePanel = EditorPanel
        EditorMode = Interactive
    }

// Update
/// <summary> Processes incoming messages to produce a new model and optional commands. </summary>
/// <param name="js"> The JavaScript runtime for interop. </param>
/// <param name="message"> The message to handle. </param>
/// <param name="model"> The current state. </param>
/// <returns> The updated model and Elmish command. </returns>
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
            | Syntax ->
                let inner = 
                    match model.PolygonEditor with 
                    | Stable m | FreshlyImported m -> m

                let newState = Parse.importFromHyw model.stx1 inner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                syncPolygonState finalPoly
                model.stx1
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

        // Trigger the Shadow Save to browser memory immediately
        Storage.autoSave js updatedStx1 |> ignore

        let newModel =
            match model.EditorMode with
            | Syntax ->
                let inner = 
                    match model.PolygonEditor with 
                    | Stable m | FreshlyImported m -> m

                let newState = Parse.importFromHyw updatedStx1 inner

                { model with 
                    stx1 = updatedStx1
                    Derived = deriveData updatedStx1 elv
                    PolygonEditor = newState }

            | Interactive ->
                { model with 
                    stx1 = updatedStx1
                    Derived = deriveData updatedStx1 elv }
                
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
        let currentInnerModel = 
            match model.PolygonEditor with
            | Stable m | FreshlyImported m -> m

        model,
        Cmd.OfAsync.perform
            (PolygonEditor.update js subMsg)
            currentInnerModel
            PolygonEditorUpdated

    | PolygonEditorUpdated newModel ->
        syncPolygonState newModel
        { model with PolygonEditor = Stable newModel }, Cmd.none

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
        let updatedStx1 = NodeCode.getOutput 
                            model.Tree model.Sequence 
                            latestWidth latestHeight latestAbsStr 
                            latestEntryStr latestOuterStr latestIslandsStr

        match model.ActivePanel with
        | BoundaryPanel ->
            { model with ActivePanel = EditorPanel; stx1 = updatedStx1 }, Cmd.none

        | EditorPanel ->
            let currentInner = 
                match model.PolygonEditor with 
                | Stable m | FreshlyImported m -> m
        
            // 1. Parse the text into a new model
            let updatedEditorState = Parse.importFromHyw updatedStx1 currentInner
        
            // 2. Extract the record to sync with JS/Canvas
            let finalPoly = 
                match updatedEditorState with 
                | FreshlyImported m | Stable m -> m

            { model with 
                ActivePanel = BoundaryPanel
                stx1 = updatedStx1 
                // 3. MUST ASSIGN THE NEW STATE HERE
                PolygonEditor = updatedEditorState 
            }, 
            Cmd.ofMsg (PolygonEditorUpdated finalPoly)
    
    | SaveRequested ->
        // 1. Save to Downloads with timestamp
        Storage.saveFile js model.stx1 |> ignore
        // 2. Also update Shadow Save so the session is current
        Storage.autoSave js model.stx1 |> ignore
        model, Cmd.none

    | ImportRequested ->
        let doClick () =
            task {
                do! js.InvokeVoidAsync("clickElement", "hyw-import-hidden").AsTask()
            }
        model, Cmd.OfTask.perform doClick () (fun _ -> FinishHyweave)

    | FileImported content ->
        match System.String.IsNullOrWhiteSpace content with
        | true -> model, Cmd.none
        | false ->
            let clean = content.Trim()
            
            // 1. Sync Node Tree (Reflects in "Node" view)
            let newTree = 
                clean 
                |> CodeNode.preprocessCode 
                |> fun processed ->
                    try 
                        let tree = CodeNode.parseOutput processed
                        let laidOut = NodeCode.layoutTree tree 0 (ref 100.0)
                        { Root = laidOut; HideInstructions = model.Tree.HideInstructions }
                    with _ -> model.Tree // Fallback to current if parse fails

            // 2. Sync Polygon Editor (Reflects in "Boundary" view)
            let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            let newState = Parse.importFromHyw clean inner
            let finalPoly = match newState with FreshlyImported m | Stable m -> m
            
            // 3. Sync Global Mutables (Reflects in 3D Canvas)
            syncPolygonState finalPoly

            // 4. Update the Model
            { model with 
                stx1 = clean
                Tree = newTree
                LastValidTree = newTree
                Derived = deriveData clean elv 
                PolygonEditor = newState 
                ParseError = false
            }, 
            Cmd.batch [
                // Force UI components to notice the update
                Cmd.ofMsg (PolygonEditorUpdated finalPoly)
                
                Cmd.OfTask.attempt (fun () -> task { 
                    do! js.InvokeVoidAsync("localStorageSet", "hywe_backup", clean).AsTask() 
                }) () (fun _ -> FinishHyweave)
            ]

// View helpers
/// <summary> Renders the toggle button for switching between Node and Code views. </summary>
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

/// <summary> Renders the button to toggle boundary parameter modification. </summary>
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

/// <summary> Renders the input section based on the current EditorMode. </summary>
let private viewEditorPanel (model: Model) (dispatch: Message -> unit) =
    match model.EditorMode with
    | Syntax ->
        div {
            attr.id "hywe-input-syntax"
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 5px 10px;"
            textarea {
                attr.``class`` "hyweSyntax"
                attr.key (model.stx1.GetHashCode().ToString())
                attr.value model.stx1 
                on.change (fun e -> dispatch (SetStx1 (unbox<string> e.Value)))
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

/// <summary> Renders the primary action button for starting the weaving process. </summary>
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

/// <summary> Renders the primary action button for starting the weaving process. </summary>
let private viewHywePanel (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    match model.ActivePanel with
    | BoundaryPanel ->
        // Pattern match on the state to apply the refresh key
        match model.PolygonEditor with
        | FreshlyImported inner ->
            div {
                attr.id "hywe-polygon-editor"
                attr.key "fresh-sync" 
                PolygonEditor.view inner (PolygonEditorMsg >> dispatch) js
            }
        | Stable inner ->
            div {
                attr.id "hywe-polygon-editor"
                attr.key "stable-ui"
                PolygonEditor.view inner (PolygonEditorMsg >> dispatch) js
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

/// <summary> Renders the Import/Export toolbar. </summary>
let private viewPersistenceToolbar (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    div {
        attr.style "display:flex; gap:10px; padding: 10px; margin-bottom: 10px; justify-content: right;"
        
        button {
            attr.``class`` "hywe-toggle-btn"
            on.click (fun _ -> dispatch SaveRequested)
            text "Save"
        }

        button {
            attr.``class`` "hywe-toggle-btn"
            on.click (fun _ -> dispatch ImportRequested)
            text "Load"
        }

        input {
            attr.id "hyw-import-hidden"
            attr.``type`` "file"
            attr.style "display:none"
            attr.accept ".hyw"
            on.change (fun e ->
                async {
                    let! content = js.InvokeAsync<string>("readHywFile", "hyw-import-hidden").AsTask() |> Async.AwaitTask
                    dispatch (FileImported content)
                } |> Async.StartImmediate
            )
        }
    }

/// <summary> The main view composition. </summary>
let view model dispatch (js: IJSRuntime) =
    concat {
        // Storage Toolbar
        viewPersistenceToolbar model dispatch js
        
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

/// <summary> Bolero Component entry point for the Hywe application. </summary>
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.none)
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> view model dispatch this.JSRuntime)