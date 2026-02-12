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

/// <summary> Central application state for the interface. </summary>
type Model =
    {
        Sequence: string
        SrcOfTrth : string
        Tree : SubModel
        LastValidTree: SubModel
        ParseError: bool
        Derived : DerivedData
        IsHyweaving: bool
        PolygonEditor: EditorState
        ActivePanel: ActivePanel
        EditorMode: EditorMode
        BatchPreview: PreviewConfig[] option
        LastBatchSrc: string option
        SelectedPreviewIndex : int option
    }

/// <summary> Messages representing all possible state changes in the main module. </summary>
type Message =
    | SetSqnIndex of int
    | SetSrcOfTrth of string
    | TreeMsg of SubMsg
    | StartHyweave
    | RunHyweave
    | FinishHyweave
    | PolygonEditorMsg of PolygonEditorMessage
    | PolygonEditorUpdated of PolygonEditorModel
    | SetActivePanel of ActivePanel
    | SetBatchPreview of PreviewConfig[]
    | ToggleEditorMode
    | ToggleBoundary
    | ExportPdfRequested
    | TapPreview of int
    | CloseBatch
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
        SrcOfTrth = initialOutput
        Tree = initialTree
        ParseError = false
        LastValidTree = initialTree
        Derived = deriveData initialOutput 0
        IsHyweaving = false
        PolygonEditor = Stable PolygonEditor.initModel
        ActivePanel = LayoutPanel // Changed from EditorPanel
        EditorMode = Interactive
        BatchPreview = None
        LastBatchSrc = None
        SelectedPreviewIndex = None
    }
///

/// Update
/// <summary> Processes incoming messages to produce a new model and optional commands. </summary>
/// <param name="js"> The JavaScript runtime for interop. </param>
/// <param name="message"> The message to handle. </param>
/// <param name="model"> The current state. </param>
/// <returns> The updated model and Elmish command. </returns>
let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetSqnIndex i ->
        let newSqn = indexToSqn i
    
        // Determine the new Source of Truth based on the current mode
        let updatedSrc = 
            match model.EditorMode with
            | Interactive -> 
                // In Interactive mode, we must rebuild the string from the Tree + New Sequence
                NodeCode.getOutput 
                    model.Tree 
                    newSqn 
                    latestWidth 
                    latestHeight 
                    latestAbsStr 
                    latestEntryStr 
                    latestOuterStr 
                    latestIslandsStr
            | Syntax -> 
                // In Syntax mode, we just inject the new sequence into the existing string
                injectSqn model.SrcOfTrth newSqn

        // Update the model with the synchronized state
        let modelWithNewSqn = 
            { model with 
                Sequence = newSqn
                SrcOfTrth = updatedSrc
                IsHyweaving = true 
                SelectedPreviewIndex = None // Reset any tap-labels from the batch view
            }

        modelWithNewSqn,
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 5 
                return ()
            }) () (fun _ -> RunHyweave)

    | SetSrcOfTrth value ->
        { model with SrcOfTrth = value }, Cmd.none

    | StartHyweave ->
        // Changed to LayoutPanel
        let model2 = { model with ActivePanel = LayoutPanel; IsHyweaving = true }
        model2,
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 50
                return ()
            }) () (fun _ -> RunHyweave)

    | RunHyweave ->
        let updatedSrcOfTrth =
            match model.EditorMode with
            | Syntax ->
                let inner = 
                    match model.PolygonEditor with 
                    | Stable m | FreshlyImported m -> m

                let newState = Parse.importFromHyw model.SrcOfTrth inner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                syncPolygonState finalPoly
                model.SrcOfTrth
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
        Storage.autoSave js updatedSrcOfTrth |> ignore

        let newModel =
            match model.EditorMode with
            | Syntax ->
                let inner = 
                    match model.PolygonEditor with 
                    | Stable m | FreshlyImported m -> m

                let newState = Parse.importFromHyw updatedSrcOfTrth inner

                { model with 
                    SrcOfTrth = updatedSrcOfTrth
                    Derived = deriveData updatedSrcOfTrth elv
                    PolygonEditor = newState }

            | Interactive ->
                { model with 
                    SrcOfTrth = updatedSrcOfTrth
                    Derived = deriveData updatedSrcOfTrth elv }
                
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
        { model with Tree = updatedTree; SrcOfTrth = newOutput }, Cmd.none

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
        match panel with
        | BatchPanel ->
            // Compare current code to the code used for the last generated batch
            let isStale = Some model.SrcOfTrth <> model.LastBatchSrc
        
            match isStale with
            | true -> 
                // 1. Reset state: clear old previews, clear selection index, and start loader
                let model' = 
                    { model with 
                        ActivePanel = panel
                        IsHyweaving = true
                        BatchPreview = None 
                        SelectedPreviewIndex = None // Reset selection so old labels don't persist
                    }
            
                model', Cmd.OfAsync.perform (fun () ->
                    let rec computeBatch i acc = async {
                        match i >= 24 with
                        | true -> return acc
                        | false ->
                            let sqnStr = indexToSqn i
                            let forcedStr = injectSqn model.SrcOfTrth sqnStr
                            let cxls, _ = Parse.spaceCxl [||] forcedStr
                    
                            // Generate geometry for this specific variation
                            let d = getStaticGeometry cxls (deriveData forcedStr 0).cxClr1 0 10 
                    
                            let configData = 
                                {| sqnName = sqnStr; w = d.w; h = d.h
                                   shapes = d.shapes |> Array.map (fun s -> 
                                     {| color = s.color; points = s.points; name = s.name; lx = s.lx; ly = s.ly |}) 
                                |}
                        
                            // Yield control to UI thread to keep the spinner spinning
                            do! Async.Sleep 5
                            return! computeBatch (i + 1) (configData :: acc)
                    }
                    async {
                        let! results = computeBatch 0 []
                        // Reverse to maintain correct order (0 to 23)
                        return results |> List.toArray |> Array.rev
                    }) () SetBatchPreview

            | false -> 
                // Data is identical to the last run; just switch tabs
                { model with ActivePanel = panel }, Cmd.none
            
        | _ -> 
            { model with ActivePanel = panel }, Cmd.none

    | ToggleEditorMode ->
        match model.EditorMode with
        | Syntax ->
            let maybeSubModel =
                model.SrcOfTrth
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
                    SrcOfTrth = newOutput
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
                SrcOfTrth = newOutput
                LastValidTree = model.Tree
                EditorMode = Syntax
                ParseError = false
            }, Cmd.none

    | ToggleBoundary ->
        match model.ActivePanel with
        | BoundaryPanel -> 
            { model with ActivePanel = LayoutPanel }, Cmd.none
        | _ -> 
            // Logic for entering BoundaryPanel
            let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            let newState = Parse.importFromHyw model.SrcOfTrth inner
            let finalPoly = match newState with FreshlyImported m | Stable m -> m
            { model with 
                ActivePanel = BoundaryPanel
                PolygonEditor = newState 
            }, Cmd.ofMsg (PolygonEditorUpdated finalPoly)

    | SetBatchPreview results ->
        { model with 
            BatchPreview = Some results
            LastBatchSrc = Some model.SrcOfTrth // Store the string used for this batch
            IsHyweaving = false 
            ActivePanel = BatchPanel 
        }, Cmd.none

    | TapPreview i ->
        let nextSelection = 
            match model.SelectedPreviewIndex with
            | Some current when current = i -> None
            | _ -> Some i
    
        { model with SelectedPreviewIndex = nextSelection }, Cmd.none

    | CloseBatch ->
        // Reset selection when closing the panel
        { model with ActivePanel = LayoutPanel; SelectedPreviewIndex = None }, Cmd.none

    | ExportPdfRequested ->
            { model with IsHyweaving = true }, 
            Cmd.OfAsync.perform (fun () ->
                let rec computeBatch i acc = async {
                    match i >= 24 with
                    | true -> return acc
                    | false ->
                        let sqnStr = indexToSqn i
                        let forcedStr = injectSqn model.SrcOfTrth sqnStr
                        let cxls, _ = Parse.spaceCxl [||] forcedStr
                        let d = getStaticGeometry cxls (deriveData forcedStr 0).cxClr1 0 10 
                        let configData = 
                            {| sqnName = sqnStr; w = d.w; h = d.h
                               shapes = d.shapes |> Array.map (fun s -> 
                                {| color = s.color; points = s.points; name = s.name; lx = s.lx; ly = s.ly |}) 
                            |}
                        do! Async.Sleep 5
                        return! computeBatch (i + 1) (configData :: acc)
                }
                async {
                    let! results = computeBatch 0 []
                    return results |> List.toArray |> Array.rev
                }) () SetBatchPreview

    | SaveRequested ->
        // 1. Save to Downloads with timestamp
        Storage.saveFile js model.SrcOfTrth |> ignore
        // 2. Also update Shadow Save so the session is current
        Storage.autoSave js model.SrcOfTrth |> ignore
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
                SrcOfTrth = clean
                Tree = newTree
                LastValidTree = newTree
                Derived = deriveData clean elv 
                PolygonEditor = newState 
                ParseError = false
                LastBatchSrc = None
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
let private viewNodeCodeButtons (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    let nodeCodeButtonText =
        match model.EditorMode with
        | Syntax -> "Node"
        | Interactive -> "Code"

    div {
        // Changed justify-content to flex-start to align buttons to the left
        attr.style "display:flex; width: 100%; gap:10px; padding: 0 10px; justify-content: flex-start; align-items: center;"
        
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
        
        button {
            attr.``class`` "hywe-toggle-btn"
            on.click (fun _ -> dispatch ToggleEditorMode)
            text nodeCodeButtonText
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
                attr.key (model.SrcOfTrth.GetHashCode().ToString())
                attr.value model.SrcOfTrth
                on.change (fun e -> dispatch (SetSrcOfTrth (unbox<string> e.Value)))
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

/// <summary> Renders the tab navigation bar. </summary>
let private viewHyweTabs (model: Model) (dispatch: Message -> unit) =
    div {
        attr.style "display:flex; width: 100%; justify-content: center; gap: 10px; margin: 10px 0; border-bottom: 1px solid #333;"
        
        let tab title panel =
            let isActive = model.ActivePanel = panel
            button {
                attr.style (
                    "padding: 8px 16px; cursor: pointer; border: none; font-weight: bold; " +
                    (if isActive then "background: #444; color: white;" 
                     else "background: transparent; color: #888;")
                )
                on.click (fun _ -> dispatch (SetActivePanel panel))
                text title
            }

        tab "Boundary" BoundaryPanel
        tab "Layout" LayoutPanel
        tab "Table" TablePanel
        tab "View" ViewPanel
        tab "Batch" BatchPanel
    }

/// <summary> Renders the primary action button for starting the weaving process. </summary>
let private viewHywePanels (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    div {
        attr.style "padding: 10px; min-height: 400px;"
        
        match model.ActivePanel with
        | BoundaryPanel ->
            let currentInner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            div { 
                attr.id "hywe-polygon-editor"
                PolygonEditor.view currentInner (PolygonEditorMsg >> dispatch) js 
            }

        | LayoutPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }

                div {
                    attr.id "hywe-svg-container"
                    svgCoxels model.Derived.cxCxl1 model.Derived.cxOuIl elv model.Derived.cxClr1 10
                }
            }
        
        | TablePanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    viewHyweTable model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxlAvl
                }
            }

        | ViewPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px; width: 100%; overflow-x: hidden;"
        
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%; max-width: 100vw;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }                
        
                div {
                    // Added max-width: 100% and changed aspect-ratio handling
                    attr.style "width: 95%; max-width: 100%; aspect-ratio: 3/2; position: relative; overflow: hidden; background: #f9f9f9; border-radius: 8px;"
            
                    canvas { 
                        attr.id "hywe-extruded-polygon"
                        // Ensure the canvas itself respects the container bounds
                        attr.style "width: 100%; height: 100%; display: block;" 
                    }
            
                    async { do! extrudePolygons js "hywe-extruded-polygon" model.Derived.cxCxl1 model.Derived.cxClr1 3.0 0 } 
                    |> Async.StartImmediate
                }
            }

        | BatchPanel ->
            div {
                attr.style "width: 100vw; margin-left: calc(-50vw + 50%); min-height: 500px; display: flex; flex-direction: column; align-items: center; background: #ffffff;"
        
                cond model.BatchPreview <| function
                    | Some results -> 
                        alternateConfigurations 
                            results 
                            model.SelectedPreviewIndex // 1. Current selection state
                            TapPreview                 // 2. The Message constructor
                            dispatch                   // 3. The Elmish dispatcher
                            (fun () -> dispatch (SetActivePanel LayoutPanel)) js
                    | None -> 
                        div { 
                            attr.style "text-align:center; padding: 100px; color: #888; width: 100%;"
                            span { attr.``class`` "spinner"; attr.style "display: block; margin: 0 auto 20px;" }
                            text "Generating 24 variations..." 
                        }
            }
    }

/// <summary> The main view composition. </summary>
let view model dispatch (js: IJSRuntime) =
    concat {
        // Node/Code toggle
        viewNodeCodeButtons model dispatch js
        // Interactive Tree and Text Editor
        viewEditorPanel model dispatch
        // Hyweave
        viewHyweButton model dispatch
        // Tabs
        viewHyweTabs model dispatch 
        // Boundary Editor, Layout View, Batch Compare
        viewHywePanels model dispatch js
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