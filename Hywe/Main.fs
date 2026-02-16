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
        NeedsHyweave: bool
        IsHyweaving: bool
        PolygonEditor: EditorState
        ActivePanel: ActivePanel
        EditorMode: EditorMode
        BatchPreview: PreviewConfig[] option
        IsCancelling: bool
        CancelToken: System.Threading.CancellationTokenSource option
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
    | CancelBatch
    | BatchCancelled
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
        NeedsHyweave = false
        IsHyweaving = false
        IsCancelling = false
        CancelToken = None
        PolygonEditor = Stable PolygonEditor.initModel
        ActivePanel = LayoutPanel 
        EditorMode = Interactive
        BatchPreview = None
        LastBatchSrc = None
        SelectedPreviewIndex = None
    }

/// Update
let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetSqnIndex i ->
        let newSqn = indexToSqn i
        let updatedSrc = 
            match model.EditorMode with
            | Interactive -> 
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
                injectSqn model.SrcOfTrth newSqn

        let modelWithNewSqn = 
            { model with 
                Sequence = newSqn
                SrcOfTrth = updatedSrc
                IsHyweaving = true 
                SelectedPreviewIndex = None 
            }

        modelWithNewSqn,
        Cmd.batch [
                    Cmd.map TreeMsg (Cmd.ofMsg NodeCode.CancelDelete)
                    Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 5 }) () (fun _ -> RunHyweave)
                ]

    | SetSrcOfTrth value ->
        { model with SrcOfTrth = value }, Cmd.none

    | StartHyweave ->
        let model2 = { model with 
                        ActivePanel = LayoutPanel
                        IsHyweaving = true
                        NeedsHyweave = false}
        model2,
        Cmd.batch [
                    Cmd.map TreeMsg (Cmd.ofMsg NodeCode.CancelDelete)
                    Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 50 }) () (fun _ -> RunHyweave)
                ]

    | RunHyweave ->
        let updatedSrcOfTrth =
            match model.EditorMode with
            | Syntax ->
                let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
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

        Storage.autoSave js updatedSrcOfTrth |> ignore

        let newModel =
            match model.EditorMode with
            | Syntax ->
                let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
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
            { model with 
                IsHyweaving = false
                NeedsHyweave = false
            }, Cmd.none

    | TreeMsg subMsg ->
            // Destructure the tuple returned by updateSub
            let updatedTree, treeCmd = NodeCode.updateSub subMsg model.Tree 
        
            let newOutput = NodeCode.getOutput
                                updatedTree
                                model.Sequence
                                latestWidth
                                latestHeight
                                latestAbsStr
                                latestEntryStr
                                latestOuterStr
                                latestIslandsStr
            { model with 
                Tree = updatedTree 
                SrcOfTrth = newOutput 
                NeedsHyweave = true }, 
            Cmd.map TreeMsg treeCmd

    | PolygonEditorMsg subMsg ->
        let currentInnerModel = match model.PolygonEditor with Stable m | FreshlyImported m -> m
        model,
        Cmd.OfAsync.perform
            (PolygonEditor.update js subMsg)
            currentInnerModel
            PolygonEditorUpdated

    | PolygonEditorUpdated newModel ->
        syncPolygonState newModel
        { model with 
            PolygonEditor = Stable newModel
            NeedsHyweave = true        },
            Cmd.none

    | SetActivePanel panel ->
        match panel with
        | BatchPanel ->
            let isStale = Some model.SrcOfTrth <> model.LastBatchSrc
            match isStale with
            | true -> 
                let cts = new System.Threading.CancellationTokenSource()
                let model' = 
                    { model with 
                        ActivePanel = panel
                        IsHyweaving = true
                        IsCancelling = false 
                        CancelToken = Some cts
                        BatchPreview = None 
                    }

                model', Cmd.OfAsync.perform (fun (token: System.Threading.CancellationToken) ->
                    // Type annotation (m: Model) helps the compiler find .SrcOfTrth
                    let rec compute (m: Model) i acc = async {
                        if i >= 24 || token.IsCancellationRequested then 
                            return acc
                        else
                            let sqnStr = indexToSqn i
                            let forcedStr = injectSqn m.SrcOfTrth sqnStr
                            let cxls, _ = Parse.spaceCxl [||] forcedStr
                            let d = getStaticGeometry cxls (deriveData forcedStr 0).cxClr1 0 10 
                        
                            // We define configData inside the loop logic
                            let configData = 
                                {| sqnName = sqnStr; w = d.w; h = d.h
                                   shapes = d.shapes |> Array.map (fun s -> 
                                     {| color = s.color; points = s.points; name = s.name; lx = s.lx; ly = s.ly |}) 
                                |}
                        
                            do! Async.Sleep 5
                            // Pass configData into the next recursive call
                            return! compute m (i + 1) (configData :: acc)
                    }
                    async {
                        let! results = compute model 0 []
                        // CONVERSION: Model expects PreviewConfig[], so we convert List to Array
                        return results |> List.toArray |> Array.rev
                    }) cts.Token SetBatchPreview

            | false -> 
                { model with ActivePanel = panel }, Cmd.none

        // This handles the "Incomplete pattern match" error
        | BoundaryPanel | LayoutPanel | TablePanel | ViewPanel ->
            { model with ActivePanel = panel }, Cmd.none
    
    | CancelBatch ->
        model.CancelToken |> Option.iter (fun cts -> cts.Cancel())
        { model with IsCancelling = true }, Cmd.none
    
    | BatchCancelled ->
        { model with IsHyweaving = false; IsCancelling = false }, Cmd.none

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
                    let laidOut = NodeCode.layoutTree tree 0 (ref 50.0)
                    { Root = laidOut; ConfirmingId = None }
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
            LastBatchSrc = Some model.SrcOfTrth 
            IsHyweaving = false 
            IsCancelling = false
            ActivePanel = BatchPanel 
        }, Cmd.none

    | TapPreview i ->
        let nextSelection = 
            match model.SelectedPreviewIndex with
            | Some current when current = i -> None
            | _ -> Some i
        { model with SelectedPreviewIndex = nextSelection }, Cmd.none

    | CloseBatch ->
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
        Storage.saveFile js model.SrcOfTrth |> ignore
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
            let newTree = 
                clean 
                |> CodeNode.preprocessCode 
                |> fun processed ->
                    try 
                        let tree = CodeNode.parseOutput processed
                        let laidOut = NodeCode.layoutTree tree 0 (ref 50.0)
                        { Root = laidOut; ConfirmingId = None }
                    with _ -> model.Tree 

            let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            let newState = Parse.importFromHyw clean inner
            let finalPoly = match newState with FreshlyImported m | Stable m -> m
            syncPolygonState finalPoly

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
                Cmd.ofMsg (PolygonEditorUpdated finalPoly)
                Cmd.OfTask.attempt (fun () -> task { 
                    do! js.InvokeVoidAsync("localStorageSet", "hywe_backup", clean).AsTask() 
                }) () (fun _ -> FinishHyweave)
            ]

// View helpers
let private viewNodeCodeButtons (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    let nodeCodeButtonText =
        match model.EditorMode with
        | Syntax -> "Node"
        | Interactive -> "Code"

    div {
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
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px; gap: 5px;"
            div {
                attr.style "flex:1; overflow-y:hidden; display: flex; justify-content: center; width: 100%;"
                viewTreeEditor model.Tree (TreeMsg >> dispatch)
            }
        }

let private viewHyweButton (model: Model) (dispatch: Message -> unit) =
    let syntaxAltered = model.NeedsHyweave && not model.IsHyweaving
    
    let buttonClass = 
        match model.IsHyweaving with
        | true -> "hyWeaveButton stop-state" 
        | false -> 
            match syntaxAltered with
            | true -> "hyWeaveButton needs-update"
            | false -> "hyWeaveButton"

    div {
        attr.``class`` "hyweave-container"
        button {
            attr.id "hywe-hyweave"
            attr.``class`` buttonClass
            attr.disabled model.IsCancelling 

            on.click (fun _ -> 
                match model.IsHyweaving with
                | true -> dispatch CancelBatch
                | false -> dispatch StartHyweave)
            
            match model.IsHyweaving with
            | true ->
                span { attr.``class`` "spinner" }
                span { 
                    attr.``class`` "label-stack"
                    span { attr.``class`` "weaving-label"; text " h y W E A V E i n g . . ." }
                    span { 
                        attr.``class`` "stop-label"
                        span { attr.style "color: #E67E22; font-weight: bold;white-space: pre"; text " S T O P " } 
                        text "h y W E A V E i n g" 
                    }
                }
            | false -> 
                match syntaxAltered with
                | true ->
                    span { attr.``class`` "hyweave-prompt"; text "syntax altered" }
                    span { attr.``class`` "hyweave-main-text"; text "h y W E A V E" }
                    span { attr.``class`` "hyweave-prompt"; text "to regenerate" }
                | false -> 
                    text "h y W E A V E"
        }
    }

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
                    attr.style "width: 95%; max-width: 100%; aspect-ratio: 3/2; position: relative; overflow: hidden; background: #f9f9f9; border-radius: 8px;"
                    canvas { 
                        attr.id "hywe-extruded-polygon"
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
                            model.SelectedPreviewIndex 
                            TapPreview                   
                            dispatch                   
                            (fun () -> dispatch (SetActivePanel LayoutPanel)) js
                    | None -> 
                        div { 
                            attr.style "text-align:center; padding: 100px; color: #888; width: 100%;"
                            span { attr.``class`` "spinner"; attr.style "display: block; margin: 0 auto 20px;" }
                            text "Generating 24 variations..." 
                        }
            }
    }

let view model dispatch (js: IJSRuntime) =
    concat {
        viewNodeCodeButtons model dispatch js
        viewEditorPanel model dispatch
        viewHyweButton model dispatch
        viewHyweTabs model dispatch 
        viewHywePanels model dispatch js
    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.none)
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> view model dispatch this.JSRuntime)