module Hywe.Main

open System
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open Page
open Hywe.Node
open Hywe.Site
open Hywe.Site.State
open Hywe.Site.View
open ModelTypes
open ModelHelpers
open Hywe
open Hywe.Core
open Hywe.Core.Coxel
open Hywe.Core.Lexel
open Cache
open FileManager

// Defaults / init 
let toMarker lvl = if lvl = 0 then "L0" else sprintf "L%d" lvl
let initialTree = Serialization.initModel beeyond
let initialSequence = allSqns.[11]
let initialPolygonExport = syncPolygonState State.initModel

let initialOutput = Serialization.getOutput
                        initialTree
                        (Map.ofList [0, initialSequence])
                        initialPolygonExport.Width
                        initialPolygonExport.Height
                        initialPolygonExport.AbsStr
                        initialPolygonExport.OuterStr
                        initialPolygonExport.IslandsStr

let initModel =
    {
        Sequences = Map.ofList [0, initialSequence]
        Elevation = 0
        BaseStr = ""
        SrcOfTrth = beeyond
        Tree = initialTree
        ParseError = false
        LastValidTree = initialTree
        Derived = Cache.deriveFromSource beeyond (Map.ofList [0, initialSequence]) initialPolygonExport 0
        LayoutCache = Map.empty
        NeedsHyweave = false
        IsHyweaving = false
        IsCancelling = false
        CancelToken = None
        PolygonEditor = Stable State.initModel
        ActivePanel = LayoutPanel 
        EditorMode = Interactive
        LastBatchSrc = None
        SelectedPreviewIndex = None
        UserDescription = ""
        TeachMetadata = {
            Author = ""
            ProjectTitle = ""
            SessionId = System.Guid.NewGuid().ToString()
            Scale = "Layout"
            Typology = "Residential"
            Flow = "Sequential"
            Ambience = "Open"
            Stage = "Ideation"
        }
        ReportOptions = {
            ProjectTitle = "Spatial Design Exploration"
            ProjectNumber = "HY-001"
            Author = "Hywe Designer"
            ClientName = "Creative Partner"
            Description = "An automated architectural layout study derived from hierarchical spatial requirements, multi-level flow charts, and adjacency matrices."
            IncludeCover = true
            LevelSections = Map.empty
            Captured3DImage = None
        }
        Captured3DImage = None
        ReportBatch = Map.empty
        IsGeneratingReport = false
        SelectedPreset = Some "Simple"
        HoveredInfo = None
        IsSavingToHynteract = false
        ShowSuccessMessage = false
        IsRecording = false
        PolygonExport = initialPolygonExport
        Onboarding = {
            IsActive = true
            IsAutoSimulating = false
            CurrentStep = Welcome
            SeenSteps = Set.empty
        }
        BatchProgress = 0
        CurrentScreen = LoadingScreen
        ViewLocked = false
        EditsCount = 0
        IsPresetsCollapsed = true
        IsWorkspaceCollapsed = true
        IsHelpCollapsed = false
        PendingConfirm = None
        UndoStack = []
        RedoStack = []
        InstallPromptAvailable = false
        ShowPrivacyAlert = false
        IsStandalone = false
        IsCoordsVisible = false
        ShowLinkCopied = false
    }

let updateMetadata (js: IJSRuntime) =
    async {
        do! js.InvokeVoidAsync("document.querySelector('meta[property=\"article:published_time\"]').setAttribute", "content", PUBLISHED_DATE).AsTask() |> Async.AwaitTask
        do! js.InvokeVoidAsync("document.querySelector('meta[property=\"article:modified_time\"]').setAttribute", "content", MODIFIED_DATE).AsTask() |> Async.AwaitTask
        return ()
    } |> Async.StartImmediate
 

let maxUndoDepth = 50

/// Captures the current undoable state and prepends it to the undo stack.
let pushUndo (model: Model) : Model =
    let snap = {
        SrcOfTrth  = model.SrcOfTrth
        Tree       = model.Tree
        PolygonEditor = model.PolygonEditor
        Sequences   = model.Sequences
    }
    match model.UndoStack with
    // Optimized: Only compare the Source of Truth string for equality (very fast)
    | top :: _ when top.SrcOfTrth = snap.SrcOfTrth -> model 
    | _ ->
        let newStack = snap :: model.UndoStack |> List.truncate maxUndoDepth
        { model with UndoStack = newStack; RedoStack = [] }



/// Update
let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    let modelBefore = 
        if model.Onboarding.IsActive then
            match message with
            | NextOnboardingStep | PreviousOnboardingStep | SkipOnboarding | RestartOnboarding | NoOp | ToggleCoords
            | TransitionToIntro | TransitionToMain 
            | LoadState _ | StartHyweave | RunHyweave | FinishHyweave | SetSqnIndex _
            | SelectPreset _ | TogglePresetsCollapse | ToggleHelpCollapse | ToggleConfirm _
            | UpdateMetadata _ | Undo | Redo
            | SetIsStandalone _ | SetPrivacyAlert _ | SetInstallPromptAvailable _
            | CacheResult _ | HyweaveResult _ | RecordResult _ | ReportGenerated _
            | HideLinkCopied -> model
            | TreeMsg (SubMsg.PointerMove _) -> model
            | PolygonEditorMsg (PointerMove _) -> model
            | _ -> { model with Onboarding = { model.Onboarding with IsActive = false; IsAutoSimulating = false }; IsPresetsCollapsed = true; IsWorkspaceCollapsed = true }
        else model
    
    let model = modelBefore
    match message with
    | NoOp -> model, Cmd.none
    | SetSqnIndex i ->
        let model = pushUndo model
        let newSqn = indexToSqn i
        let currentLevel = model.Tree.ActiveLevel
        let targetIsVR = i < 12

        // Enforce category consistency: All levels must share the same category (VR or HR)
        let newSqns = 
            model.Sequences 
            |> Map.map (fun lvl sqn ->
                if lvl = currentLevel then newSqn
                else
                    let currentIsVR = sqnToIndex sqn < 12
                    if currentIsVR <> targetIsVR then 
                        // Mismatch! Force it to match the new category.
                        newSqn
                    else sqn
            )

        let updatedSrc = 
            match model.EditorMode with
            | Interactive -> 
                Serialization.getOutput 
                    model.Tree 
                    newSqns 
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr
            | Syntax -> 
                let mutable s = model.SrcOfTrth
                // Inject changes for all levels that were updated
                for KeyValue(lvl, sqn) in newSqns do
                    let oldSqn = model.Sequences |> Map.tryFind lvl |> Option.defaultValue ""
                    if sqn <> oldSqn then
                        s <- injectSqn s lvl sqn
                s

        Protocol.sync js updatedSrc model.ActivePanel

        match Cache.get (toMarker currentLevel) i model.LayoutCache with
        | Some config ->
            { model with 
                Sequences = newSqns
                SrcOfTrth = updatedSrc
                Derived = Cache.toDerived config
                SelectedPreviewIndex = None 
            }, Cmd.none
        | None ->
            { model with 
                Sequences = newSqns
                SrcOfTrth = updatedSrc
                IsHyweaving = true 
                SelectedPreviewIndex = None 
            }, Cmd.OfAsync.perform (fun () -> async {
                let config = Cache.generateSingleConfig updatedSrc Hexel.sqnArray.[i] model.PolygonExport currentLevel
                return toMarker currentLevel, currentLevel, i, config
            }) () CacheResult

    | SetSrcOfTrth value ->
        let m = pushUndo model
        let nextCount = m.EditsCount + 1
        let nextCollapse = if nextCount = 2 then true else model.IsPresetsCollapsed
        let nextWorkspaceCollapse = if nextCount = 2 then true else model.IsWorkspaceCollapsed
        let newSqns = extractSequences value
        { m with 
            SrcOfTrth = value
            Sequences = newSqns
            EditsCount = nextCount 
            IsPresetsCollapsed = nextCollapse 
            IsWorkspaceCollapsed = nextWorkspaceCollapse
        }, Cmd.OfAsync.perform (fun () -> async { Protocol.sync js value m.ActivePanel }) () (fun _ -> NoOp)

    | StartHyweave ->
        let markers = model.Tree.Levels.Keys |> Seq.map toMarker |> Seq.toList
        let newCache = Cache.init markers
        let model2 = { model with 
                        IsHyweaving = true
                        NeedsHyweave = false
                        LayoutCache = newCache }
        model2,
        Cmd.batch [
                    Cmd.map TreeMsg (Cmd.ofMsg CancelAction)
                    Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 50 }) () (fun _ -> RunHyweave)
                ]

    | RunHyweave ->
        let updatedSrcOfTrth =
            match model.EditorMode with
            | Syntax -> model.SrcOfTrth
            | Interactive ->
                Serialization.getOutput
                    model.Tree
                    model.Sequences
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr

        Protocol.sync js updatedSrcOfTrth model.ActivePanel

        let currentLevel = max 0 model.Tree.ActiveLevel
        let currentSqnIdx = 
            model.Sequences 
            |> Map.tryFind currentLevel 
            |> Option.bind (fun s -> Hexel.sqnArray |> Array.tryFindIndex (fun x -> sprintf "%A" x = s)) 
            |> Option.defaultValue 11
        let currentSqn = Hexel.sqnArray.[currentSqnIdx]
        
        model, Cmd.OfAsync.perform (fun () -> async {
            let mutable updatedCache = Map.empty // Clear cache because source text or boundary changed
            
            // 1. Handle current orientation (might be different from 11)
            let srcForCurrent = ensureCategory updatedSrcOfTrth currentSqnIdx
            let fullDataCurrent = Cache.computeFullLayout srcForCurrent currentSqn model.PolygonExport currentLevel
            for lvl in model.Tree.Levels.Keys do
                let c = Cache.fromFullLayout fullDataCurrent currentSqn lvl
                updatedCache <- Cache.update (toMarker lvl) currentSqnIdx c updatedCache
            
            // 2. Handle orientation 11 (standard default) if current orientation is different
            if currentSqnIdx <> 11 then
                let srcFor11 = ensureCategory updatedSrcOfTrth 11
                let fullData11 = Cache.computeFullLayout srcFor11 Hexel.sqnArray.[11] model.PolygonExport 0
                for lvl in model.Tree.Levels.Keys do
                    let c = Cache.fromFullLayout fullData11 Hexel.sqnArray.[11] lvl
                    updatedCache <- Cache.update (toMarker lvl) 11 c updatedCache

            return updatedSrcOfTrth, updatedCache
        }) () HyweaveResult

    | HyweaveResult (src, cache) ->
        let currentLevel = max 0 model.Tree.ActiveLevel
        let currentSqnIdx = 
            model.Sequences 
            |> Map.tryFind currentLevel 
            |> Option.bind (fun s -> Hexel.sqnArray |> Array.tryFindIndex (fun x -> sprintf "%A" x = s)) 
            |> Option.defaultValue 11
        let activeConfig = Cache.get (toMarker currentLevel) currentSqnIdx cache |> Option.get
        
        let finalSrc = Cache.populateNestBoundaries src activeConfig.cxCxl1

        { model with 
            SrcOfTrth = finalSrc
            LayoutCache = cache
            Derived = Cache.toDerived activeConfig
            IsHyweaving = false
            NeedsHyweave = false
        }, Cmd.OfAsync.perform (fun () -> async { Protocol.sync js finalSrc model.ActivePanel }) () (fun _ -> NoOp)

    | CacheResult (marker, lvl, idx, data) ->
        let newCache = Cache.update marker idx data model.LayoutCache
        let newModel = { model with LayoutCache = newCache }
        if lvl = max 0 model.Tree.ActiveLevel then
            let currentSqnIdx = 
                model.Sequences 
                |> Map.tryFind lvl 
                |> Option.bind (fun s -> Hexel.sqnArray |> Array.tryFindIndex (fun x -> sprintf "%A" x = s)) 
                |> Option.defaultValue 11
            if idx = currentSqnIdx then
                let finalSrc = Cache.populateNestBoundaries newModel.SrcOfTrth data.cxCxl1
                { newModel with Derived = Cache.toDerived data; SrcOfTrth = finalSrc; IsHyweaving = false }, Cmd.OfAsync.perform (fun () -> async { Protocol.sync js finalSrc model.ActivePanel }) () (fun _ -> NoOp)
            else newModel, Cmd.none
        else newModel, Cmd.none

    | FinishHyweave ->
            { model with 
                IsHyweaving = false
                NeedsHyweave = false
            }, Cmd.none

    | TreeMsg subMsg ->
            let isMoving = match subMsg with SubMsg.PointerMove _ -> true | _ -> false
            let shouldPush = 
                match subMsg with
                | SubMsg.ExecuteAction _ | SubMsg.AddChild _ | SubMsg.UpdateName _ 
                | SubMsg.UpdateWeight _ | SubMsg.UpdateExtrusion _ | SubMsg.SetTopExtrusion _ -> true
                | SubMsg.PointerUp ->
                    // Push only if a drag actually happened
                    model.Tree.DraggingId.IsSome
                | _ -> false

            let model = if shouldPush then pushUndo model else model
            let updatedTree, treeCmd = NodeTree.updateSub js subMsg model.Tree 
        
            // Synchronize sequences map with all levels in the tree
            let newSqns = 
                (model.Sequences, updatedTree.Levels.Keys)
                ||> Seq.fold (fun m lvl ->
                    match Map.containsKey lvl m with
                    | true -> m
                    | false -> 
                        // Inherit from parent (lvl-1) if possible, else default to 11 (VRCCNE)
                        let parentSqn = m |> Map.tryFind (lvl - 1) |> Option.defaultValue "VRCCNE"
                        Map.add lvl parentSqn m
                )

            let newOutput = Serialization.getOutput
                                 updatedTree
                                 newSqns
                                 model.PolygonExport.Width
                                 model.PolygonExport.Height
                                 model.PolygonExport.AbsStr
                                 model.PolygonExport.OuterStr
                                 model.PolygonExport.IslandsStr

            let isLevelSwitch = match subMsg with SubMsg.SetLevel _ | SubMsg.SetNest _ -> true | _ -> false
            let isAction = match subMsg with SubMsg.ExecuteAction _ -> true | _ -> false

            let isIncrementalEdit = not (isMoving || isLevelSwitch)
            let nextCount = if isIncrementalEdit then model.EditsCount + 1 else model.EditsCount
            let nextCollapse = if nextCount = 2 then true else model.IsPresetsCollapsed
            let nextWorkspaceCollapse = if nextCount = 2 then true else model.IsWorkspaceCollapsed
            
            let modelWithTree = 
                { model with 
                    Tree = updatedTree 
                    Sequences = newSqns
                    SrcOfTrth = newOutput 
                    NeedsHyweave = if isMoving then model.NeedsHyweave else true
                    EditsCount = nextCount
                    IsPresetsCollapsed = nextCollapse
                    IsWorkspaceCollapsed = nextWorkspaceCollapse }

            if isIncrementalEdit || (match subMsg with SubMsg.PointerUp -> true | _ -> false) then
                Protocol.sync js newOutput model.ActivePanel

            let finalModel, finalCmd = 
                if isLevelSwitch || isAction then
                    let m = { modelWithTree with Derived = Cache.deriveFromSource newOutput model.Sequences model.PolygonExport updatedTree.ActiveLevel }
                    if model.ActivePanel = BatchPanel then
                        { m with 
                            IsHyweaving = true
                            BatchProgress = 0
                        }, Cmd.batch [ Cmd.map TreeMsg treeCmd; Cmd.ofMsg (GenerateNextBatchItem 0) ]
                    else
                        m, Cmd.map TreeMsg treeCmd
                else modelWithTree, Cmd.map TreeMsg treeCmd

            finalModel, finalCmd

    | PolygonEditorMsg subMsg ->
        let currentInnerModel = match model.PolygonEditor with Stable m | FreshlyImported m -> m
        model,
        Cmd.OfAsync.perform
            (State.update js subMsg)
            currentInnerModel
            PolygonEditorUpdated

    | PolygonEditorUpdated newModel ->
        let model = pushUndo model
        let newExport = syncPolygonState newModel
        let newOutput = Serialization.getOutput
                             model.Tree
                             model.Sequences
                             newExport.Width
                             newExport.Height
                             newExport.AbsStr
                             newExport.OuterStr
                             newExport.IslandsStr
        Protocol.sync js newOutput model.ActivePanel

        { model with 
            PolygonEditor = Stable newModel
            PolygonExport = newExport
            SrcOfTrth = newOutput
            NeedsHyweave = true        },
            Cmd.none

    | SetActivePanel _ | FileImported _ | SelectPreset _ | ReportGenerated _ | UpdateReportOptions _ | DownloadCoordCsv | DownloadMetricsCsv | DownloadAdjCsv | DownloadBatchCoordCsv | DownloadBatchMetricsCsv | DownloadBatchAdjCsv | ToggleCoords as msg ->
        let model = 
            match msg with
            | FileImported _ | SelectPreset _ -> pushUndo model
            | _ -> model
        match PageHelpers.update js msg model with
        | Some (newModel, cmd) -> 
            Protocol.sync js newModel.SrcOfTrth newModel.ActivePanel
            newModel, cmd
        | None -> model, Cmd.none

    | ToggleEditorMode | ExportPdfRequested | ToggleBoundary | ToggleViewLock | Download3DSvg 
    | DownloadDxf | DownloadObj | DownloadBatchDxf | GenerateReport as msg ->
        let model = 
            match msg with
            | ToggleBoundary | ToggleEditorMode -> pushUndo model
            | _ -> model
        match PageHelpers.update js msg model with
        | Some (newModel, cmd) -> 
            Protocol.sync js newModel.SrcOfTrth newModel.ActivePanel
            newModel, cmd
        | None -> model, Cmd.none
    | SetBatchFinished ->
        { model with 
            LastBatchSrc = Some model.SrcOfTrth 
            IsHyweaving = false 
            IsCancelling = false
            ActivePanel = BatchPanel 
            BatchProgress = 24
        }, Cmd.none
    | SetBatchProgress p ->
        { model with BatchProgress = p }, Cmd.none
    
    // --- Recursive Batch Generation ---
    | GenerateNextBatchItem i ->
        if i >= 24 || model.IsCancelling then
            { model with IsHyweaving = false; IsCancelling = false }, 
            Cmd.ofMsg SetBatchFinished
        else
            model, Cmd.OfAsync.perform (fun () -> async {
                try
                    let mutable currentCache = model.LayoutCache
                    let sqn = Hexel.sqnArray.[i]
                    let marker = toMarker model.Tree.ActiveLevel
                    
                    match Cache.get marker i currentCache with
                    | Some _ ->
                        // Fill-in-the-blanks approach: skip if already computed
                        do! Async.Sleep 1
                        return currentCache
                    | None ->
                        // Force all levels in the source to match the current batch orientation 'sqn'
                        let srcForBatch = ensureCategory model.SrcOfTrth i

                        // Compute full layout once for this orientation
                        let fullData = Cache.computeFullLayout srcForBatch sqn model.PolygonExport 0
                        
                        // Update cache for all levels
                        for lvl in model.Tree.Levels.Keys do
                            let config = Cache.fromFullLayout fullData sqn lvl
                            currentCache <- Cache.update (toMarker lvl) i config currentCache
                        
                        do! Async.Sleep 5
                        return currentCache
                with _ -> return model.LayoutCache
            }) () AddBatchItem

    | AddBatchItem updatedCache ->
        let nextI = model.BatchProgress + 1
        { model with BatchProgress = nextI; LayoutCache = updatedCache }, Cmd.ofMsg (GenerateNextBatchItem nextI)
    | TapBatchPreview i ->
        let nextSelection = 
            match model.SelectedPreviewIndex with
            | Some current when current = i -> None
            | _ -> Some i
        { model with SelectedPreviewIndex = nextSelection }, Cmd.none
    | CloseBatch ->
        { model with ActivePanel = LayoutPanel; SelectedPreviewIndex = None }, Cmd.none
    | CancelBatch ->
        model.CancelToken |> Option.iter (fun cts -> cts.Cancel())
        { model with IsCancelling = true }, Cmd.none
    | BatchCancelled ->
        { model with IsHyweaving = false; IsCancelling = false }, Cmd.none
    | SaveRequested ->
        FileManager.saveFile js model.SrcOfTrth |> ignore
        Protocol.sync js model.SrcOfTrth model.ActivePanel
        model, Cmd.none
    | ImportRequested ->
        let doClick () =
            task {
                do! js.InvokeVoidAsync("clickElement", "hyw-import-hidden").AsTask()
            }
        model, Cmd.OfTask.perform doClick () (fun _ -> FinishHyweave)
    | ViewCaptured dataUrl ->
        { model with Captured3DImage = Some dataUrl }, Cmd.none
    | SetDescription _ | SuggestDescription | RecordResult _ | UpdateMetadata _ 
    | SetHoveredInfo _ | StartVoiceCapture | OnVoiceResult | RecordToHynteract as msg ->
        match Teach.update js msg model with
        | Some (newModel, cmd) -> newModel, cmd
        | None -> model, Cmd.none

    | ShareLink ->
        let p = 
            match model.ActivePanel with
            | BoundaryPanel -> "boundary"
            | LayoutPanel -> "layout"
            | AnalyzePanel -> "analyze"
            | ViewPanel -> "3d"
            | TeachPanel -> "teach"
            | ReportPanel -> "report"
            | BatchPanel -> "batch"
        let fullUrl = sprintf "https://hywe.in/#%s|P=%s" model.SrcOfTrth p
        { model with ShowLinkCopied = true }, 
        Cmd.batch [
            Cmd.OfTask.perform (fun () -> js.InvokeAsync<bool>("shareUrl", "Hywe Design", "", fullUrl).AsTask()) () (fun _ -> NoOp)
            Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 2000 }) () (fun _ -> HideLinkCopied)
        ]

    | HideLinkCopied ->
        { model with ShowLinkCopied = false }, Cmd.none

    | LoadState (content, panel, isFromUrl) ->
        let resolvedPanel = panel |> Option.defaultValue model.ActivePanel
        if isFromUrl then
             async { do! js.InvokeVoidAsync("console.log", sprintf "Hywe: Restoration successful. Panel: %A" panel).AsTask() |> Async.AwaitTask } |> Async.StartImmediate

        let modelWithPanel = 
            { model with 
                ActivePanel = resolvedPanel
                Onboarding = { model.Onboarding with IsActive = if isFromUrl then false else model.Onboarding.IsActive }
            }

        if String.IsNullOrWhiteSpace content then modelWithPanel, Cmd.none
        else
            try
                // Extract Sequences (Q=...)
                let newSqns = extractSequences content
                
                // Restore Tree
                let newTree = Serialization.initModel content
                
                // Restore Polygon
                let currentInner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
                let newState = FileManager.importFromHyw content currentInner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                let newExport = syncPolygonState finalPoly
                
                let updatedModel = 
                    { modelWithPanel with 
                        SrcOfTrth = content
                        Tree = newTree
                        LastValidTree = newTree
                        PolygonEditor = newState
                        PolygonExport = newExport
                        Sequences = newSqns
                        Derived = Cache.deriveFromSource content newSqns newExport newTree.ActiveLevel
                        LayoutCache = Map.empty
                        NeedsHyweave = true
                        IsPresetsCollapsed = true
                        IsWorkspaceCollapsed = true
                    }
                
                // Validate that the loaded state actually results in a layout, 
                // but allow deep links to bypass this check so they can set the panel even on empty projects.
                if not isFromUrl && Array.isEmpty updatedModel.Derived.cxCxl1 then
                    modelWithPanel, Cmd.none 
                else
                    updatedModel, Cmd.none
            with _ ->
                modelWithPanel, Cmd.none

    | HardReset ->
        Protocol.purgeLocalBackup js
        Protocol.sync js "" model.ActivePanel
        let model = pushUndo model
        let resetSyntax = start
        let resetTree = Serialization.initModel resetSyntax
        let resetPoly = State.initModel
        let resetExport = syncPolygonState resetPoly
        
        { model with 
            SrcOfTrth = resetSyntax
            Tree = resetTree
            LastValidTree = resetTree
            PolygonEditor = Stable resetPoly
            PolygonExport = resetExport
            Sequences = Map.ofList [0, allSqns.[11]]
            Derived = Cache.deriveFromSource resetSyntax (Map.ofList [0, allSqns.[11]]) resetExport 0
            LayoutCache = Map.empty
            NeedsHyweave = true
            EditsCount = 0
            SelectedPreset = None
            PendingConfirm = None
        }, Cmd.none

    | Undo ->
        match model.UndoStack with
        | [] -> model, Cmd.none
        | snap :: rest ->
            let redoSnap = { SrcOfTrth = model.SrcOfTrth; Tree = model.Tree; PolygonEditor = model.PolygonEditor; Sequences = model.Sequences }
            let finalPoly = match snap.PolygonEditor with Stable m | FreshlyImported m -> m
            let newExport = syncPolygonState finalPoly
            let restored = { model with
                                SrcOfTrth    = snap.SrcOfTrth
                                Tree         = snap.Tree
                                PolygonEditor = snap.PolygonEditor
                                PolygonExport = newExport
                                Sequences     = snap.Sequences
                                Derived      = Cache.deriveFromSource snap.SrcOfTrth snap.Sequences newExport snap.Tree.ActiveLevel
                                LayoutCache  = Map.empty
                                UndoStack    = rest
                                RedoStack    = redoSnap :: model.RedoStack
                                NeedsHyweave = true }
            restored, Cmd.none

    | Redo ->
        match model.RedoStack with
        | [] -> model, Cmd.none
        | snap :: rest ->
            let undoSnap = { SrcOfTrth = model.SrcOfTrth; Tree = model.Tree; PolygonEditor = model.PolygonEditor; Sequences = model.Sequences }
            let finalPoly = match snap.PolygonEditor with Stable m | FreshlyImported m -> m
            let newExport = syncPolygonState finalPoly
            let restored = { model with
                                SrcOfTrth    = snap.SrcOfTrth
                                Tree         = snap.Tree
                                PolygonEditor = snap.PolygonEditor
                                PolygonExport = newExport
                                Sequences     = snap.Sequences
                                Derived      = Cache.deriveFromSource snap.SrcOfTrth snap.Sequences newExport snap.Tree.ActiveLevel
                                LayoutCache  = Map.empty
                                RedoStack    = rest
                                UndoStack    = undoSnap :: model.UndoStack
                                NeedsHyweave = true }
            restored, Cmd.none

    | NextOnboardingStep ->
        if not model.Onboarding.IsActive then model, Cmd.none
        else
        let nextStep = 
            match model.Onboarding.CurrentStep with
            | Welcome -> NodeGuide
            | NodeGuide -> NodeMenuGuide
            | NodeMenuGuide -> ElevateGuide
            | ElevateGuide -> MoveNodeGuide
            | MoveNodeGuide -> BoundaryGuide
            | BoundaryGuide -> LayoutGuide
            | LayoutGuide -> Finish
            | Finish -> Finish

        let isFinished = nextStep = Finish && model.Onboarding.CurrentStep = Finish
        let newActivePanel = 
            match nextStep with
            | NodeGuide -> LayoutPanel
            | NodeMenuGuide -> LayoutPanel
            | ElevateGuide -> LayoutPanel
            | MoveNodeGuide -> LayoutPanel
            | BoundaryGuide -> BoundaryPanel
            | LayoutGuide -> LayoutPanel
            | _ -> model.ActivePanel

        let cmd = Cmd.none

        { model with 
            Onboarding = { model.Onboarding with 
                            CurrentStep = nextStep
                            IsActive = not isFinished
                            SeenSteps = model.Onboarding.SeenSteps.Add(model.Onboarding.CurrentStep) }
            ActivePanel = newActivePanel
            IsPresetsCollapsed = if isFinished then true else model.IsPresetsCollapsed
            IsWorkspaceCollapsed = if isFinished then true else model.IsWorkspaceCollapsed
        }, cmd

    | PreviousOnboardingStep ->
        if not model.Onboarding.IsActive then model, Cmd.none
        else
        let prevStep = 
            match model.Onboarding.CurrentStep with
            | Welcome -> Welcome
            | NodeGuide -> Welcome
            | NodeMenuGuide -> NodeGuide
            | ElevateGuide -> NodeMenuGuide
            | MoveNodeGuide -> ElevateGuide
            | BoundaryGuide -> MoveNodeGuide
            | LayoutGuide -> BoundaryGuide
            | Finish -> LayoutGuide

        let newActivePanel = 
            match prevStep with
            | NodeGuide -> LayoutPanel
            | NodeMenuGuide -> LayoutPanel
            | ElevateGuide -> LayoutPanel
            | MoveNodeGuide -> LayoutPanel
            | BoundaryGuide -> BoundaryPanel
            | LayoutGuide -> LayoutPanel
            | _ -> LayoutPanel

        { model with 
            Onboarding = { model.Onboarding with CurrentStep = prevStep }
            ActivePanel = newActivePanel
        }, Cmd.none

    | SkipOnboarding ->
        { model with Onboarding = { model.Onboarding with IsActive = false; IsAutoSimulating = false }; IsPresetsCollapsed = true; IsWorkspaceCollapsed = true }, 
        Cmd.map TreeMsg (Cmd.ofMsg CancelAction) // Just in case

    | RestartOnboarding ->
        { model with 
            Onboarding = { IsActive = true; IsAutoSimulating = false; CurrentStep = Welcome; SeenSteps = Set.empty } 
            ActivePanel = LayoutPanel
        }, Cmd.none

    | StartAutoSimulation ->
        model, Cmd.none

    | StopAutoSimulation ->
        { model with Onboarding = { model.Onboarding with IsAutoSimulating = false } }, Cmd.none

    | SetInstallPromptAvailable available ->
        { model with InstallPromptAvailable = available }, Cmd.none

    | InstallRequested ->
        model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("triggerPwaInstall").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp)

    | SetPrivacyAlert show ->
        { model with ShowPrivacyAlert = show }, Cmd.none

    | SetIsStandalone isS ->
        { model with IsStandalone = isS }, Cmd.none

    | TransitionToIntro ->
        { model with CurrentScreen = IntroScreen }, Cmd.none

    | TransitionToMain ->
        { model with 
            CurrentScreen = MainScreen
            Onboarding = { model.Onboarding with IsActive = model.Onboarding.IsActive }
            IsPresetsCollapsed = true 
            IsWorkspaceCollapsed = true
        }, Cmd.none

    | TogglePresetsCollapse ->
        { model with IsPresetsCollapsed = not model.IsPresetsCollapsed }, Cmd.none

    | ToggleWorkspaceCollapse ->
        { model with IsWorkspaceCollapsed = not model.IsWorkspaceCollapsed }, Cmd.none

    | ToggleHelpCollapse ->
        { model with IsHelpCollapsed = not model.IsHelpCollapsed }, Cmd.none

    | ToggleConfirm action ->
        { model with PendingConfirm = action }, Cmd.none

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    let mutable _dotnetRef: DotNetObjectReference<MyApp> option = None

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    [<JSInvokable>]
    member this.HandleUndo() = this.Dispatch Undo

    [<JSInvokable>]
    member this.HandleRedo() = this.Dispatch Redo

    [<JSInvokable>]
    member this.HandleHashChange(rawHash: string) =
        let (content, panel, isFromUrl) = Protocol.resolveHashChange rawHash
        this.Dispatch (LoadState (content, panel, isFromUrl))

    [<JSInvokable>]
    member this.SetInstallPromptAvailable(available: bool) = this.Dispatch (SetInstallPromptAvailable available)

    [<JSInvokable>]
    member this.SetPrivacyAlert(show: bool) = this.Dispatch (SetPrivacyAlert show)

    [<JSInvokable>]
    member this.SetIsStandalone(isS: bool) = this.Dispatch (SetIsStandalone isS)

    override this.OnAfterRenderAsync(firstRender) =
        let t = base.OnAfterRenderAsync(firstRender)
        if firstRender then
            let ref = DotNetObjectReference.Create(this)
            _dotnetRef <- Some ref
            this.JSRuntime.InvokeVoidAsync("registerUndoRedo", ref).AsTask() |> ignore
            this.JSRuntime.InvokeVoidAsync("registerPwaInstall", ref).AsTask() |> ignore
            this.JSRuntime.InvokeVoidAsync("registerHashChange", ref).AsTask() |> ignore
        t

    interface System.IDisposable with
        member _.Dispose() =
            _dotnetRef |> Option.iter (fun r -> r.Dispose())

    override this.OnInitialized() =
        base.OnInitialized()

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.batch [
                Cmd.OfAsync.perform (fun () -> Protocol.resolveStartupState this.JSRuntime) () (fun (res, panel, isFromUrl) -> 
                        LoadState (res, panel, isFromUrl))
                Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 1000 }) () (fun _ -> TransitionToIntro)
                Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 3000 }) () (fun _ -> TransitionToMain)
                Cmd.OfAsync.perform (fun () -> async { updateMetadata this.JSRuntime; return () }) () (fun _ -> NoOp)
            ])
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> 
                concat {
                    if model.Onboarding.IsActive && model.CurrentScreen = MainScreen then
                        Page.Help.viewHelp model.Onboarding dispatch

                    Styles.render()
                    Index.coreScript
                    Index.siteHeader
                    div {
                        attr.id "page-content"
                        if model.CurrentScreen <> LoadingScreen then
                            attr.``class`` "fade-container fade-in"
                        else
                            attr.``class`` "fade-container"
                        
                        Index.introSplash model.CurrentScreen dispatch

                        div {
                            attr.id "main"
                            if model.CurrentScreen = MainScreen then
                                attr.``class`` "fade-in"
                                attr.style "display: block; opacity: 1;"
                            else
                                attr.style "display: none; opacity: 0;"

                            view model dispatch this.JSRuntime
                        }

                        if model.ShowPrivacyAlert && model.CurrentScreen = MainScreen then
                            div {
                                attr.style "position: fixed; top: 80px; left: 50%; transform: translateX(-50%); z-index: 5000; background: #363636; color: white; padding: 15px 18px; border-radius: 6px; box-shadow: 0 8px 24px rgba(0,0,0,0.3); font-size: 13px; max-width: 260px; display: flex; flex-direction: column; gap: 10px; border: 1px solid rgba(255,255,255,0.1); animation: fadeIn 0.3s ease-out;"
                                div {
                                    attr.style "font-weight: 500; display: flex; align-items: center; gap: 8px;"
                                    rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="#f0ad4e" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"></path><line x1="12" y1="9" x2="12" y2="13"></line><line x1="12" y1="17" x2="12" y2="17.01"></line></svg>"""
                                    span { attr.style "font-size: 11px; letter-spacing: 0.5px; text-transform: uppercase;"; text "Persistence" }
                                }
                                div {
                                    attr.style "opacity: 0.7; line-height: 1.4; font-size: 11px;"
                                    text "Privacy browsers may clear local work. Install as an app to ensure data is saved."
                                }
                                div {
                                    attr.style "display: flex; flex-direction: column; gap: 6px; margin-top: 4px;"
                                    if model.InstallPromptAvailable then
                                        button {
                                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                                            attr.style "width: 100%; font-size: 10px; font-weight: 600;"
                                            on.click (fun _ -> dispatch InstallRequested)
                                            text "INSTALL"
                                        }
                                    button {
                                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-ghost"
                                        attr.style "width: 100%; color: white; opacity: 0.5; font-size: 10px;"
                                        on.click (fun _ -> dispatch (SetPrivacyAlert false))
                                        text "DISMISS"
                                    }
                                }
                            }

                        Index.siteFooter model.CurrentScreen
                    }
                    Index.loadingScreen model.CurrentScreen
                }
            )
