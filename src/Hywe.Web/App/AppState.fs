module AppState

open System
open Microsoft.JSInterop
open Elmish
open PageElements
open Hywe.Node
open Types
open State
open ModelTypes
open Hywe.Core
open FileManager

// Active Patterns & Pure Helpers
let (|BlankString|ValidString|) (s: string) =
    if String.IsNullOrWhiteSpace s then BlankString
    else ValidString (s.Trim())

let (|NonEmptyString|_|) (s: string) =
    match s with
    | BlankString -> None
    | ValidString trimmed -> Some trimmed

let toMarker lvl = match lvl with 0 -> "L0" | _ -> sprintf "L%d" lvl

let getInnerPolygonEditor = function
    | Stable m | FreshlyImported m -> m

let getSequenceIndex level sequences =
    sequences
    |> Map.tryFind level
    |> Option.bind (fun s -> Hexel.sqnArray |> Array.tryFindIndex (fun x -> Hexel.sqnToString x = s))
    |> Option.defaultValue 11

let serializeModelTree (tree: SubModel) sequences (export: PolygonExportData) =
    Serialization.getOutput
        tree
        sequences
        export.Width
        export.Height
        export.AbsStr
        export.BaseStr
        export.OuterStr
        export.IslandsStr

let hasGeometryChanged (a: PolygonExportData) (b: PolygonExportData) =
    a.OuterStr <> b.OuterStr || 
    a.IslandsStr <> b.IslandsStr ||
    a.EntryStr <> b.EntryStr ||
    a.Width <> b.Width ||
    a.Height <> b.Height ||
    a.BaseStr <> b.BaseStr ||
    a.AbsStr <> b.AbsStr

let hasBoundaryChanged (a: PolygonExportData) (b: PolygonExportData) =
    a.OuterStr <> b.OuterStr || 
    a.IslandsStr <> b.IslandsStr ||
    a.EntryStr <> b.EntryStr

// Defaults / init 
let initialTree = Serialization.initModel beeyond
let initialSequence = allSqns.[11]
let initialPolygonExport = syncPolygonState State.initModel
let initialOutput = serializeModelTree initialTree (Map.ofList [0, initialSequence]) initialPolygonExport

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
            ExplorationDescription = ""
            SessionId = System.Guid.NewGuid().ToString()
            Scale = ""
            Typology = ""
            Flow = ""
            Ambience = ""
            Stage = ""
            ThumbnailLevel = ""
            ThumbnailVariation = 11
        }
        ReportOptions = {
            ProjectTitle = ""
            Author = ""
            IncludeCover = true
            LevelSections = Map.empty
            Captured3DImage = None
        }
        Captured3DImage = None
        ReportBatch = Map.empty
        IsGeneratingReport = false
        SelectedPreset = None
        HoveredInfo = None
        IsSavingToHynteract = false
        ShowSuccessMessage = false
        TeachErrorMessage = None
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
        PreDragSnapshot = None
        InstallPromptAvailable = false
        ShowPrivacyAlert = false
        IsStandalone = false
        IsCoordsVisible = false
        ShowLinkCopied = false
        ShowGallery = false
        IsLoadingGallery = false
        GalleryEntries = None
        GalleryOffset = 0
        GalleryFilter = ""
        LoadedCommunityAuthor = None
        CachedAuthor = None
        HasAppendedModSuffix = false
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
    let cleanPolyInner = 
        model.PolygonEditor
        |> getInnerPolygonEditor
        |> State.snapshot
        |> State.refreshCachedStrings
    let snap = {
        SrcOfTrth     = model.SrcOfTrth
        Tree          = model.Tree
        PolygonEditor = Stable cleanPolyInner
        Sequences     = model.Sequences
    }
    match model.UndoStack with
    // Only skip if both source of truth and boundary geometry are identical
    | top :: _ when top.SrcOfTrth = snap.SrcOfTrth && 
                    syncPolygonState (getInnerPolygonEditor top.PolygonEditor) = syncPolygonState cleanPolyInner -> 
        model 
    | _ ->
        let newStack = snap :: model.UndoStack |> List.truncate maxUndoDepth
        { model with UndoStack = newStack; RedoStack = [] }

let applyAlterationSuffix (js: IJSRuntime) (model: Model) : Model =
    let userAuthor = 
        match model.CachedAuthor with
        | Some (NonEmptyString a) -> a
        | _ -> ""

    let modelWithoutCommunity = 
        match model.LoadedCommunityAuthor with
        | Some _ ->
            js.InvokeVoidAsync("localStorage.removeItem", "hywe_community_author") |> ignore
            { model with 
                LoadedCommunityAuthor = None
                TeachMetadata = { model.TeachMetadata with Author = userAuthor }
                ReportOptions = { model.ReportOptions with Author = userAuthor }
            } 
        | None -> model

    match modelWithoutCommunity.HasAppendedModSuffix with
    | true -> modelWithoutCommunity
    | false ->
        let currentTitleOpt = 
            [ modelWithoutCommunity.TeachMetadata.ExplorationDescription
              modelWithoutCommunity.ReportOptions.ProjectTitle ]
            |> List.tryPick (function NonEmptyString s -> Some s | _ -> None)
            
        match currentTitleOpt with
        | None -> modelWithoutCommunity
        | Some currentTitle ->
            let dateStr = System.DateTime.Now.ToString("MMdd")
            match currentTitle.Contains(sprintf "(%s)" dateStr) || currentTitle.Contains("(mod ") with
            | true -> { modelWithoutCommunity with HasAppendedModSuffix = true }
            | false ->
                let suffixedTitle = sprintf "%s (%s)" currentTitle dateStr
                js.InvokeVoidAsync("localStorage.setItem", "hywe_title", suffixedTitle) |> ignore
                { modelWithoutCommunity with
                    HasAppendedModSuffix = true
                    TeachMetadata = { modelWithoutCommunity.TeachMetadata with ExplorationDescription = suffixedTitle }
                    ReportOptions = { modelWithoutCommunity.ReportOptions with ProjectTitle = suffixedTitle }
                }

let restoreSnapshot (js: IJSRuntime) (model: Model) (snap: UndoSnapshot) (updateStacks: UndoSnapshot -> Model -> Model) : Model * Cmd<Message> =
    let currentPolyInner = getInnerPolygonEditor model.PolygonEditor
    let reverseSnap = { 
        SrcOfTrth     = model.SrcOfTrth
        Tree          = model.Tree
        PolygonEditor = Stable (currentPolyInner |> State.snapshot |> State.refreshCachedStrings)
        Sequences     = model.Sequences 
    }
    let restoredPolyInner = 
        getInnerPolygonEditor snap.PolygonEditor
        |> State.snapshot
        |> State.refreshCachedStrings
    let newExport = syncPolygonState restoredPolyInner

    // Fast path: if in BoundaryPanel or if source of truth didn't change, reuse current Derived.
    // Avoid running the heavy multi-level layout solver (deriveFromSource) on the UI thread!
    let newDerived =
        match model.ActivePanel with
        | BoundaryPanel -> model.Derived
        | _ when snap.SrcOfTrth = model.SrcOfTrth -> model.Derived
        | _ -> Cache.deriveFromSource snap.SrcOfTrth snap.Sequences newExport snap.Tree.ActiveLevel

    let restored = 
        { model with
            SrcOfTrth       = snap.SrcOfTrth
            Tree            = snap.Tree
            PolygonEditor   = Stable restoredPolyInner
            PolygonExport   = newExport
            Sequences       = snap.Sequences
            Derived         = newDerived
            LayoutCache     = Map.empty
            PreDragSnapshot = None
            NeedsHyweave    = true }
        |> updateStacks reverseSnap

    let syncCmd = 
        Cmd.OfAsync.perform (fun () -> async { Protocol.sync js snap.SrcOfTrth model.ActivePanel }) () (fun _ -> NoOp)

    restored, syncCmd

let performUndo (js: IJSRuntime) (model: Model) : Model * Cmd<Message> =
    match model.UndoStack with
    | [] -> model, Cmd.none
    | snap :: rest ->
        restoreSnapshot js model snap (fun revSnap m ->
            { m with UndoStack = rest; RedoStack = revSnap :: m.RedoStack })

let performRedo (js: IJSRuntime) (model: Model) : Model * Cmd<Message> =
    match model.RedoStack with
    | [] -> model, Cmd.none
    | snap :: rest ->
        restoreSnapshot js model snap (fun revSnap m ->
            { m with RedoStack = rest; UndoStack = revSnap :: m.UndoStack })

let dismissOnboardingIfInteracting (message: Message) (model: Model) : Model =
    match model.Onboarding.IsActive with
    | false -> model
    | true ->
        match message with
        | NextOnboardingStep | PreviousOnboardingStep | SkipOnboarding | RestartOnboarding | NoOp | ToggleCoords
        | TransitionToIntro | TransitionToMain 
        | LoadState _ | StartHyweave | RunHyweave | FinishHyweave | SetSqnIndex _
        | SelectPreset _ | TogglePresetsCollapse | ToggleHelpCollapse | ToggleConfirm _
        | UpdateMetadata _ | Undo | Redo | SetAuthor _ | SetExplorationTitle _ | AuthorCachedLoaded _
        | TitleCachedLoaded _ | CommunityAuthorCachedLoaded _
        | SetIsStandalone _ | SetPrivacyAlert _ | SetInstallPromptAvailable _
        | CacheResult _ | HyweaveResult _ | RecordResult _ | ReportGenerated _
        | HideLinkCopied
        | TreeMsg (SubMsg.PointerMove _)
        | PolygonEditorMsg (PointerMove _) -> model
        | _ ->
            { model with 
                Onboarding = { model.Onboarding with IsActive = false; IsAutoSimulating = false }
                IsPresetsCollapsed = true
                IsWorkspaceCollapsed = true }

let handlePageHelperUpdate (js: IJSRuntime) (shouldPushUndo: bool) (msg: Message) (model: Model) : Model * Cmd<Message> =
    let modelToUpdate = if shouldPushUndo then pushUndo model else model
    match PageHelpers.update js msg modelToUpdate with
    | Some (newModel, cmd) -> 
        Protocol.sync js newModel.SrcOfTrth newModel.ActivePanel
        newModel, cmd
    | None -> model, Cmd.none

/// Update
let update (js: IJSRuntime) (message: Message) (model: Model) : Model * Cmd<Message> =
    let model = dismissOnboardingIfInteracting message model

    match message with
    | NoOp -> model, Cmd.none

    | SetSqnIndex i ->
        let model = pushUndo model
        let newSqn = indexToSqn i
        let currentLevel = model.Tree.ActiveLevel
        let targetIsVR = i < 12

        // Enforce category consistency: When generating for upper levels, the base level should also match.
        let newSqns = 
            model.Sequences 
            |> Map.map (fun lvl sqn ->
                match lvl = currentLevel || (currentLevel > 0 && lvl < currentLevel) with
                | true -> newSqn
                | false ->
                    let currentIsVR = sqnToIndex sqn < 12
                    if currentIsVR <> targetIsVR then newSqn else sqn
            )

        let updatedSrc = 
            match model.EditorMode with
            | Interactive -> serializeModelTree model.Tree newSqns model.PolygonExport
            | Syntax -> 
                (model.SrcOfTrth, newSqns)
                ||> Map.fold (fun s lvl sqn ->
                    let oldSqn = model.Sequences |> Map.tryFind lvl |> Option.defaultValue ""
                    if sqn <> oldSqn then Lexel.injectSqn s lvl sqn else s)

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
        let m = pushUndo model |> applyAlterationSuffix js
        let nextCount = m.EditsCount + 1
        let isSecondEdit = nextCount = 2
        let nextCollapse = isSecondEdit || model.IsPresetsCollapsed
        let nextWorkspaceCollapse = isSecondEdit || model.IsWorkspaceCollapsed

        let synced = PageHelpers.syncSyntaxToModel value m
        let currentLevel = max 0 synced.Tree.ActiveLevel
        let newDerived =
            try Cache.deriveFromSource value synced.Sequences synced.PolygonExport currentLevel
            with _ -> m.Derived

        { synced with 
            SrcOfTrth = value
            Derived = newDerived
            LayoutCache = Map.empty
            NeedsHyweave = true
            EditsCount = nextCount 
            IsPresetsCollapsed = nextCollapse 
            IsWorkspaceCollapsed = nextWorkspaceCollapse
        }, Cmd.OfAsync.perform (fun () -> async { Protocol.sync js value m.ActivePanel }) () (fun _ -> NoOp)

    | StartHyweave ->
        let modelWithSyntax =
            match model.EditorMode with
            | Syntax -> PageHelpers.syncSyntaxToModel model.SrcOfTrth model
            | Interactive -> model

        let markers = modelWithSyntax.Tree.Levels.Keys |> Seq.map toMarker |> Seq.toList
        let newCache = Cache.init markers
        let model2 = { modelWithSyntax with 
                        IsHyweaving = true
                        NeedsHyweave = false
                        LayoutCache = newCache }
        model2,
        Cmd.batch [
            Cmd.map TreeMsg (Cmd.ofMsg CancelAction)
            Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 50 }) () (fun _ -> RunHyweave)
        ]

    | RunHyweave ->
        let modelWithSyntax, updatedSrcOfTrth =
            match model.EditorMode with
            | Syntax ->
                let synced = PageHelpers.syncSyntaxToModel model.SrcOfTrth model
                synced, synced.SrcOfTrth
            | Interactive ->
                model, serializeModelTree model.Tree model.Sequences model.PolygonExport

        Protocol.sync js updatedSrcOfTrth modelWithSyntax.ActivePanel

        let currentExport = modelWithSyntax.PolygonExport
        let currentLevel = max 0 modelWithSyntax.Tree.ActiveLevel
        let currentSqnIdx = getSequenceIndex currentLevel modelWithSyntax.Sequences
        let currentSqn = Hexel.sqnArray.[currentSqnIdx]
        
        let populateLevelsCache orientationIdx sqn layoutLevel cache =
            let src = ensureCategory updatedSrcOfTrth orientationIdx
            let fullData = Cache.computeFullLayout src sqn currentExport layoutLevel
            (cache, modelWithSyntax.Tree.Levels.Keys)
            ||> Seq.fold (fun acc lvl ->
                let config = Cache.fromFullLayout fullData sqn lvl currentExport
                Cache.update (toMarker lvl) orientationIdx config acc)

        modelWithSyntax, Cmd.OfAsync.perform (fun () -> async {
            let initialCache = Map.empty // Clear cache because source text or boundary changed
            let cacheWithCurrent = populateLevelsCache currentSqnIdx currentSqn currentLevel initialCache
            let finalCache =
                match currentSqnIdx with
                | 11 -> cacheWithCurrent
                | _ -> populateLevelsCache 11 Hexel.sqnArray.[11] 0 cacheWithCurrent

            return updatedSrcOfTrth, finalCache
        }) () HyweaveResult

    | HyweaveResult (src, cache) ->
        let currentLevel = max 0 model.Tree.ActiveLevel
        let currentSqnIdx = getSequenceIndex currentLevel model.Sequences
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
        let currentLevel = max 0 model.Tree.ActiveLevel
        let currentSqnIdx = getSequenceIndex lvl model.Sequences

        match (lvl = currentLevel, idx = currentSqnIdx) with
        | true, true ->
            let finalSrc = Cache.populateNestBoundaries newModel.SrcOfTrth data.cxCxl1
            { newModel with 
                Derived = Cache.toDerived data
                SrcOfTrth = finalSrc
                IsHyweaving = false 
            }, Cmd.OfAsync.perform (fun () -> async { Protocol.sync js finalSrc model.ActivePanel }) () (fun _ -> NoOp)
        | _ -> newModel, Cmd.none

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

        let newOutput = serializeModelTree updatedTree newSqns model.PolygonExport

        let isLevelSwitch = match subMsg with SubMsg.SetLevel _ | SubMsg.SetNest _ -> true | _ -> false
        let isAction = match subMsg with SubMsg.ExecuteAction _ -> true | _ -> false
        let isPointerUp = match subMsg with SubMsg.PointerUp -> true | _ -> false

        let isIncrementalEdit = shouldPush
        let nextCount = if isIncrementalEdit then model.EditsCount + 1 else model.EditsCount
        let isSecondEdit = nextCount = 2
        let nextCollapse = isSecondEdit || model.IsPresetsCollapsed
        let nextWorkspaceCollapse = isSecondEdit || model.IsWorkspaceCollapsed
        
        let modelToUse = if isIncrementalEdit then applyAlterationSuffix js model else model
        let modelWithTree = 
            { modelToUse with 
                Tree = updatedTree 
                Sequences = newSqns
                SrcOfTrth = newOutput 
                NeedsHyweave = if isMoving then model.NeedsHyweave else true
                EditsCount = nextCount
                IsPresetsCollapsed = nextCollapse
                IsWorkspaceCollapsed = nextWorkspaceCollapse }

        if isIncrementalEdit || isPointerUp then
            Protocol.sync js newOutput model.ActivePanel

        match isLevelSwitch || isAction with
        | true ->
            let m = { modelWithTree with Derived = Cache.deriveFromSource newOutput model.Sequences model.PolygonExport updatedTree.ActiveLevel }
            match model.ActivePanel with
            | BatchPanel ->
                { m with 
                    IsHyweaving = true
                    BatchProgress = 0
                }, Cmd.batch [ Cmd.map TreeMsg treeCmd; Cmd.ofMsg (GenerateNextBatchItem 0) ]
            | _ ->
                m, Cmd.map TreeMsg treeCmd
        | false ->
            modelWithTree, Cmd.map TreeMsg treeCmd

    | PolygonEditorMsg subMsg ->
        let currentInnerModel = getInnerPolygonEditor model.PolygonEditor
        let isDraggingAny = currentInnerModel.Dragging.IsSome || currentInnerModel.DraggingEntry || currentInnerModel.DraggingIsland.IsSome

        match subMsg with
        | PointerDown _ | StartDragEntry _ ->
            let cleanPolyInner = currentInnerModel |> State.snapshot |> State.refreshCachedStrings
            let preSnap = {
                SrcOfTrth     = model.SrcOfTrth
                Tree          = model.Tree
                PolygonEditor = Stable cleanPolyInner
                Sequences     = model.Sequences
            }
            let modelWithSnap = 
                match model.PreDragSnapshot with
                | None -> { model with PreDragSnapshot = Some preSnap }
                | Some _ -> model
            modelWithSnap,
            Cmd.OfAsync.perform
                (State.update js subMsg)
                currentInnerModel
                PolygonEditorUpdated

        | PointerMove _ ->
            match State.updateSync subMsg currentInnerModel with
            | Some updatedInner ->
                // Fast path: synchronous move update during dragging.
                // Do NOT push undo, do NOT run heavy Protocol.sync/LZString, do NOT re-serialize.
                let newExport = syncPolygonState updatedInner
                let modelSnap =
                    match model.PreDragSnapshot with
                    | None when isDraggingAny ->
                        let cleanPolyInner = currentInnerModel |> State.snapshot |> State.refreshCachedStrings
                        Some {
                            SrcOfTrth     = model.SrcOfTrth
                            Tree          = model.Tree
                            PolygonEditor = Stable cleanPolyInner
                            Sequences     = model.Sequences
                        }
                    | existing -> existing
                { model with
                    PolygonEditor   = Stable updatedInner
                    PolygonExport   = newExport
                    PreDragSnapshot = modelSnap }, Cmd.none
            | None -> model, Cmd.none

        | PointerUp ->
            match State.updateSync subMsg currentInnerModel with
            | Some updatedInner ->
                // Drag completed! Check if an actual drag occurred.
                let wasDragging = isDraggingAny
                let newExport = syncPolygonState updatedInner
                let isChanged =
                    match model.PreDragSnapshot with
                    | Some preSnap ->
                        let preExport = syncPolygonState (getInnerPolygonEditor preSnap.PolygonEditor)
                        hasBoundaryChanged newExport preExport || 
                        newExport.Width <> preExport.Width || 
                        newExport.Height <> preExport.Height
                    | None -> false

                let isBoundaryChanged = model.EditsCount > 0 && isChanged

                let newUndoStack =
                    match model.PreDragSnapshot with
                    | Some preSnap when wasDragging && isChanged ->
                        preSnap :: model.UndoStack |> List.truncate maxUndoDepth
                    | _ -> model.UndoStack

                let newRedoStack =
                    match wasDragging && isChanged with
                    | true -> []
                    | false -> model.RedoStack

                let model = if isBoundaryChanged then applyAlterationSuffix js model else model
                let newOutput = serializeModelTree model.Tree model.Sequences newExport

                let syncCmd =
                    match wasDragging && isChanged with
                    | true -> Cmd.OfAsync.perform (fun () -> async { Protocol.sync js newOutput model.ActivePanel }) () (fun _ -> NoOp)
                    | false -> Cmd.none

                { model with 
                    PolygonEditor   = Stable (updatedInner |> State.snapshot |> State.refreshCachedStrings)
                    PolygonExport   = newExport
                    SrcOfTrth       = newOutput
                    UndoStack       = newUndoStack
                    RedoStack       = newRedoStack
                    PreDragSnapshot = None
                    NeedsHyweave    = if wasDragging && isChanged then true else model.NeedsHyweave },
                    syncCmd
            | None -> { model with PreDragSnapshot = None }, Cmd.none

        | CommitGhostVertex ->
            match State.updateSync subMsg currentInnerModel with
            | Some updatedInner ->
                let model = pushUndo model
                let newExport = syncPolygonState updatedInner
                let model = applyAlterationSuffix js model
                let newOutput = serializeModelTree model.Tree model.Sequences newExport
                let syncCmd = Cmd.OfAsync.perform (fun () -> async { Protocol.sync js newOutput model.ActivePanel }) () (fun _ -> NoOp)
                { model with
                    PolygonEditor   = Stable (updatedInner |> State.snapshot |> State.refreshCachedStrings)
                    PolygonExport   = newExport
                    SrcOfTrth       = newOutput
                    PreDragSnapshot = None
                    NeedsHyweave    = true }, syncCmd
            | None -> model, Cmd.none

        | SelectVertex sel ->
            { model with PolygonEditor = Stable { currentInnerModel with SelectedVertex = sel } }, Cmd.none

        | ToggleInstructions ->
            { model with PolygonEditor = Stable { currentInnerModel with ShowInstructions = not currentInnerModel.ShowInstructions } }, Cmd.none

        | ToggleLock ->
            { model with PolygonEditor = Stable { currentInnerModel with IsLocked = not currentInnerModel.IsLocked; SelectedVertex = None; GhostVertex = None } }, Cmd.none

        | RequestResetBoundary ->
            { model with PendingConfirm = Some ConfirmAction.ResetBoundaryAction }, Cmd.none

        | UndoBoundary ->
            performUndo js model

        | RedoBoundary ->
            performRedo js model

        | _ ->
            model,
            Cmd.OfAsync.perform
                (State.update js subMsg)
                currentInnerModel
                PolygonEditorUpdated

    | PolygonEditorUpdated newModel ->
        let newExport = syncPolygonState newModel
        let isGeometryChanged = hasGeometryChanged newExport model.PolygonExport

        match isGeometryChanged with
        | true ->
            let isBoundaryChanged = 
                model.EditsCount > 0 && hasBoundaryChanged newExport model.PolygonExport
            let model = pushUndo model
            let model = if isBoundaryChanged then applyAlterationSuffix js model else model
            let newOutput = serializeModelTree model.Tree model.Sequences newExport
            let syncCmd = Cmd.OfAsync.perform (fun () -> async { Protocol.sync js newOutput model.ActivePanel }) () (fun _ -> NoOp)

            { model with 
                PolygonEditor   = Stable (newModel |> State.snapshot |> State.refreshCachedStrings)
                PolygonExport   = newExport
                SrcOfTrth       = newOutput
                PreDragSnapshot = None
                NeedsHyweave    = true },
                syncCmd
        | false ->
            { model with 
                PolygonEditor = Stable newModel
                PolygonExport = newExport },
                Cmd.none

    | SetActivePanel _ | FileImported _ | SelectPreset _ | ReportGenerated _ | UpdateReportOptions _ 
    | DownloadCoordCsv | DownloadMetricsCsv | DownloadAdjCsv | DownloadBatchCoordCsv 
    | DownloadBatchMetricsCsv | DownloadBatchAdjCsv | ToggleCoords as msg ->
        let shouldPush = match msg with FileImported _ | SelectPreset _ -> true | _ -> false
        handlePageHelperUpdate js shouldPush msg model

    | ToggleEditorMode | ExportPdfRequested | ToggleBoundary | ToggleViewLock | Download3DPng 
    | Download3DSvg | DownloadBatchSvg | DownloadBatchPng | GenerateReport as msg ->
        let shouldPush = match msg with ToggleBoundary | ToggleEditorMode -> true | _ -> false
        handlePageHelperUpdate js shouldPush msg model

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
    | GenerateNextBatchItem i when i >= 24 || model.IsCancelling ->
        { model with IsHyweaving = false; IsCancelling = false }, 
        Cmd.ofMsg SetBatchFinished

    | GenerateNextBatchItem i ->
        model, Cmd.OfAsync.perform (fun () -> async {
            try
                let sqn = Hexel.sqnArray.[i]
                let marker = toMarker model.Tree.ActiveLevel
                
                match Cache.get marker i model.LayoutCache with
                | Some _ ->
                    // Fill-in-the-blanks approach: skip if already computed
                    do! Async.Sleep 1
                    return model.LayoutCache
                | None ->
                    // Force all levels in the source to match the current batch orientation 'sqn'
                    let srcForBatch = ensureCategory model.SrcOfTrth i
                    let fullData = Cache.computeFullLayout srcForBatch sqn model.PolygonExport 0
                    
                    let updatedCache =
                        (model.LayoutCache, model.Tree.Levels.Keys)
                        ||> Seq.fold (fun cache lvl ->
                            let config = Cache.fromFullLayout fullData sqn lvl model.PolygonExport
                            Cache.update (toMarker lvl) i config cache)
                    
                    do! Async.Sleep 5
                    return updatedCache
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
        Teach.update js msg model
        |> Option.defaultValue (model, Cmd.none)

    | ShareLink ->
        let panelParam = 
            match model.ActivePanel with
            | BoundaryPanel -> "boundary"
            | LayoutPanel -> "layout"
            | AnalyzePanel -> "analyze"
            | ViewPanel -> "3d"
            | TeachPanel -> "teach"
            | ReportPanel -> "report"
            | BatchPanel -> "batch"
        let hash = sprintf "%s|P=%s" model.SrcOfTrth panelParam
        { model with ShowLinkCopied = true }, 
        Cmd.batch [
            Cmd.OfTask.perform (fun () -> js.InvokeAsync<bool>("shareCompressedHash", "Hywe Design", "", hash).AsTask()) () (fun _ -> NoOp)
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

        match content with
        | BlankString -> modelWithPanel, Cmd.none
        | ValidString cleanContent ->
            try
                let newSqns = Lexel.extractSequences cleanContent
                let newTree = Serialization.initModel cleanContent
                let currentInner = getInnerPolygonEditor model.PolygonEditor
                let newState = FileManager.importFromHyw cleanContent currentInner
                let finalPoly = getInnerPolygonEditor newState
                let newExport = syncPolygonState finalPoly
                
                let updatedModel = 
                    { modelWithPanel with 
                        SrcOfTrth = cleanContent
                        Tree = newTree
                        LastValidTree = newTree
                        PolygonEditor = newState
                        PolygonExport = newExport
                        Sequences = newSqns
                        Derived = Cache.deriveFromSource cleanContent newSqns newExport newTree.ActiveLevel
                        LayoutCache = Map.empty
                        NeedsHyweave = true
                        IsPresetsCollapsed = true
                        IsWorkspaceCollapsed = true
                        EditsCount = 0
                        HasAppendedModSuffix = false
                    }
                
                // Validate that the loaded state actually results in a layout, 
                // but allow deep links to bypass this check so they can set the panel even on empty projects.
                match not isFromUrl && Array.isEmpty updatedModel.Derived.cxCxl1 with
                | true -> modelWithPanel, Cmd.none 
                | false -> updatedModel, Cmd.none
            with _ ->
                modelWithPanel, Cmd.none

    | HardReset ->
        Protocol.purgeLocalBackup js
        Protocol.sync js "" model.ActivePanel
        js.InvokeVoidAsync("localStorage.removeItem", "hywe_title") |> ignore
        js.InvokeVoidAsync("localStorage.removeItem", "hywe_community_author") |> ignore
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
            LoadedCommunityAuthor = None
            HasAppendedModSuffix = false
            TeachMetadata = { model.TeachMetadata with ExplorationDescription = "" }
            ReportOptions = { model.ReportOptions with ProjectTitle = "" }
            UserDescription = ""
        }, Cmd.none

    | Undo ->
        performUndo js model

    | Redo ->
        performRedo js model

    | NextOnboardingStep ->
        match model.Onboarding.IsActive with
        | false -> model, Cmd.none
        | true ->
            let nextStep = 
                match model.Onboarding.CurrentStep with
                | Welcome -> NodeGuide
                | NodeGuide -> NodeMenuGuide
                | NodeMenuGuide -> ElevateGuide
                | ElevateGuide -> MoveNodeGuide
                | MoveNodeGuide -> BoundaryGuide
                | BoundaryGuide -> LayoutGuide
                | LayoutGuide | Finish -> Finish

            let isFinished = nextStep = Finish && model.Onboarding.CurrentStep = Finish
            let newActivePanel = 
                match nextStep with
                | BoundaryGuide -> BoundaryPanel
                | NodeGuide | NodeMenuGuide | ElevateGuide | MoveNodeGuide | LayoutGuide -> LayoutPanel
                | _ -> model.ActivePanel

            { model with 
                Onboarding = { model.Onboarding with 
                                CurrentStep = nextStep
                                IsActive = not isFinished
                                SeenSteps = model.Onboarding.SeenSteps.Add(model.Onboarding.CurrentStep) }
                ActivePanel = newActivePanel
                IsPresetsCollapsed = if isFinished then true else model.IsPresetsCollapsed
                IsWorkspaceCollapsed = if isFinished then true else model.IsWorkspaceCollapsed
            }, Cmd.none

    | PreviousOnboardingStep ->
        match model.Onboarding.IsActive with
        | false -> model, Cmd.none
        | true ->
            let prevStep = 
                match model.Onboarding.CurrentStep with
                | Welcome | NodeGuide -> Welcome
                | NodeMenuGuide -> NodeGuide
                | ElevateGuide -> NodeMenuGuide
                | MoveNodeGuide -> ElevateGuide
                | BoundaryGuide -> MoveNodeGuide
                | LayoutGuide -> BoundaryGuide
                | Finish -> LayoutGuide

            let newActivePanel = 
                match prevStep with
                | BoundaryGuide -> BoundaryPanel
                | _ -> LayoutPanel

            { model with 
                Onboarding = { model.Onboarding with CurrentStep = prevStep }
                ActivePanel = newActivePanel
            }, Cmd.none

    | SkipOnboarding ->
        { model with Onboarding = { model.Onboarding with IsActive = false; IsAutoSimulating = false }; IsPresetsCollapsed = true; IsWorkspaceCollapsed = true }, 
        Cmd.map TreeMsg (Cmd.ofMsg CancelAction)

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

    | ToggleGallery ->
        let newShow = not model.ShowGallery
        let cmd = match newShow with true -> Cmd.ofMsg LoadGalleryEntries | false -> Cmd.none
        { model with ShowGallery = newShow; GalleryOffset = 0 }, cmd

    | LoadGalleryEntries ->
        let fetchGallery () = async {
            let! res = js.InvokeAsync<System.Text.Json.JsonElement>("fetchHFGallery").AsTask() |> Async.AwaitTask
            let results = 
                res.EnumerateArray() 
                |> Seq.map (fun e -> 
                    let safeGetString (prop: string) =
                        match e.TryGetProperty(prop) with
                        | true, p when p.ValueKind = System.Text.Json.JsonValueKind.String -> p.GetString()
                        | _ -> ""
                    let safeGetInt (prop: string) =
                        match e.TryGetProperty(prop) with
                        | true, p when p.ValueKind = System.Text.Json.JsonValueKind.Number -> p.GetInt32()
                        | _ -> 0
                    let safeGetBool (prop: string) =
                        match e.TryGetProperty(prop) with
                        | true, p when p.ValueKind = System.Text.Json.JsonValueKind.True -> true
                        | true, p when p.ValueKind = System.Text.Json.JsonValueKind.False -> false
                        | true, p when p.ValueKind = System.Text.Json.JsonValueKind.String ->
                            System.String.Equals(p.GetString(), "true", System.StringComparison.OrdinalIgnoreCase)
                        | _ -> false

                    { Id = safeGetString "id"
                      ExplorationDescription = safeGetString "explorationDescription"
                      Author = safeGetString "author"
                      Description = safeGetString "description"
                      SvgThumbnail = safeGetString "svgThumbnail"
                      LevelsCount = safeGetInt "levelsCount"
                      SpacesCount = safeGetInt "spacesCount"
                      Typology = safeGetString "typology"
                      Scale = safeGetString "scale"
                      Stage = safeGetString "stage"
                      Flow = safeGetString "flow"
                      Ambience = safeGetString "ambience"
                      CreatedAt = safeGetString "createdAt"
                      IsFeatured = safeGetBool "isFeatured" })
                |> Seq.toList

            // Curated distribution:
            // The 3 most recent featured ones appear shuffled within the top 5-6 items,
            // preserving a fair chance for the newest community submission to appear at position 1.
            let featured = results |> List.filter (fun e -> e.IsFeatured)
            let curatedFeatured = featured |> List.truncate 3

            let otherItems = results |> List.filter (fun e -> not (curatedFeatured |> List.exists (fun f -> f.Id = e.Id)))
            let topPoolSize = min 6 results.Length
            let neededFromOther = max 0 (topPoolSize - curatedFeatured.Length)
            let topOther = otherItems |> List.truncate neededFromOther
            let remainingOther = otherItems |> List.skip neededFromOther

            let rnd = System.Random()
            let shuffledTopPool = (curatedFeatured @ topOther) |> List.sortBy (fun _ -> rnd.Next())
            let finalOrderedEntries = shuffledTopPool @ remainingOther

            return finalOrderedEntries
        }
        { model with IsLoadingGallery = true }, Cmd.OfAsync.either fetchGallery () GalleryEntriesLoaded (fun _ -> GalleryEntriesLoaded [])

    | GalleryEntriesLoaded entries ->
        { model with IsLoadingGallery = false; GalleryEntries = Some entries }, Cmd.none

    | NextGalleryPage ->
        let newOffset = model.GalleryOffset + GALLERY_PAGE_SIZE
        { model with GalleryOffset = newOffset }, Cmd.none
        
    | PrevGalleryPage ->
        let newOffset = max 0 (model.GalleryOffset - GALLERY_PAGE_SIZE)
        { model with GalleryOffset = newOffset }, Cmd.none

    | GoToGalleryPage page ->
        let newOffset = max 0 ((page - 1) * GALLERY_PAGE_SIZE)
        { model with GalleryOffset = newOffset }, Cmd.none

    | LoadGalleryDefinition (name, rowId, author) ->
        let loadAsync () = async {
            let! def = js.InvokeAsync<string>("fetchGalleryDefinition", rowId).AsTask() |> Async.AwaitTask
            return (name, def, author)
        }
        let successHandler (loadedName, loadedDef, loadedAuthor) =
            match loadedDef with
            | BlankString -> NoOp
            | ValidString _ -> LoadGalleryDefinitionSuccess (loadedName, loadedDef, loadedAuthor)
            
        { model with PendingConfirm = None; ShowGallery = false }, Cmd.OfAsync.perform loadAsync () successHandler

    | LoadGalleryDefinitionSuccess (name, def, author) ->
        let clean = def.Trim()
        let newTree = 
            clean 
            |> Serialization.preprocessCode 
            |> fun processed ->
                try Serialization.initModel processed
                with _ -> model.Tree 

        let currentInner = getInnerPolygonEditor model.PolygonEditor
        let newState = FileManager.importFromHyw clean currentInner
        let finalPoly = getInnerPolygonEditor newState
        let newExport = syncPolygonState finalPoly
        let newSqns = Lexel.extractSequences clean
        let loadedAuthorOpt = match author with NonEmptyString a -> Some a | _ -> None
        let loadedAuthorStr = defaultArg loadedAuthorOpt ""
        
        let newModel = 
            { model with 
                SrcOfTrth = clean
                Tree = newTree
                LastValidTree = newTree
                Derived = Cache.deriveFromSource clean newSqns newExport newTree.ActiveLevel
                PolygonEditor = newState
                PolygonExport = newExport
                ParseError = false
                Sequences = newSqns
                LayoutCache = Map.empty
                UserDescription = name
                ShowGallery = false
                PendingConfirm = None
                EditsCount = 0
                LoadedCommunityAuthor = loadedAuthorOpt
                HasAppendedModSuffix = false
                TeachMetadata = { model.TeachMetadata with ExplorationDescription = name; Author = loadedAuthorStr }
                ReportOptions = { model.ReportOptions with ProjectTitle = name; Author = loadedAuthorStr }
            }
        js.InvokeVoidAsync("localStorage.setItem", "hywe_title", name) |> ignore
        match loadedAuthorOpt with
        | Some a -> js.InvokeVoidAsync("localStorage.setItem", "hywe_community_author", a) |> ignore
        | None -> js.InvokeVoidAsync("localStorage.removeItem", "hywe_community_author") |> ignore

        pushUndo newModel, Cmd.batch [
            Cmd.ofMsg (PolygonEditorUpdated finalPoly)
            Cmd.ofMsg StartHyweave
        ]

    | ToggleConfirm action ->
        let nextModel = 
            match action with
            | Some (ConfirmAction.LoadGallery _) -> { model with PendingConfirm = action; ShowGallery = false }
            | _ -> { model with PendingConfirm = action }
        nextModel, Cmd.none

    | UpdateGalleryFilter filter ->
        { model with GalleryFilter = filter; GalleryOffset = 0 }, Cmd.none

    | SetAuthor newAuthor ->
        let authorOpt = match newAuthor with NonEmptyString a -> Some a | _ -> None
        match authorOpt with
        | Some a -> js.InvokeVoidAsync("localStorage.setItem", "hywe_author", a) |> ignore
        | None -> js.InvokeVoidAsync("localStorage.removeItem", "hywe_author") |> ignore
        
        let authorStr = defaultArg authorOpt ""
        let newModel = 
            { model with 
                CachedAuthor = authorOpt
                TeachMetadata = { model.TeachMetadata with Author = authorStr }
                ReportOptions = { model.ReportOptions with Author = authorStr }
                LoadedCommunityAuthor = None
            }
        newModel, Cmd.none

    | AuthorCachedLoaded cachedAuthor ->
        match cachedAuthor with
        | BlankString -> model, Cmd.none
        | ValidString author ->
            let newModel = 
                { model with 
                    CachedAuthor = Some author
                    TeachMetadata = 
                        match model.LoadedCommunityAuthor with
                        | None -> { model.TeachMetadata with Author = author }
                        | Some _ -> model.TeachMetadata
                    ReportOptions = 
                        match model.LoadedCommunityAuthor with
                        | None -> { model.ReportOptions with Author = author }
                        | Some _ -> model.ReportOptions
                }
            newModel, Cmd.none

    | SetExplorationTitle newTitle ->
        let titleOpt = match newTitle with NonEmptyString t -> Some t | _ -> None
        match titleOpt with
        | Some t -> js.InvokeVoidAsync("localStorage.setItem", "hywe_title", t) |> ignore
        | None -> js.InvokeVoidAsync("localStorage.removeItem", "hywe_title") |> ignore

        let userAuthor = 
            match model.CachedAuthor with
            | Some (NonEmptyString a) -> a
            | _ -> ""

        let isCommunityLoaded = Option.isSome model.LoadedCommunityAuthor
        if isCommunityLoaded then
            js.InvokeVoidAsync("localStorage.removeItem", "hywe_community_author") |> ignore

        let titleStr = defaultArg titleOpt ""
        let newAuthor = if isCommunityLoaded then userAuthor else model.TeachMetadata.Author
        let newModel = 
            { model with 
                TeachMetadata = { model.TeachMetadata with ExplorationDescription = titleStr; Author = newAuthor }
                ReportOptions = { model.ReportOptions with ProjectTitle = titleStr; Author = newAuthor }
                LoadedCommunityAuthor = None
                HasAppendedModSuffix = false
            }
        newModel, Cmd.none

    | TitleCachedLoaded cachedTitle ->
        match cachedTitle with
        | BlankString -> model, Cmd.none
        | ValidString title ->
            let newModel = 
                { model with 
                    TeachMetadata = { model.TeachMetadata with ExplorationDescription = title }
                    ReportOptions = { model.ReportOptions with ProjectTitle = title }
                }
            newModel, Cmd.none

    | CommunityAuthorCachedLoaded cachedAuthor ->
        match cachedAuthor with
        | BlankString -> model, Cmd.none
        | ValidString author ->
            let newModel = 
                { model with 
                    LoadedCommunityAuthor = Some author
                    TeachMetadata = { model.TeachMetadata with Author = author }
                    ReportOptions = { model.ReportOptions with Author = author }
                }
            newModel, Cmd.none
