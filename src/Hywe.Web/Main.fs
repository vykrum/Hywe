module Hywe.Main

open System
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open Page
open NodeCode
open PolygonEditor
open ModelTypes
open PageHelpers
open ModelHelpers
open Hywe
open Hywe.Core
open Hywe.Core.Coxel

// Defaults / init 
let initialTree = NodeCode.initModel beeyond
let initialSequence = allSqns.[11]
let initialPolygonExport = syncPolygonState PolygonEditor.initModel

let initialOutput = NodeCode.getOutput
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
        SrcOfTrth = initialOutput
        Tree = initialTree
        ParseError = false
        LastValidTree = initialTree
        Derived = PageHelpers.deriveData initialOutput initialPolygonExport.EntryStr 0
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
        UserDescription = ""
        TeachMetadata = {
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
        BatchAccumulator = []
        CurrentScreen = LoadingScreen
        ViewLocked = false
        EditsCount = 0
        IsPresetsCollapsed = true
        IsHelpCollapsed = false
        PendingConfirm = None
        UndoStack = []
        RedoStack = []
        InstallPromptAvailable = false
        ShowPrivacyAlert = false
        IsStandalone = false
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
            | NextOnboardingStep | PreviousOnboardingStep | SkipOnboarding | RestartOnboarding | NoOp 
            | TransitionToIntro | TransitionToMain 
            | LoadBackup _ | StartHyweave | RunHyweave | FinishHyweave | SetSqnIndex _
            | SelectPreset _ | TogglePresetsCollapse | ToggleHelpCollapse | ToggleConfirm _
            | UpdateMetadata _ | Undo | Redo -> model
            | TreeMsg (NodeCode.PointerMove _) -> model
            | PolygonEditorMsg (PolygonEditor.PointerMove _) -> model
            | _ -> { model with Onboarding = { model.Onboarding with IsActive = false; IsAutoSimulating = false }; IsPresetsCollapsed = false }
        else model
    
    let model = modelBefore
    match message with
    | NoOp -> model, Cmd.none
    | SetSqnIndex i ->
        let model = pushUndo model
        let newSqn = indexToSqn i
        let currentLevel = model.Tree.ActiveLevel

        let newSqns = 
            if currentLevel = 0 then
                let treeMax = if model.Tree.Levels.IsEmpty then 0 else model.Tree.Levels.Keys |> Seq.max
                let strMax = model.SrcOfTrth.Split(';', StringSplitOptions.None).Length - 1
                let maxLvl = max treeMax strMax
                (model.Sequences, [0..maxLvl]) ||> List.fold (fun m l -> Map.add l newSqn m)
            else
                model.Sequences |> Map.add currentLevel newSqn

        let updatedSrc = 
            match model.EditorMode with
            | Interactive -> 
                NodeCode.getOutput 
                    model.Tree 
                    newSqns 
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr
            | Syntax -> 
                if currentLevel = 0 then
                    let levelsCount = model.SrcOfTrth.Split(';', StringSplitOptions.None).Length
                    let mutable s = model.SrcOfTrth
                    for l in 0 .. levelsCount - 1 do
                        s <- Parsing.injectSqn s l newSqn
                    s
                else
                    Parsing.injectSqn model.SrcOfTrth currentLevel newSqn

        let modelWithNewSqn = 
            { model with 
                Sequences = newSqns
                SrcOfTrth = updatedSrc
                IsHyweaving = true 
                SelectedPreviewIndex = None 
            }

        Storage.autoSave js updatedSrc |> ignore

        modelWithNewSqn,
        Cmd.batch [
                    Cmd.map TreeMsg (Cmd.ofMsg NodeCode.CancelAction)
                    Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 5 }) () (fun _ -> RunHyweave)
                ]

    | SetSrcOfTrth value ->
        let m = pushUndo model
        let nextCount = m.EditsCount + 1
        let nextCollapse = if nextCount = 2 then true else model.IsPresetsCollapsed
        let newSqns = Parsing.extractSequences value
        { m with 
            SrcOfTrth = value
            Sequences = newSqns
            EditsCount = nextCount 
            IsPresetsCollapsed = nextCollapse 
        }, Cmd.none

    | StartHyweave ->
        let model2 = { model with 
                        ActivePanel = LayoutPanel
                        IsHyweaving = true
                        NeedsHyweave = false}
        model2,
        Cmd.batch [
                    Cmd.map TreeMsg (Cmd.ofMsg NodeCode.CancelAction)
                    Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 50 }) () (fun _ -> RunHyweave)
                ]

    | RunHyweave ->
        let updatedSrcOfTrth =
            match model.EditorMode with
            | Syntax ->
                let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
                let newState = Storage.importFromHyw model.SrcOfTrth inner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                model.SrcOfTrth
            | Interactive ->
                NodeCode.getOutput
                    model.Tree
                    model.Sequences
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr

        Storage.autoSave js updatedSrcOfTrth |> ignore

        let newModel =
            match model.EditorMode with
            | Syntax ->
                let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
                let newState = Storage.importFromHyw updatedSrcOfTrth inner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                let newExport = syncPolygonState finalPoly
                { model with 
                    SrcOfTrth = updatedSrcOfTrth
                    Derived = PageHelpers.deriveData updatedSrcOfTrth model.PolygonExport.EntryStr model.Tree.ActiveLevel
                    PolygonEditor = newState 
                    PolygonExport = newExport }
            | Interactive ->
                { model with 
                    SrcOfTrth = updatedSrcOfTrth
                    Derived = PageHelpers.deriveData updatedSrcOfTrth model.PolygonExport.EntryStr model.Tree.ActiveLevel }
                
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
            let isMoving = match subMsg with NodeCode.PointerMove _ -> true | _ -> false
            let shouldPush = 
                match subMsg with
                | NodeCode.ExecuteAction _ | NodeCode.AddChild _ | NodeCode.UpdateName _ 
                | NodeCode.UpdateWeight _ | NodeCode.UpdateExtrusion _ -> true
                | NodeCode.PointerUp ->
                    // Push only if a drag actually happened
                    model.Tree.DraggingId.IsSome
                | _ -> false

            let model = if shouldPush then pushUndo model else model
            let updatedTree, treeCmd = NodeCode.updateSub js subMsg model.Tree 
        
            let newOutput = NodeCode.getOutput
                                 updatedTree
                                 model.Sequences
                                 model.PolygonExport.Width
                                 model.PolygonExport.Height
                                 model.PolygonExport.AbsStr
                                 model.PolygonExport.OuterStr
                                 model.PolygonExport.IslandsStr

            let isLevelSwitch = match subMsg with NodeCode.SetLevel _ -> true | _ -> false
            let isAction = match subMsg with NodeCode.ExecuteAction _ -> true | _ -> false

            let isIncrementalEdit = not (isMoving || isLevelSwitch)
            let nextCount = if isIncrementalEdit then model.EditsCount + 1 else model.EditsCount
            let nextCollapse = if nextCount = 2 then true else model.IsPresetsCollapsed
            
            let modelWithTree = 
                { model with 
                    Tree = updatedTree 
                    SrcOfTrth = newOutput 
                    NeedsHyweave = if isMoving then model.NeedsHyweave else true
                    EditsCount = nextCount
                    IsPresetsCollapsed = nextCollapse }

            if isIncrementalEdit then
                Storage.autoSave js newOutput |> ignore

            let finalModel, finalCmd = 
                if isLevelSwitch || isAction then
                    let m = { modelWithTree with Derived = PageHelpers.deriveData newOutput model.PolygonExport.EntryStr updatedTree.ActiveLevel }
                    if model.ActivePanel = BatchPanel then
                        { m with 
                            IsHyweaving = true
                            BatchProgress = 0
                            BatchAccumulator = []
                            BatchPreview = None
                        }, Cmd.batch [ Cmd.map TreeMsg treeCmd; Cmd.ofMsg (GenerateNextBatchItem 0) ]
                    else
                        m, Cmd.map TreeMsg treeCmd
                else modelWithTree, Cmd.map TreeMsg treeCmd

            finalModel, finalCmd

    | PolygonEditorMsg subMsg ->
        let currentInnerModel = match model.PolygonEditor with Stable m | FreshlyImported m -> m
        model,
        Cmd.OfAsync.perform
            (PolygonEditor.update js subMsg)
            currentInnerModel
            PolygonEditorUpdated

    | PolygonEditorUpdated newModel ->
        let model = pushUndo model
        let newExport = syncPolygonState newModel
        let newOutput = NodeCode.getOutput
                             model.Tree
                             model.Sequences
                             newExport.Width
                             newExport.Height
                             newExport.AbsStr
                             newExport.OuterStr
                             newExport.IslandsStr
        Storage.autoSave js newOutput |> ignore

        { model with 
            PolygonEditor = Stable newModel
            PolygonExport = newExport
            SrcOfTrth = newOutput
            NeedsHyweave = true        },
            Cmd.none

    | SetActivePanel _ | FileImported _ | SelectPreset _ | ReportGenerated _ | UpdateReportOptions _ | DownloadCoordCsv | DownloadMetricsCsv | DownloadAdjCsv | DownloadBatchCoordCsv | DownloadBatchMetricsCsv | DownloadBatchAdjCsv as msg ->
        let model = 
            match msg with
            | FileImported _ | SelectPreset _ -> pushUndo model
            | _ -> model
        match UpdateUI.update js msg model with
        | Some (newModel, cmd) -> newModel, cmd
        | None -> model, Cmd.none

    | ToggleEditorMode | ExportPdfRequested | ToggleBoundary | ToggleViewLock | Download3DSvg 
    | DownloadDxf | DownloadObj | DownloadBatchDxf | DownloadBatchObj | GenerateReport as msg ->
        let model = 
            match msg with
            | ToggleBoundary | ToggleEditorMode -> pushUndo model
            | _ -> model
        match UpdateUI.update js msg model with
        | Some (newModel, cmd) -> newModel, cmd
        | None -> model, Cmd.none
    | SetBatchPreview results ->
        { model with 
            BatchPreview = Some results
            LastBatchSrc = Some model.SrcOfTrth 
            IsHyweaving = false 
            IsCancelling = false
            ActivePanel = BatchPanel 
            BatchProgress = 24
        }, Cmd.none
    | SetBatchProgress p ->
        { model with BatchProgress = p }, Cmd.none
    
    // --- Recursive Batch Generation ---
    // This pattern allows the UI to update after every single configuration is processed,
    // providing real-time feedback via the progress grid.
    | GenerateNextBatchItem i ->
        if i >= 24 || model.IsCancelling then
            { model with IsHyweaving = false; IsCancelling = false }, 
            Cmd.ofMsg (SetBatchPreview (model.BatchAccumulator |> List.toArray |> Array.rev))
        else
            let sqn = Hexel.sqnArray.[i]
            let sqnStr = sprintf "%A" sqn
            let forcedStr = Parsing.injectSqn model.SrcOfTrth model.Tree.ActiveLevel sqnStr
            
            model, Cmd.OfAsync.perform (fun () -> async {
                try
                    let cxls, cxOuIl, cxElv1 = Parsing.generateMultiLevelLayout forcedStr model.PolygonExport.EntryStr [||] (Some sqn) (Some model.PolygonExport.OuterStr) (Some model.PolygonExport.IslandsStr)
                    let cxls = cxls |> Array.map (fun (c: Cxl) -> c)
                    let derived = PageHelpers.deriveDataFromLayout cxls cxOuIl cxElv1 model.Tree.ActiveLevel
                    let (d: {| shapes: {| color: string; points: float[]; name: string; lx: float; ly: float |}[]; w: float; h: float |}) = 
                        Layout.getStaticGeometry cxls derived.cxClr1 model.Tree.ActiveLevel 10 
                    
                    let configData : BatchConfgrtns = 
                        {| sqnName = sqnStr
                           shapes = d.shapes |> Array.map (fun s -> 
                             {| color = s.color; points = s.points; name = s.name; lx = s.lx; ly = s.ly |}) 
                           w = d.w; h = d.h
                           cxCxl1 = cxls
                           cxElv1 = cxElv1
                           cxlAvl = derived.cxlAvl
                           cxOuIl = cxOuIl
                           cxAdj1 = derived.cxAdj1
                           cxB36 = derived.cxB36 |}
                    
                    do! Async.Sleep 5
                    return Some configData
                with ex -> 
                    return None
            }) () AddBatchItem

    | AddBatchItem res ->
        let nextI = model.BatchProgress + 1
        let nextAcc = 
            match res with
            | Some r -> r :: model.BatchAccumulator
            | None -> model.BatchAccumulator
        // Update progress state and trigger next iteration
        { model with BatchProgress = nextI; BatchAccumulator = nextAcc }, Cmd.ofMsg (GenerateNextBatchItem nextI)
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
        Storage.saveFile js model.SrcOfTrth |> ignore
        Storage.autoSave js model.SrcOfTrth |> ignore
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
        match UpdateTeach.update js msg model with
        | Some (newModel, cmd) -> newModel, cmd
        | None -> model, Cmd.none

    | LoadBackup content ->
        if String.IsNullOrWhiteSpace content then model, Cmd.none
        else
            try
                // Extract Sequences (Q=...)
                let regex = System.Text.RegularExpressions.Regex(@"Q=([A-Z]+)")
                let newSqns = 
                    content.Split(';', System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.mapi (fun i segment ->
                        let m = regex.Match(segment)
                        if m.Success then (i, m.Groups.[1].Value) else (i, (model.Sequences |> Map.tryFind i |> Option.defaultValue allSqns.[11])))
                    |> Map.ofArray
                
                // Restore Tree
                let newTree = NodeCode.initModel content
                
                // Restore Polygon
                let currentInner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
                let newState = Storage.importFromHyw content currentInner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                let newExport = syncPolygonState finalPoly
                
                let updatedModel = 
                    { model with 
                        SrcOfTrth = content
                        Tree = newTree
                        LastValidTree = newTree
                        PolygonEditor = newState
                        PolygonExport = newExport
                        Sequences = newSqns
                        Derived = PageHelpers.deriveData content newExport.EntryStr newTree.ActiveLevel
                        NeedsHyweave = true
                        Onboarding = { model.Onboarding with IsActive = true }
                        IsPresetsCollapsed = true
                    }
                updatedModel, Cmd.none
            with _ ->
                // If backup is malformed or incompatible, ignore it to prevent startup hang
                model, Cmd.none

    | HardReset ->
        Storage.clearBackup js |> ignore
        let model = pushUndo model
        let resetSyntax = Page.emptyState
        let resetTree = NodeCode.initModel resetSyntax
        let resetPoly = PolygonEditor.initModel
        let resetExport = syncPolygonState resetPoly
        
        { model with 
            SrcOfTrth = resetSyntax
            Tree = resetTree
            LastValidTree = resetTree
            PolygonEditor = Stable resetPoly
            PolygonExport = resetExport
            Sequences = Map.ofList [0, allSqns.[11]]
            Derived = PageHelpers.deriveData resetSyntax resetExport.EntryStr 0
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
                                Derived      = PageHelpers.deriveData snap.SrcOfTrth newExport.EntryStr snap.Tree.ActiveLevel
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
                                Derived      = PageHelpers.deriveData snap.SrcOfTrth newExport.EntryStr snap.Tree.ActiveLevel
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
            IsPresetsCollapsed = if isFinished then false else model.IsPresetsCollapsed
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
        { model with Onboarding = { model.Onboarding with IsActive = false; IsAutoSimulating = false }; IsPresetsCollapsed = false }, 
        Cmd.map TreeMsg (Cmd.ofMsg NodeCode.CancelAction) // Just in case

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
            Onboarding = { model.Onboarding with IsActive = true }
            IsPresetsCollapsed = true 
        }, Cmd.none

    | TogglePresetsCollapse ->
        { model with IsPresetsCollapsed = not model.IsPresetsCollapsed }, Cmd.none

    | ToggleHelpCollapse ->
        { model with IsHelpCollapsed = not model.IsHelpCollapsed }, Cmd.none

    | ToggleConfirm action ->
        { model with PendingConfirm = action }, Cmd.none

open Help

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
        t

    interface System.IDisposable with
        member _.Dispose() =
            _dotnetRef |> Option.iter (fun r -> r.Dispose())

    override this.OnInitialized() =
        base.OnInitialized()

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.batch [
                Cmd.OfAsync.perform (fun () -> Storage.getBackup this.JSRuntime) () (fun res -> if String.IsNullOrEmpty res then NoOp else LoadBackup res)
                Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 1000 }) () (fun _ -> TransitionToIntro)
                Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 3000 }) () (fun _ -> TransitionToMain)
                Cmd.OfAsync.perform (fun () -> async { updateMetadata this.JSRuntime; return () }) () (fun _ -> NoOp)
            ])
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> 
                concat {
                    if model.Onboarding.IsActive && model.CurrentScreen = MainScreen then
                        Help.viewHelp model.Onboarding dispatch

                    Styles.render()
                    Shell.jsonLd
                    Shell.siteHeader
                    Shell.aboutSection
                    div {
                        attr.id "page-content"
                        if model.CurrentScreen <> LoadingScreen then
                            attr.``class`` "fade-container fade-in"
                        else
                            attr.``class`` "fade-container"
                        
                        Shell.introSplash model.CurrentScreen dispatch

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
                                attr.style "position: fixed; top: 75px; right: 10px; z-index: 5000; background: #363636; color: white; padding: 15px 18px; border-radius: 6px; box-shadow: 0 8px 24px rgba(0,0,0,0.3); font-size: 13px; max-width: 220px; display: flex; flex-direction: column; gap: 10px; border: 1px solid rgba(255,255,255,0.1); animation: slideIn 0.3s ease-out;"
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

                        Shell.siteFooter model.CurrentScreen
                    }
                    Shell.loadingScreen model.CurrentScreen
                }
            )
