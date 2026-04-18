module Hywe.Main

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Page
open NodeCode
open PolygonEditor
open ModelTypes
open ModelHelpers

// Defaults / init 
let initialTree = NodeCode.initModel beeyond
let initialSequence = allSqns.[11]
let initialPolygonExport = syncPolygonState PolygonEditor.initModel

let initialOutput = NodeCode.getOutput
                        initialTree
                        initialSequence
                        initialPolygonExport.Width
                        initialPolygonExport.Height
                        initialPolygonExport.AbsStr
                        initialPolygonExport.EntryStr
                        initialPolygonExport.OuterStr
                        initialPolygonExport.IslandsStr

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
        UserDescription = ""
        IsSavingToHynteract = false
        ShowSuccessMessage = false
        IsRecording = false
        PolygonExport = initialPolygonExport
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
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.EntryStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr
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
                model.SrcOfTrth
            | Interactive ->
                NodeCode.getOutput
                    model.Tree
                    model.Sequence
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.EntryStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr

        Storage.autoSave js updatedSrcOfTrth |> ignore

        let newModel =
            match model.EditorMode with
            | Syntax ->
                let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
                let newState = Parse.importFromHyw updatedSrcOfTrth inner
                let finalPoly = match newState with Stable m | FreshlyImported m -> m
                let newExport = syncPolygonState finalPoly
                { model with 
                    SrcOfTrth = updatedSrcOfTrth
                    Derived = deriveData updatedSrcOfTrth elv
                    PolygonEditor = newState 
                    PolygonExport = newExport }
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
            let updatedTree, treeCmd = NodeCode.updateSub subMsg model.Tree 
        
            let newOutput = NodeCode.getOutput
                                updatedTree
                                model.Sequence
                                model.PolygonExport.Width
                                model.PolygonExport.Height
                                model.PolygonExport.AbsStr
                                model.PolygonExport.EntryStr
                                model.PolygonExport.OuterStr
                                model.PolygonExport.IslandsStr
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
        let newExport = syncPolygonState newModel
        { model with 
            PolygonEditor = Stable newModel
            PolygonExport = newExport
            NeedsHyweave = true        },
            Cmd.none

    | SetActivePanel panel -> handleSetActivePanel model panel
    | ToggleEditorMode -> handleToggleEditorMode model
    | ExportPdfRequested -> handleExportPdfRequested model
    | RecordToHynteract -> handleRecordToHynteract model js
    | FileImported content -> handleFileImported model content js
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
    | SetDescription d -> 
        { model with UserDescription = d }, Cmd.none
    | RecordResult success ->
        { model with 
            IsSavingToHynteract = false
            ShowSuccessMessage = success
            UserDescription = if success then "" else model.UserDescription 
        }, 
        if success then 
            Cmd.OfAsync.perform (fun () -> Async.Sleep 3000) () (fun _ -> StartHyweave)
        else Cmd.none
    | StartVoiceCapture -> 
        { model with IsRecording = true }, 
        Cmd.OfAsync.perform (fun () -> 
            async {
                do! js.InvokeVoidAsync("startTranscription").AsTask() |> Async.AwaitTask
                return ()
            }) () (fun _ -> OnVoiceResult model.UserDescription)
    | OnVoiceResult text ->
        { model with IsRecording = false }, Cmd.none

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.none)
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> view model dispatch this.JSRuntime)
