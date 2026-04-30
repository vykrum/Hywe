module Hywe.UpdateUI

open Elmish
open Microsoft.JSInterop
open ModelTypes
open Layout
open Page
open PolygonEditor
open Storage
open ExportFormats
open System

let handleSetActivePanel (model: Model) (panel: ActivePanel) : Model * Cmd<Message> =
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
                    BatchProgress = 0
                    BatchAccumulator = []
                }
            model', Cmd.ofMsg (GenerateNextBatchItem 0)
        | false -> 
            { model with ActivePanel = panel }, Cmd.none
    | BoundaryPanel | LayoutPanel | AnalyzePanel | ViewPanel | TeachPanel ->
        { model with ActivePanel = panel }, Cmd.none

let handleToggleEditorMode (model: Model) : Model * Cmd<Message> =
    match model.EditorMode with
    | Syntax ->
        let maybeSubModel =
            model.SrcOfTrth
            |> CodeNode.preprocessCode
            |> fun processed ->
                try Some (NodeCode.initModel processed)
                with _ -> None

        match maybeSubModel with
        | Some subModel ->
            let newOutput =
                NodeCode.getOutput
                    subModel
                    model.Sequence
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr

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
                model.PolygonExport.Width
                model.PolygonExport.Height
                model.PolygonExport.AbsStr
                model.PolygonExport.OuterStr
                model.PolygonExport.IslandsStr

        { model with
            SrcOfTrth = newOutput
            LastValidTree = model.Tree
            EditorMode = Syntax
            ParseError = false
        }, Cmd.none

let handleExportPdfRequested (model: Model) : Model * Cmd<Message> =
    { model with IsHyweaving = true; BatchProgress = 0; BatchAccumulator = [] }, 
    Cmd.ofMsg (GenerateNextBatchItem 0)

let handleFileImported (model: Model) (content: string) (js: IJSRuntime) : Model * Cmd<Message> =
    match System.String.IsNullOrWhiteSpace content with
    | true -> model, Cmd.none
    | false ->
        let clean = content.Trim()
        let newTree = 
            clean 
            |> CodeNode.preprocessCode 
            |> fun processed ->
                try NodeCode.initModel processed
                with _ -> model.Tree 

        let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
        let newState = Parse.importFromHyw clean inner
        let finalPoly = match newState with FreshlyImported m | Stable m -> m
        let newExport = syncPolygonState finalPoly

        { model with 
            SrcOfTrth = clean
            Tree = newTree
            LastValidTree = newTree
            Derived = Page.deriveData clean model.PolygonExport.EntryStr 0 
            PolygonEditor = newState 
            PolygonExport = newExport
            ParseError = false
            LastBatchSrc = None
        }, 
        Cmd.batch [
            Cmd.ofMsg (PolygonEditorUpdated finalPoly)
            Cmd.OfTask.attempt (fun () -> task { 
                do! js.InvokeVoidAsync("localStorageSet", "hywe_backup", clean).AsTask() 
            }) () (fun _ -> FinishHyweave)
        ]

let update (js: IJSRuntime) (msg: Message) (model: Model) : (Model * Cmd<Message>) option =
    match msg with
    | SetActivePanel panel -> Some (handleSetActivePanel model panel)
    | ToggleEditorMode -> Some (handleToggleEditorMode model)
    | ExportPdfRequested -> Some (handleExportPdfRequested model)
    | FileImported content -> Some (handleFileImported model content js)
    | Message.ToggleBoundary ->
        match model.ActivePanel with
        | BoundaryPanel -> 
            Some ({ model with ActivePanel = LayoutPanel }, Cmd.none)
        | _ -> 
            let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            let newState = Parse.importFromHyw model.SrcOfTrth inner
            let finalPoly = match newState with FreshlyImported m | Stable m -> m
            let newM = { model with ActivePanel = BoundaryPanel; PolygonEditor = newState }
            Some (newM, Cmd.ofMsg (PolygonEditorUpdated finalPoly))
    | ToggleViewLock ->
        Some ({ model with ViewLocked = not model.ViewLocked }, Cmd.none)
    | Download3DSvg ->
        let datePart = System.DateTime.Now.ToString("yyMMddmm")
        let fileName = "Hywe3D_" + datePart + ".svg"
        Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("export3DToSVG", "hywe-extruded-polygon", fileName).AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
    | DownloadCsv ->
        let csv = ExportFormats.generateCsv model.Derived.cxCxl1 model.Sequence
        let fileName = "Hywe_Analysis_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
        Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
    | DownloadDxf ->
        let dxf = ExportFormats.generateDxf model.Derived.cxCxl1 0.0 0.0
        let fileName = "Hywe_Layout_" + DateTime.Now.ToString("yyMMddHHmm") + ".dxf"
        Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, dxf, "application/dxf").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
    | DownloadObj ->
        let vOff = ref 1
        let objStr = ExportFormats.generateObj model.Derived.cxCxl1 model.Derived.cxElv1 0.0 0.0 vOff
        let fileName = "Hywe_3D_" + DateTime.Now.ToString("yyMMddHHmm") + ".obj"
        Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, objStr, "model/obj").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
    | DownloadBatchDxf ->
        match model.BatchPreview with
        | Some results ->
            let batchData = results |> Array.toList |> List.map (fun r -> r.cxCxl1)
            let dxf = ExportFormats.generateDxfBatch batchData
            let fileName = "Hywe_Batch_FloorPlates_" + DateTime.Now.ToString("yyMMddHHmm") + ".dxf"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, dxf, "application/dxf").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
    | DownloadBatchObj ->
        match model.BatchPreview with
        | Some results ->
            let batchData = results |> Array.toList |> List.map (fun r -> r.cxCxl1, r.cxElv1)
            let objStr = ExportFormats.generateObjBatch batchData
            let fileName = "Hywe_Batch_3D_" + DateTime.Now.ToString("yyMMddHHmm") + ".obj"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, objStr, "model/obj").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
    | _ -> None
