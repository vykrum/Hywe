module PageHelpers

open System
open Elmish
open Microsoft.JSInterop
open ModelTypes
open Hywe.Core
open Hywe.Core.Coxel
open Hywe.Core.Lexel
open Hywe.Site
open Page
open Hywe.Node

// --- Logic ---

let toMarker lvl = if lvl = 0 then "L0" else sprintf "L%d" lvl

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
                    BatchProgress = 0
                    IsPresetsCollapsed = true
                    IsWorkspaceCollapsed = true
                }
            model', Cmd.ofMsg (GenerateNextBatchItem 0)
        | false -> 
            { model with ActivePanel = panel; IsPresetsCollapsed = true; IsWorkspaceCollapsed = true }, Cmd.none
    | BoundaryPanel | LayoutPanel | AnalyzePanel | ViewPanel | TeachPanel | ReportPanel ->
        { model with ActivePanel = panel; IsPresetsCollapsed = true; IsWorkspaceCollapsed = true }, Cmd.none

let handleToggleEditorMode (model: Model) : Model * Cmd<Message> =
    match model.EditorMode with
    | Syntax ->
        let maybeSubModel =
            model.SrcOfTrth
            |> Serialization.preprocessCode
            |> fun processed ->
                try Some (Serialization.initModel processed)
                with _ -> None

        match maybeSubModel with
        | Some subModel ->
            let newOutput =
                Serialization.getOutput
                    subModel
                    model.Sequences
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.BaseStr
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
            Serialization.getOutput
                model.Tree
                model.Sequences
                model.PolygonExport.Width
                model.PolygonExport.Height
                model.PolygonExport.AbsStr
                model.PolygonExport.BaseStr
                model.PolygonExport.OuterStr
                model.PolygonExport.IslandsStr

        { model with
            SrcOfTrth = newOutput
            LastValidTree = model.Tree
            EditorMode = Syntax
            ParseError = false
        }, Cmd.none

let handleExportPdfRequested (model: Model) : Model * Cmd<Message> =
    { model with IsHyweaving = true; BatchProgress = 0; LayoutCache = Map.empty; ReportBatch = Map.empty }, 
    Cmd.ofMsg (GenerateNextBatchItem 0)

let handleFileImported (model: Model) (content: string) (js: IJSRuntime) : Model * Cmd<Message> =
    match System.String.IsNullOrWhiteSpace content with
    | true -> model, Cmd.none
    | false ->
        let clean = content.Trim()
        let newTree = 
            clean 
            |> Serialization.preprocessCode 
            |> fun processed ->
                try Serialization.initModel processed
                with _ -> model.Tree 

        let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
        let newState = FileManager.importFromHyw clean inner
        let finalPoly = match newState with Stable m | FreshlyImported m -> m
        let newExport = syncPolygonState finalPoly
        let newSqns = extractSequences clean

        { model with 
            SrcOfTrth = clean
            Tree = newTree
            LastValidTree = newTree
            Derived = Cache.deriveFromSource clean newSqns newExport model.Tree.ActiveLevel
            PolygonEditor = newState 
            PolygonExport = newExport
            ParseError = false
            LastBatchSrc = None
            Sequences = newSqns
            EditsCount = 0
            PendingConfirm = None
        }, 
        Cmd.batch [
            Cmd.ofMsg (PolygonEditorUpdated finalPoly)
            Cmd.ofMsg StartHyweave
        ]

let update (js: IJSRuntime) (msg: Message) (model: Model) : (Model * Cmd<Message>) option =
    match msg with
    | SetActivePanel panel -> Some (handleSetActivePanel model panel)
    | ToggleEditorMode -> Some (handleToggleEditorMode model)
    | SetSrcOfTrth src -> Some ({ model with SrcOfTrth = src; SelectedPreset = None }, Cmd.none)
    | SelectPreset name ->
        let content = 
            match name with 
            | "Simple" -> beeyond 
            | "Branched" -> beedroom 
            | "Stacked" -> stacked 
            | _ -> ""
        let (nextModel, cmd) = handleFileImported model content js
        Some ({ nextModel with SelectedPreset = Some name; EditsCount = 0 }, cmd)
    | FileImported content -> Some (handleFileImported model content js)
    | Message.ToggleBoundary ->
        match model.ActivePanel with
        | BoundaryPanel -> 
            Some ({ model with ActivePanel = LayoutPanel }, Cmd.none)
        | _ -> 
            let inner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            let newState = FileManager.importFromHyw model.SrcOfTrth inner
            let finalPoly = match newState with Stable m | FreshlyImported m -> m
            let newM = { model with ActivePanel = BoundaryPanel; PolygonEditor = newState }
            Some (newM, Cmd.ofMsg (PolygonEditorUpdated finalPoly))
    | ToggleViewLock ->
        let isLocking = not model.ViewLocked
        let newModel = { model with ViewLocked = isLocking; Captured3DImage = if isLocking then model.Captured3DImage else None }
        let cmd = 
            if isLocking then
                Cmd.OfAsync.perform (fun () -> js.InvokeAsync<string>("captureCanvasSVG", "hywe-extruded-polygon").AsTask() |> Async.AwaitTask) () ViewCaptured
            else Cmd.none
        Some (newModel, cmd)
    | Download3DPng ->
        let datePart = System.DateTime.Now.ToString("yyMMddmm")
        let fileName = "Hywe3D_" + datePart + ".png"
        let downloadCmd = 
            Cmd.OfAsync.perform (fun () -> 
                async {
                    let! pngDataUrl = js.InvokeAsync<string>("captureCanvasWebGPU", "hywe-extruded-polygon").AsTask() |> Async.AwaitTask
                    if not (System.String.IsNullOrEmpty(pngDataUrl)) then
                        // The string returned is already a Data URL (base64) so we just trigger a download directly
                        let jsCode = sprintf "var a = document.createElement('a'); a.href = '%s'; a.download = '%s'; document.body.appendChild(a); a.click(); document.body.removeChild(a);" pngDataUrl fileName
                        do! js.InvokeVoidAsync("eval", jsCode).AsTask() |> Async.AwaitTask
                }) () (fun _ -> NoOp)
        Some (model, downloadCmd)
    
    | DownloadCoordCsv ->
        let sqnStr = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue allSqns.[11]
        let activeIndex = allSqns |> List.tryFindIndex ((=) sqnStr) |> Option.defaultValue 11
        let baseLevel = match model.Tree.ActiveNest with | Some n -> (Map.find n model.Tree.Nests).Level | None -> model.Tree.ActiveLevel
        match Cache.get (toMarker baseLevel) activeIndex model.LayoutCache with
        | Some c -> 
            let activeConfig = Page.TreeFiltering.filterBatchConfig true model.Tree c
            let level = model.Tree.ActiveLevel
            let levelIdx = 
                activeConfig.cxCxl1 
                |> Array.indexed 
                |> Array.filter (fun (_, (c: Cxl)) -> let (_, _, z) = Hexel.hxlCrd c.Base in z = level)
                |> Array.map fst
            let cxls = levelIdx |> Array.map (fun i -> activeConfig.cxCxl1.[i])
            let csv = FileManager.generateCoordinatesCsv [| (sqnStr, level, cxls) |]
            let fileName = "Hywe_Coords_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
        
    | DownloadMetricsCsv ->
        let sqnStr = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue allSqns.[11]
        let activeIndex = allSqns |> List.tryFindIndex ((=) sqnStr) |> Option.defaultValue 11
        let baseLevel = match model.Tree.ActiveNest with | Some n -> (Map.find n model.Tree.Nests).Level | None -> model.Tree.ActiveLevel
        match Cache.get (toMarker baseLevel) activeIndex model.LayoutCache with
        | Some c -> 
            let activeConfig = Page.TreeFiltering.filterBatchConfig true model.Tree c
            let activeCxls = activeConfig.cxCxl1 |> Array.filter (fun (c: Cxl) -> let (_, _, z) = Hexel.hxlCrd c.Base in z = model.Tree.ActiveLevel)
            let csv = FileManager.generateAreaMetricsCsv [| (sqnStr, model.Tree.ActiveLevel, activeCxls) |]
            let fileName = "Hywe_Metrics_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
        
    | DownloadAdjCsv ->
        let sqnStr = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue allSqns.[11]
        let activeIndex = allSqns |> List.tryFindIndex ((=) sqnStr) |> Option.defaultValue 11
        let baseLevel = match model.Tree.ActiveNest with | Some n -> (Map.find n model.Tree.Nests).Level | None -> model.Tree.ActiveLevel
        match Cache.get (toMarker baseLevel) activeIndex model.LayoutCache with
        | Some c -> 
            let activeConfig = Page.TreeFiltering.filterBatchConfig true model.Tree c
            let csv = FileManager.generateAdjacencyCsv [| (sqnStr, model.Tree.ActiveLevel, activeConfig.cxAdj1) |]
            let fileName = "Hywe_Adjacency_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
    | DownloadBatchCoordCsv ->
        let rawResults = Cache.getAllVariations (toMarker model.Tree.ActiveLevel) model.LayoutCache
        let results = rawResults |> Array.map (Page.TreeFiltering.filterBatchConfig true model.Tree)
            
        if results.Length > 0 then
            let batchData = results |> Array.collect (fun r -> 
                r.cxCxl1 
                |> Array.indexed
                |> Array.groupBy (fun (_, (c: Cxl)) -> let (_, _, z) = Hexel.hxlCrd c.Base in z) 
                |> Array.map (fun (z, indexedCxls) -> 
                    let cxls = indexedCxls |> Array.map (snd)
                    (r.sqnName, z, cxls))
            )
            let csv = FileManager.generateCoordinatesCsv batchData
            let fileName = "Hywe_Batch_Coords_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        else Some (model, Cmd.none)
    | DownloadBatchMetricsCsv ->
        let rawResults = Cache.getAllVariations (toMarker model.Tree.ActiveLevel) model.LayoutCache
        let results = rawResults |> Array.map (Page.TreeFiltering.filterBatchConfig true model.Tree)

        if results.Length > 0 then
            let batchData = results |> Array.collect (fun r -> 
                r.cxCxl1 
                |> Array.groupBy (fun (c: Cxl) -> let (_, _, z) = Hexel.hxlCrd c.Base in z) 
                |> Array.map (fun (z, cxls) -> (r.sqnName, z, cxls))
            )
            let csv = FileManager.generateAreaMetricsCsv batchData
            let fileName = "Hywe_Batch_Metrics_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        else Some (model, Cmd.none)
    | DownloadBatchAdjCsv ->
        let rawResults = Cache.getAllVariations (toMarker model.Tree.ActiveLevel) model.LayoutCache
        let results = rawResults |> Array.map (Page.TreeFiltering.filterBatchConfig true model.Tree)

        if results.Length > 0 then
            let batchData = results |> Array.map (fun r -> 
                (r.sqnName, 0, r.cxAdj1) // Using 0 as a placeholder level for batch adjacency
            )
            let csv = FileManager.generateAdjacencyCsv batchData
            let fileName = "Hywe_Batch_Adjacency_" + DateTime.Now.ToString("yyMMddHHmm") + ".csv"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, csv, "text/csv").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        else Some (model, Cmd.none)
    | DownloadDxf ->
        let sqnStr = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue allSqns.[11]
        let activeIndex = allSqns |> List.tryFindIndex ((=) sqnStr) |> Option.defaultValue 11
        let baseLevel = match model.Tree.ActiveNest with | Some n -> (Map.find n model.Tree.Nests).Level | None -> model.Tree.ActiveLevel
        match Cache.get (toMarker baseLevel) activeIndex model.LayoutCache with
        | Some c -> 
            let activeConfig = Page.TreeFiltering.filterBatchConfig true model.Tree c
            let dxf = FileManager.generateDxf activeConfig.cxCxl1 0.0 0.0
            let fileName = "Hywe_Layout_" + DateTime.Now.ToString("yyMMddHHmm") + ".dxf"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, dxf, "application/dxf").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
        
    | DownloadObj ->
        let sqnStr = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue allSqns.[11]
        let activeIndex = allSqns |> List.tryFindIndex ((=) sqnStr) |> Option.defaultValue 11
        let baseLevel = match model.Tree.ActiveNest with | Some n -> (Map.find n model.Tree.Nests).Level | None -> model.Tree.ActiveLevel
        match Cache.get (toMarker baseLevel) activeIndex model.LayoutCache with
        | Some c -> 
            let activeConfig = Page.TreeFiltering.filterBatchConfig true model.Tree c
            let _, objStr = FileManager.generateObj activeConfig.cxCxl1 activeConfig.cxElv1 0.0 0.0 1
            let fileName = "Hywe_3D_" + DateTime.Now.ToString("yyMMddHHmm") + ".obj"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, objStr, "model/obj").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        | None -> Some (model, Cmd.none)
    | DownloadBatchDxf ->
        let rawResults = Cache.getAllVariations (toMarker model.Tree.ActiveLevel) model.LayoutCache
        let results = rawResults |> Array.map (Page.TreeFiltering.filterBatchConfig true model.Tree)

        if results.Length > 0 then
            let batchData = results |> Array.toList |> List.map (fun r -> r.cxCxl1)
            let dxf = FileManager.generateDxfBatch batchData
            let fileName = "Hywe_Batch_FloorPlates_" + DateTime.Now.ToString("yyMMddHHmm") + ".dxf"
            Some (model, Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("downloadFile", fileName, dxf, "application/dxf").AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
        else Some (model, Cmd.none)
    | UpdateReportOptions updateFn ->
        Some ({ model with ReportOptions = updateFn model.ReportOptions }, Cmd.none)
    | GenerateReport ->
        let currentSrc =
            match model.EditorMode with
            | Interactive ->
                Serialization.getOutput
                    model.Tree
                    model.Sequences
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.BaseStr
                    model.PolygonExport.OuterStr
                    model.PolygonExport.IslandsStr
            | Syntax -> model.SrcOfTrth
            
        Some ({ model with IsGeneratingReport = true; SrcOfTrth = currentSrc },
              Cmd.OfAsync.perform (fun () -> async {
                  do! Async.Sleep 50
                  
                  let mutable currentCache = model.LayoutCache
                  let mutable allBatches = Map.empty
                  
                  let levelMarkers = model.Tree.Levels.Keys |> Seq.map (fun lvl -> if lvl = 0 then "L0" else sprintf "L%d" lvl) |> Seq.toList
                  let nestMarkers = model.Tree.Nests.Keys |> Seq.map (sprintf "N%d") |> Seq.toList
                  let allMarkers = levelMarkers @ nestMarkers
                  
                  for marker in allMarkers do
                      do! js.InvokeVoidAsync("console.log", sprintf "Hywe: Processing %s" marker).AsTask() |> Async.AwaitTask
                      let section = 
                          match Map.tryFind marker model.ReportOptions.LevelSections with
                          | Some s -> s
                          | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
                      
                      let mutable levelBatches = []
                      
                      if section.BatchOverview || section.Variations || section.FlowChart then
                          do! js.InvokeVoidAsync("console.log", sprintf "Hywe: Generating layout data for %s..." marker).AsTask() |> Async.AwaitTask
                          let range = if section.BatchOverview || section.Variations then [0..23] else [11] // Use index 11 as default for coloring
                          
                          let baseLevel = 
                              if marker.StartsWith("N") then
                                  let nestId = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
                                  match model.Tree.Nests |> Map.tryFind nestId with
                                  | Some n -> n.Level
                                  | None -> 0
                              else
                                  match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0

                          for i in range do
                              try
                                  let config = 
                                      match Cache.get (toMarker baseLevel) i currentCache with
                                      | Some c -> c
                                      | None -> 
                                          // Compute full data once, and update cache for ALL levels
                                          let srcForBatch = ensureCategory currentSrc i
                                          let fullData = Cache.computeFullLayout srcForBatch Hexel.sqnArray.[i] model.PolygonExport baseLevel
                                          for l in model.Tree.Levels.Keys do
                                              let cfg = Cache.fromFullLayout fullData Hexel.sqnArray.[i] l model.PolygonExport
                                              currentCache <- Cache.update (toMarker l) i cfg currentCache
                                          
                                          Cache.fromFullLayout fullData Hexel.sqnArray.[i] baseLevel model.PolygonExport

                                  let filteredConfig = Page.TreeFiltering.filterBatchConfigForMarker true model.Tree marker config
                                  levelBatches <- filteredConfig :: levelBatches
                              with _ -> ()
                              
                      allBatches <- Map.add marker (levelBatches |> List.rev |> List.toArray) allBatches
                      
                  do! js.InvokeVoidAsync("console.log", "Hywe: Compiling final HTML report...").AsTask() |> Async.AwaitTask
                  let opts = { model.ReportOptions with Captured3DImage = model.Captured3DImage }
                  let html = Hywe.Report.generateReportHtml opts model.Tree allBatches
                  do! js.InvokeVoidAsync("console.log", sprintf "Hywe: Report compiled. HTML size: %d bytes" html.Length).AsTask() |> Async.AwaitTask
                  return html, currentCache
              }) () (fun (html, cache) -> ReportGenerated (html, cache)))
    | ReportGenerated (html, cache) ->
        Some ({ model with IsGeneratingReport = false; LayoutCache = cache },
              Cmd.OfAsync.perform (fun () -> js.InvokeVoidAsync("openReport", html).AsTask() |> Async.AwaitTask) () (fun _ -> NoOp))
    | ToggleCoords -> Some ({ model with IsCoordsVisible = not model.IsCoordsVisible }, Cmd.none)
    | _ -> None
