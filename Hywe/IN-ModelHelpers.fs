module Helper

open Microsoft.JSInterop
open Elmish
open Layout
open Page
open NodeCode
open PolygonEditor
open ModelTypes

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
                }

            model', Cmd.OfAsync.perform (fun (token: System.Threading.CancellationToken) ->
                let rec compute (m: Model) i acc = async {
                    if i >= 24 || token.IsCancellationRequested then 
                        return acc
                    else
                        let sqnStr = indexToSqn i
                        let forcedStr = injectSqn m.SrcOfTrth sqnStr
                        let cxls, _ = Parse.spaceCxl [||] forcedStr
                        let d = getStaticGeometry cxls (deriveData forcedStr 0).cxClr1 0 10 
                        
                        let configData = 
                            {| sqnName = sqnStr; w = d.w; h = d.h
                               shapes = d.shapes |> Array.map (fun s -> 
                                 {| color = s.color; points = s.points; name = s.name; lx = s.lx; ly = s.ly |}) 
                            |}
                        
                        do! Async.Sleep 5
                        return! compute m (i + 1) (configData :: acc)
                }
                async {
                    let! results = compute model 0 []
                    return results |> List.toArray |> Array.rev
                }) cts.Token SetBatchPreview

        | false -> 
            { model with ActivePanel = panel }, Cmd.none

    | BoundaryPanel | LayoutPanel | TablePanel | ViewPanel | TeachPanel ->
        { model with ActivePanel = panel }, Cmd.none

let handleToggleEditorMode (model: Model) : Model * Cmd<Message> =
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
                    model.PolygonExport.Width
                    model.PolygonExport.Height
                    model.PolygonExport.AbsStr
                    model.PolygonExport.EntryStr
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
                model.PolygonExport.EntryStr
                model.PolygonExport.OuterStr
                model.PolygonExport.IslandsStr

        { model with
            SrcOfTrth = newOutput
            LastValidTree = model.Tree
            EditorMode = Syntax
            ParseError = false
        }, Cmd.none

let handleRecordToHynteract (model: Model) (js: IJSRuntime) : Model * Cmd<Message> =
    let currentSrc = model.SrcOfTrth
    let currentDesc = model.UserDescription
    let currentOuter = model.PolygonExport.OuterStr
    let currentIslands = model.PolygonExport.IslandsStr

    { model with IsSavingToHynteract = true },

    Cmd.OfAsync.perform (fun () -> async {
        try
            let configMap = 
                Hexel.sqnArray 
                |> Array.map (fun sqnCase -> 
                    let key = sprintf "%A" sqnCase
                    let data = 
                        try 
                            Parse.generateCxlArray currentSrc sqnCase currentOuter currentIslands [||]
                        with ex -> 
                            printfn "Warning: Orientation %s failed Dataset Generation: %s" key ex.Message
                            "" 
                    key, data)
                |> Map.ofArray

            let payload = {| 
                instruction = currentSrc 
                description = currentDesc 
                configuration = configMap 
            |}

            let! success = 
                js.InvokeAsync<bool>("recordToHynteract", "https://hynteract.vercel.app/api/record", payload).AsTask() 
                |> Async.AwaitTask
            
            return success
        
        with ex -> 
            printfn "Critical Recording Failure: %s" ex.Message
            return false
    }) () RecordResult

let handleExportPdfRequested (model: Model) : Model * Cmd<Message> =
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

let handleFileImported (model: Model) (content: string) (js: IJSRuntime) : Model * Cmd<Message> =
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
        let newExport = syncPolygonState finalPoly

        { model with 
            SrcOfTrth = clean
            Tree = newTree
            LastValidTree = newTree
            Derived = deriveData clean 0 
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
