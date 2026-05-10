module Hywe.UpdateTeach

open System
open Microsoft.JSInterop
open Elmish
open PolygonEditor
open ModelTypes
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open FileManager
open Cache

let generateSuggestion (model: Model) =
    let tree = model.Tree
    let meta = model.TeachMetadata
    
    // Maps Level -> Parent Anchor Guid
    let levelToAnchor = tree.LevelAnchors 

    let rec getTreeSummary (node: NodeCode.TreeNode) =
        if node.Children.IsEmpty then ""
        else
            let childNames = node.Children |> List.map (fun c -> c.Name) |> String.concat ", "
            let verb = if node.Children.Length > 1 then "branches into" else "leads to"
            let current = sprintf "The %s %s %s." node.Name verb childNames
            let children = node.Children |> List.map getTreeSummary |> String.concat " "
            current + " " + children

    let describeLevel (level: int) (root: NodeCode.TreeNode) =
        let header = 
            if level = 0 then "\nBase Level: "
            else 
                match levelToAnchor |> Map.tryFind level with
                | Some anchorId -> 
                    sprintf "\nLevel %d (Elevated): " level
                | None -> sprintf "\nLevel %d: " level
        
        let body = getTreeSummary root
        if String.IsNullOrWhiteSpace body then header + sprintf "Starting from %s." root.Name
        else header + body

    let intro = sprintf "This is a %s stage %s %s project with a %s flow and %s ambience. " 
                    (meta.Stage.ToLower()) (meta.Scale.ToLower()) (meta.Typology.ToLower()) (meta.Flow.ToLower()) (meta.Ambience.ToLower())

    let levelsContent = 
        tree.Levels 
        |> Map.toList 
        |> List.sortBy fst
        |> List.map (fun (lvl, root) -> describeLevel lvl root)
        |> String.concat "\n"

    (intro + "\n" + levelsContent).Trim()

let update (js: IJSRuntime) (msg: Message) (model: Model) : (Model * Cmd<Message>) option =
    match msg with
    | SetDescription d -> 
        Some ({ model with UserDescription = d }, Cmd.none)
    | SuggestDescription -> 
        Some ({ model with UserDescription = generateSuggestion model }, Cmd.none)
    | RecordResult (success, cache) ->
        let newModel = 
            { model with 
                IsSavingToHynteract = false
                ShowSuccessMessage = success
                UserDescription = if success then "" else model.UserDescription 
                LayoutCache = cache
            }
        let cmd = 
            if success then 
                Cmd.OfAsync.perform (fun () -> Async.Sleep 3000) () (fun _ -> StartHyweave)
            else Cmd.none
        Some (newModel, cmd)
    | UpdateMetadata f ->
        Some ({ model with TeachMetadata = f model.TeachMetadata }, Cmd.none)
    | SetHoveredInfo info ->
        Some ({ model with HoveredInfo = info }, Cmd.none)
    | StartVoiceCapture -> 
        let newModel = { model with IsRecording = true }
        let cmd = 
            Cmd.OfAsync.perform (fun () -> 
                async {
                    do! Teach.startTranscription js "hynteract-desc-input"
                    return ()
                }) () (fun _ -> OnVoiceResult)
        Some (newModel, cmd)
    | OnVoiceResult ->
        Some ({ model with IsRecording = false }, Cmd.none)
    | RecordToHynteract ->
        let currentSrc = model.SrcOfTrth
        let currentDesc = model.UserDescription
        let currentOuter = model.PolygonExport.OuterStr
        let currentIslands = model.PolygonExport.IslandsStr

        let newModel = { model with IsSavingToHynteract = true }

        let cmd = 
            Cmd.OfAsync.perform (fun () -> async {
                try
                    let mutable currentCache = model.LayoutCache
                    let configMap = 
                        Hexel.sqnArray 
                        |> Array.indexed
                        |> Array.map (fun (i, sqnCase) -> 
                            let key = sprintf "%A" sqnCase
                            let data = 
                                try 
                                    // Try to pull from any level in the cache
                                    let cached = 
                                        model.Tree.Levels.Keys 
                                        |> Seq.tryPick (fun lvl -> Cache.get lvl i currentCache)
                                    
                                    match cached with
                                    | Some c -> 
                                        FileManager.generateHynteractPayloadFromCxls c.cxCxl1
                                    | None -> 
                                        // Compute and UPDATE LOCAL CACHE for all levels
                                        let fullData = Cache.computeFullLayout currentSrc sqnCase model.PolygonExport 0
                                        for lvl in model.Tree.Levels.Keys do
                                            let config = Cache.fromFullLayout fullData sqnCase lvl
                                            currentCache <- Cache.update lvl i config currentCache
                                        
                                        FileManager.generateHynteractPayloadFromCxls (let cxls, _, _, _ = fullData in cxls)
                                with ex -> 
                                    printfn "Warning: Orientation %s failed Dataset Generation: %s" key ex.Message
                                    "" 
                            key, data)
                        |> Map.ofArray

                    let payload = {| 
                        Definition = currentSrc 
                        Description = currentDesc 
                        Configuration = configMap 
                        Boundary = currentOuter
                        Islands = currentIslands
                        Metadata = {|
                            Scale = model.TeachMetadata.Scale
                            Typology = model.TeachMetadata.Typology
                            Flow = model.TeachMetadata.Flow
                            Ambience = model.TeachMetadata.Ambience
                            Stage = model.TeachMetadata.Stage
                        |}
                    |}

                    let! success = 
                        js.InvokeAsync<bool>("recordToHynteract", "https://hynteract.vercel.app/api/record", payload).AsTask() 
                        |> Async.AwaitTask
                    
                    return success, currentCache
                
                with ex -> 
                    printfn "Critical Recording Failure: %s" ex.Message
                    return false, model.LayoutCache
            }) () (fun (res, cache) -> RecordResult (res, cache))
        Some (newModel, cmd)
    | _ -> None
