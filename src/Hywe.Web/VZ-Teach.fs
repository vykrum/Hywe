module Hywe.Teach

open System
open Microsoft.JSInterop
open Elmish
open Bolero.Html
open ModelTypes
open Hywe.Core
open Hywe.Core.Lexel
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Node
open Page
open Hywe.Site
open Graphics
open Layout

// --- UTILITIES & TRANSCRIPTION ---

let startTranscription (js: IJSRuntime) (textAreaId: string) =
    async {
        do! js.InvokeVoidAsync("eval", sprintf """
            (function() {
                return new Promise((resolve) => {
                    const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
                    if (!SpeechRecognition) { 
                        alert("Speech recognition is not supported."); 
                        resolve(); 
                        return; 
                    }
                    const recognition = new SpeechRecognition();
                    const textArea = document.getElementById('%s');
                    recognition.onresult = (event) => {
                        let finalTranscript = '';
                        for (let i = event.resultIndex; i < event.results.length; ++i) {
                            if (event.results[i].isFinal) {
                                finalTranscript += event.results[i][0].transcript;
                            }
                        }
                        if (finalTranscript) {
                            const currentVal = textArea.value.trim();
                            textArea.value = currentVal ? `${currentVal} ${finalTranscript}` : finalTranscript;
                            textArea.dispatchEvent(new Event('input', { bubbles: true }));
                        }
                    };
                    recognition.onend = () => resolve();
                    recognition.onerror = () => resolve();
                    recognition.start();
                });
            })()
        """ textAreaId).AsTask() |> Async.AwaitTask
    }

// --- GENERATOR LOGIC (from UP-Teach.fs) ---

let generateSuggestion (model: Model) =
    let tree = model.Tree
    let meta = model.TeachMetadata
    let levelToAnchor = tree.LevelAnchors 

    let rec getTreeSummary (node: TreeNode) =
        if node.Children.IsEmpty then ""
        else
            let childNames = node.Children |> List.map (fun c -> c.Name) |> String.concat ", "
            let verb = if node.Children.Length > 1 then "branches into" else "leads to"
            let current = sprintf "The %s %s %s." node.Name verb childNames
            let children = node.Children |> List.map getTreeSummary |> String.concat " "
            current + " " + children

    let rec findInTree (node: TreeNode) (targetId: System.Guid) =
        if node.Id = targetId then Some node
        else node.Children |> List.tryPick (fun c -> findInTree c targetId)

    let findAnchor (targetId: System.Guid) =
        let lvlMatch = 
            tree.Levels |> Map.tryPick (fun lvl root -> 
                match findInTree root targetId with
                | Some n -> Some (n.Name, sprintf "Level %d" lvl)
                | None -> None)
        match lvlMatch with
        | Some res -> Some res
        | None ->
            tree.Nests |> Map.tryPick (fun nid root ->
                match findInTree root targetId with
                | Some n -> Some (n.Name, sprintf "Nest %d" nid)
                | None -> None)

    let describeLevel (level: int) (root: TreeNode) =
        let header = 
            if level = 0 then "\nBase Level: "
            else 
                match levelToAnchor |> Map.tryFind level with
                | Some anchorId -> 
                    match findAnchor anchorId with
                    | Some (nName, hostName) -> sprintf "\nLevel %d (Ascending from %s on %s): " level nName hostName
                    | None -> sprintf "\nLevel %d (Elevated): " level
                | None -> sprintf "\nLevel %d: " level
        
        let body = getTreeSummary root
        if String.IsNullOrWhiteSpace body then header + sprintf "Starting from %s." root.Name
        else header + body

    let authorPart = 
        if String.IsNullOrWhiteSpace meta.Author then ""
        else sprintf " by %s" (meta.Author.Trim())

    let explorationPart = 
        if String.IsNullOrWhiteSpace meta.ExplorationDescription then ""
        else sprintf " exploring '%s'" (meta.ExplorationDescription.Trim())

    let boundaryPart =
        let isBoundaryActive =
            let poly = 
                match model.PolygonEditor with
                | Stable p | FreshlyImported p -> p
            poly.UseBoundary

        if not isBoundaryActive then
            "The layout is unbound."
        else
            let firstConfig = 
                model.LayoutCache 
                |> Map.toSeq
                |> Seq.tryPick (fun (_, configs) -> configs |> Array.tryPick id)
                
            match firstConfig with
            | Some cfg ->
                let activeBoundaries = cfg.cxOuIl |> Array.filter (fun poly -> poly.Length > 0)
                if activeBoundaries.Length = 0 then
                    "The layout is unbound."
                else
                    let islandText = if activeBoundaries.Length > 1 then "with islands" else "without islands"
                    let scaleText = if cfg.mapScale <> 1.0 then sprintf " with a map scale of 1:%d" (int cfg.mapScale) else ""
                    sprintf "The layout is bound at %dx%d%s, %s." model.PolygonExport.Width model.PolygonExport.Height scaleText islandText
            | None ->
                if String.IsNullOrWhiteSpace model.PolygonExport.OuterStr then
                    "The layout is unbound."
                else
                    let islandText = if String.IsNullOrWhiteSpace model.PolygonExport.IslandsStr then "without islands" else "with islands"
                    let scaleText = if model.PolygonExport.MapScale <> 1.0 then sprintf " with a map scale of 1:%d" (int model.PolygonExport.MapScale) else ""
                    sprintf "The layout is bound at %dx%d%s, %s." model.PolygonExport.Width model.PolygonExport.Height scaleText islandText

    let descTerms = [
        if not (String.IsNullOrWhiteSpace meta.Stage) then meta.Stage.ToLower() + " stage"
        if not (String.IsNullOrWhiteSpace meta.Scale) then meta.Scale.ToLower()
        if not (String.IsNullOrWhiteSpace meta.Typology) && meta.Typology <> "Other" then meta.Typology.ToLower()
    ]
    let descPart = if descTerms.IsEmpty then "project" else (String.concat " " descTerms) + " project"

    let flowAmbienceTerms = [
        if not (String.IsNullOrWhiteSpace meta.Flow) then meta.Flow.ToLower() + " flow"
        if not (String.IsNullOrWhiteSpace meta.Ambience) then meta.Ambience.ToLower() + " ambience"
    ]
    let flowAmbiencePart = 
        if flowAmbienceTerms.IsEmpty then "" 
        else " with a " + (String.concat " and " flowAmbienceTerms)

    let intro = sprintf "This is a %s%s%s%s. %s" descPart authorPart explorationPart flowAmbiencePart boundaryPart

    let levelsContent = 
        tree.Levels 
        |> Map.toList 
        |> List.sortBy fst
        |> List.map (fun (lvl, root) -> describeLevel lvl root)
        |> String.concat "\n"

    let describeNest (nestId: int) (root: TreeNode) =
        let anchorInfo = 
            match tree.NestAnchors |> Map.tryFind nestId with
            | Some anchorId ->
                match findAnchor anchorId with
                | Some (nName, hostName) -> sprintf " (Anchored in %s on %s)" nName hostName
                | None -> ""
            | None -> ""
            
        let header = sprintf "\nNest %d%s: " nestId anchorInfo
        let body = getTreeSummary root
        if String.IsNullOrWhiteSpace body then header + sprintf "Starting from %s." root.Name
        else header + body

    let nestsContent = 
        if tree.Nests.IsEmpty then ""
        else
            tree.Nests
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (nid, root) -> describeNest nid root)
            |> String.concat "\n"

    (intro + "\n" + levelsContent + "\n" + nestsContent).Trim()

let generateHynteractPayload (model: Model) : string[] =
    let b34 (v: int) = Hexel.toBase34 (int64 v)
    
    let srcForBatch = ensureCategory model.SrcOfTrth 11
    let parsedBlocks = Lexel.processFullString srcForBatch
    
    let levels = parsedBlocks |> List.choose (function Lexel.Level l -> Some l | _ -> None)
    let nests = parsedBlocks |> List.choose (function Lexel.Nest n -> Some n | _ -> None)
    
    let results = ResizeArray<string>()
    
    // Process Base Levels
    for (lvlIdx, lvlBlock) in levels |> List.indexed do
        let markerLen = lvlBlock.Marker.Length + 1
        let nodes = lvlBlock.Tree |> List.collect id
        let roomIdsStr = nodes |> List.map (fun n -> n.Id.Substring(markerLen)) |> String.concat ";"
        let header = sprintf "%s(%s" lvlBlock.Marker roomIdsStr
        
        let variations = ResizeArray<string>()
        for i = 0 to 23 do
            match Cache.get lvlBlock.Marker i model.LayoutCache with
            | Some config ->
                // Zaxel assigns z = sequential lvlIdx, not the L= attribute value
                let cxls = config.cxCxl1 |> Array.filter (fun c -> let (_,_,z) = hxlCrd c.Base in z = lvlIdx)
                let roomCoords = 
                    nodes |> List.map (fun n ->
                        match cxls |> Array.tryFind (fun c -> prpVlu c.Rfid = n.Id) with
                        | Some cxl ->
                            let coords = Array.append [|cxl.Base|] cxl.Hxls
                            coords |> Array.map (fun h -> let x,y,_ = hxlCrd h in b34 x + "," + b34 y) |> String.concat ","
                        | None -> ""
                    )
                variations.Add(roomCoords |> String.concat ";")
            | None -> variations.Add("")
            
        results.Add(sprintf "%s | %s)" header (String.concat " | " variations))
        
    // Process Nests
    for nestBlock in nests do
        let markerLen = nestBlock.Marker.Length + 1
        let nodes = nestBlock.Tree |> List.collect id
        let roomIdsStr = nodes |> List.map (fun n -> n.Id.Substring(markerLen)) |> String.concat ";"
        let header = sprintf "%s(%s" nestBlock.Marker roomIdsStr
        
        let variations = ResizeArray<string>()
        for i = 0 to 23 do
            match Cache.get nestBlock.Marker i model.LayoutCache with
            | Some nConfig ->
                let cxls = nConfig.cxCxl1
                let roomCoords = 
                    nodes |> List.map (fun n ->
                        match cxls |> Array.tryFind (fun c -> prpVlu c.Rfid = n.Id) with
                        | Some cxl ->
                            let coords = Array.append [|cxl.Base|] cxl.Hxls
                            coords |> Array.map (fun h -> let x,y,_ = hxlCrd h in b34 x + "," + b34 y) |> String.concat ","
                        | None -> ""
                    )
                variations.Add(roomCoords |> String.concat ";")
            | None -> variations.Add("")
            
        results.Add(sprintf "%s | %s)" header (String.concat " | " variations))
        
    results.ToArray()

let generateThumbnailSvg (cfg: BatchConfgrtns) =
    let scl = 1.0
    let padd = 4.0
    let wdt = max 20.0 (cfg.w * scl + padd * 2.0)
    let hgt = max 20.0 (cfg.h * scl + padd * 2.0)
    
    let sb = System.Text.StringBuilder()
    sb.Append(sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" width=\"100%%\" height=\"100%%\">" (int wdt) (int hgt)) |> ignore
    
    let sqn = match Page.Elements.parseSqn cfg.sqnName with | Some s -> s | None -> Hexel.VRCCNE
    for poly in cfg.cxOuIl do
        let xy = 
            poly 
            |> Array.map (fun (x,y) -> Graphics.svgToCartesian sqn (float x, float y))
            |> Array.map (fun (x,y) -> sprintf "%.1f,%.1f" (x * scl + padd) (y * scl + padd))
            |> String.concat " "
        sb.Append(sprintf "<polygon points=\"%s\" fill=\"none\" stroke=\"#2c3e50\" stroke-width=\"1.5\" opacity=\"0.3\" />" xy) |> ignore

    for s in cfg.shapes do
        if not (Array.isEmpty s.points) then
            let xy = 
                s.points 
                |> Array.chunkBySize 2 
                |> Array.map (fun p -> sprintf "%.1f,%.1f" (p.[0] * scl + padd) (p.[1] * scl + padd)) 
                |> String.concat " "
            sb.Append(sprintf "<polygon points=\"%s\" fill=\"%s\" opacity=\"0.85\" />" xy s.color) |> ignore
    
    sb.Append("</svg>") |> ignore
    sb.ToString()

[<CLIMutable>]
type HynteractResponse = {
    ok: bool
    error: string
    code: string
}

let update (js: IJSRuntime) (msg: Message) (model: Model) : (Model * Cmd<Message>) option =
    match msg with
    | SetDescription d -> Some ({ model with UserDescription = d; TeachErrorMessage = None }, Cmd.none)
    | SuggestDescription -> Some ({ model with UserDescription = generateSuggestion model; TeachErrorMessage = None }, Cmd.none)
    | RecordResult (success, errorOpt, cache) ->
        let newModel = 
            { model with 
                IsSavingToHynteract = false
                ShowSuccessMessage = success
                TeachErrorMessage = if success then None else errorOpt
                UserDescription = if success then "" else model.UserDescription 
                LayoutCache = cache
            }
        let cmd = if success then Cmd.OfAsync.perform (fun () -> Async.Sleep 3000) () (fun _ -> StartHyweave) else Cmd.none
        Some (newModel, cmd)
    | UpdateMetadata f -> Some ({ model with TeachMetadata = f model.TeachMetadata; TeachErrorMessage = None }, Cmd.none)
    | SetHoveredInfo info -> Some ({ model with HoveredInfo = info }, Cmd.none)
    | StartVoiceCapture -> 
        let newModel = { model with IsRecording = true }
        let cmd = 
            Cmd.OfAsync.perform (fun () -> 
                async { 
                    do! startTranscription js "hynteract-desc-input"
                    return () 
                }) () (fun _ -> OnVoiceResult)
        Some (newModel, cmd)
    | OnVoiceResult -> Some ({ model with IsRecording = false }, Cmd.none)
    | RecordToHynteract ->
        let currentSrc = model.SrcOfTrth
        let newModel = { model with IsSavingToHynteract = true }
        let cmd = 
            Cmd.OfAsync.perform (fun () -> async {
                try
                    let mutable currentCache = model.LayoutCache
                    let parsedBlocks = Lexel.processFullString currentSrc
                    
                    let levels = parsedBlocks |> List.choose (function Lexel.Level l -> Some l | _ -> None)
                    let nests = parsedBlocks |> List.choose (function Lexel.Nest n -> Some n | _ -> None)

                    for i = 0 to 23 do
                        // Check if ALL levels are already cached for this variation
                        let missingLevels = levels |> List.filter (fun lvl -> Cache.get lvl.Marker i currentCache |> Option.isNone)
                        
                        if not missingLevels.IsEmpty then
                            // Compute once - fullData contains geometry for ALL levels
                            let rootLevel = 
                                if model.Tree.Levels |> Map.containsKey 0 then 0 
                                elif model.Tree.Levels.IsEmpty then 0 
                                else model.Tree.Levels |> Map.toList |> List.map fst |> List.min
                            let srcForBatch = ensureCategory currentSrc i
                            let fullData = Cache.computeFullLayout srcForBatch Hexel.sqnArray.[i] model.PolygonExport rootLevel
                            // Populate every level marker from the single full layout
                            // Use lvlIdx (sequential index) as Zaxel assigns z by sequential block order, not L= attribute
                            for (lvlIdx, lvl) in levels |> List.indexed do
                                if Cache.get lvl.Marker i currentCache |> Option.isNone then
                                    let cfg = Cache.fromFullLayout fullData Hexel.sqnArray.[i] lvlIdx model.PolygonExport
                                    currentCache <- Cache.update lvl.Marker i cfg currentCache
                                
                        for nestBlock in nests do
                            let nestMarker = nestBlock.Marker
                            match Cache.get nestMarker i currentCache with
                            | Some _ -> ()
                            | None ->
                                // Find the level whose L= attribute matches the nest's L= attribute
                                // Cache key is the sequential marker (L0, L1...) not the L= value
                                let hostLevelOpt = levels |> List.tryFind (fun lvl -> lvl.Attributes.Level = nestBlock.Attributes.Level)
                                let hostLevelMarker = hostLevelOpt |> Option.map (fun lvl -> lvl.Marker) |> Option.defaultValue ("L" + string nestBlock.Attributes.Level)
                                match Cache.get hostLevelMarker i currentCache with
                                | None -> () // host level not available, skip
                                | Some rootCfg ->
                                    let hostCxlOpt = rootCfg.cxCxl1 |> Array.tryFind (fun c -> prpVlu c.Rfid = nestBlock.Attributes.Base || (prpVlu c.Rfid).EndsWith("." + nestBlock.Attributes.Base))
                                    match hostCxlOpt with
                                    | Some host ->
                                        let sqnStr = Hexel.sqnToString Hexel.sqnArray.[i]
                                        match Nexel.generateNestLayout nestBlock host nestBlock.Attributes.Thickness rootCfg.cxCxl1 (Some sqnStr) with
                                        | Some (ncxls, _, _) ->
                                            let nestCfg = 
                                                {| sqnName = sqnStr
                                                   shapes = [||] 
                                                   w = 0.0; h = 0.0; mapScale = 1.0
                                                   cxCxl1 = ncxls
                                                   cxElv1 = [||]; cxlAvl = [||]; cxOuIl = [||]
                                                   wtmkShapes = None
                                                   cxAdj1 = ([||], [||]); cxB36 = [||]; cxRto1 = [||]; cxClr1 = [||]; cxSol1 = None |}
                                            currentCache <- Cache.update nestMarker i nestCfg currentCache
                                        | None -> ()
                                    | None -> ()

                    let updatedModel = { model with LayoutCache = currentCache }
                    let payloadArray = generateHynteractPayload updatedModel
                    
                    let thumbSvg =
                        match Cache.get "L0" 0 currentCache with
                        | Some cfg -> generateThumbnailSvg cfg
                        | None -> ""

                    let levelsCount = levels.Length
                    let spacesCount = 
                        levels 
                        |> List.sumBy (fun l -> l.Tree |> List.collect id |> List.length)

                    let rawDesc = model.TeachMetadata.ExplorationDescription.Trim()
                    let repeatIter =
                        match model.GalleryEntries with
                        | Some entries when not (String.IsNullOrWhiteSpace rawDesc) && not (String.IsNullOrWhiteSpace model.TeachMetadata.Author) ->
                            let cleanDesc = rawDesc.ToLowerInvariant()
                            let cleanAuthor = model.TeachMetadata.Author.Trim().ToLowerInvariant()
                            let count =
                                entries
                                |> List.filter (fun e ->
                                    e.Author.Trim().ToLowerInvariant() = cleanAuthor &&
                                    let eDesc = e.ExplorationDescription.Trim().ToLowerInvariant()
                                    eDesc = cleanDesc || eDesc.StartsWith(cleanDesc + " #"))
                                |> List.length
                            if count > 0 then Some (count + 1) else None
                        | _ -> None

                    let finalDesc =
                        match repeatIter with
                        | Some iter when not (rawDesc.EndsWith(sprintf "#%d" iter)) -> sprintf "%s #%d" rawDesc iter
                        | _ -> rawDesc

                    let userDesc = if String.IsNullOrWhiteSpace model.UserDescription then "" else model.UserDescription.Trim()
                    let genSummary = generateSuggestion updatedModel
                    let combinedDesc =
                        if String.IsNullOrWhiteSpace userDesc then
                            sprintf "Described:\n\nGenerated:\n%s" genSummary
                        else
                            sprintf "Described:\n%s\n\nGenerated:\n%s" userDesc genSummary

                    let payload = {| 
                        definition = currentSrc
                        description = combinedDesc
                        configuration = payloadArray
                        explorationDescription = finalDesc
                        author = model.TeachMetadata.Author
                        svgThumbnail = thumbSvg
                        typology = model.TeachMetadata.Typology
                        scale = model.TeachMetadata.Scale
                        stage = model.TeachMetadata.Stage
                        flow = model.TeachMetadata.Flow
                        ambience = model.TeachMetadata.Ambience
                        levelsCount = levelsCount
                        spacesCount = spacesCount
                        createdAt = DateTime.UtcNow.ToString("o")
                    |}
                    let! res = js.InvokeAsync<HynteractResponse>("recordToHynteract", "https://hynteract.vercel.app/api/record", payload).AsTask() |> Async.AwaitTask
                    let errorOpt = if res.ok then None else Some (if String.IsNullOrWhiteSpace res.error then "Submission was rejected by the server." else res.error)
                    return res.ok, errorOpt, currentCache
                with e ->
                    do! js.InvokeVoidAsync("console.error", "Error recording to Hynteract: " + e.Message).AsTask() |> Async.AwaitTask
                    return false, Some ("Submission failed: " + e.Message), model.LayoutCache
            }) () RecordResult
        Some (newModel, cmd)
    | _ -> None

// --- UI HELPERS & COMPONENTS ---

let private countWords (s: string) =
    if String.IsNullOrWhiteSpace s then 0
    else s.Split([| ' '; '\t'; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries).Length

let private selectField (model: Model) dispatch (label: string) (current: string) (options: string list) (descriptions: Map<string, string>) updater =
    let isPredefined = options |> List.contains current
    let isCustom = not (String.IsNullOrWhiteSpace current) && not isPredefined
    let rowTips = descriptions |> Map.toSeq |> Seq.map snd |> Set.ofSeq
    let currentTip = 
        match model.HoveredInfo with
        | Some (tip: string) when rowTips.Contains tip -> Some tip
        | Some (tip: string) when tip.Contains(label.ToLower()) -> Some tip
        | _ -> 
            if isPredefined then descriptions |> Map.tryFind current 
            elif isCustom && current <> "Other" then Some $"Custom {label.ToLower()} tag applied."
            else None
    div {
        attr.``class`` "teach-select-row"
        span { attr.``class`` "hywe-label"; text label }
        div {
            attr.``class`` "teach-option-group"
            for opt in options do
                let activeClass = if current = opt then "hywe-btn-gray active teach-option" else "hywe-btn-light teach-option"
                button {
                    attr.``class`` ("hywe-btn hywe-btn-sm " + activeClass)
                    on.mouseover (fun _ -> dispatch (SetHoveredInfo (descriptions |> Map.tryFind opt)))
                    on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                    on.click (fun _ -> 
                        let nextVal = if current = opt then "" else opt
                        dispatch (UpdateMetadata (fun m -> updater m nextVal)))
                    text opt
                }
            let otherActiveClass = if isCustom then "hywe-btn-gray active teach-option" else "hywe-btn-light teach-option"
            button {
                attr.``class`` ("hywe-btn hywe-btn-sm " + otherActiveClass)
                on.mouseover (fun _ -> dispatch (SetHoveredInfo (Some $"Enter a custom {label.ToLower()} tag.")))
                on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                on.click (fun _ -> 
                    let nextVal = if isCustom then "" else "Other"
                    dispatch (UpdateMetadata (fun m -> updater m nextVal)))
                text "Other..."
            }
        }
        match currentTip with | Some tip -> div { attr.``class`` "teach-row-tip"; text tip } | None -> ()
        if isCustom then
            div {
                attr.style "width: 100%; display: flex; flex-direction: column; gap: 2px;"
                input {
                    attr.``class`` "hywe-input"
                    attr.placeholder (sprintf "Enter custom %s..." (label.ToLower()))
                    attr.value (if current = "Other" then "" else current)
                    on.input (fun e -> dispatch (UpdateMetadata (fun m -> updater m (unbox<string> e.Value))))
                }
                if not (label.StartsWith("Typology")) then
                    span {
                        attr.style "font-size: 0.72rem; color: #95a5a6; font-style: italic; margin-left: 2px;"
                        text "Leave blank to record as N/A, or enter your custom classification."
                    }
            }
    }

let view model dispatch =
    let flowDescs = Map [ "Sequential", "A 'deep' flow where spaces lead into one another in a chain."; "Radial", "A 'shallow' flow where most spaces branch directly from a single central hub."; "Hierarchical", "A multi-level tree where primary spaces lead to secondary clusters." ]
    let ambiDescs = Map [ "Organic", "Flowing, natural, and unstructured spatial character."; "Structured", "Rigid, highly orderly, and systematic layout."; "Intimate", "Cozy, human-scale, and sheltering environment." ]
    let stageDescs = Map [ "Ideation", "Initial loose clustering and spatial relationship mapping."; "Zoning", "Structured grouping of distinct functional areas."; "Massing", "Defined volumetric proportions and 3D stacking logic." ]
    let scaleDescs = Map [ "Layout", "Single-level spatial arrangement or individual unit logic."; "Building", "Multi-level structure with vertical hierarchical dependencies."; "Masterplan", "Large-scale arrangement or multi-building planning." ]
    let typoDescs = Map [ "Residential", "Homes, apartments, or private living quarters."; "Commercial", "Workspaces, retail, or corporate environments."; "Institutional", "Healthcare, educational, or civic facilities." ]
    let expWords = countWords model.TeachMetadata.ExplorationDescription
    let repeatIter =
        match model.GalleryEntries with
        | Some entries when not (String.IsNullOrWhiteSpace model.TeachMetadata.ExplorationDescription) && not (String.IsNullOrWhiteSpace model.TeachMetadata.Author) ->
            let cleanDesc = model.TeachMetadata.ExplorationDescription.Trim().ToLowerInvariant()
            let cleanAuthor = model.TeachMetadata.Author.Trim().ToLowerInvariant()
            let count =
                entries
                |> List.filter (fun e ->
                    e.Author.Trim().ToLowerInvariant() = cleanAuthor &&
                    let eDesc = e.ExplorationDescription.Trim().ToLowerInvariant()
                    eDesc = cleanDesc || eDesc.StartsWith(cleanDesc + " #"))
                |> List.length
            if count > 0 then Some (count + 1) else None
        | _ -> None

    let currentBlocks = Lexel.processFullString model.SrcOfTrth
    let currentLevels = currentBlocks |> List.choose (function Lexel.Level l -> Some l | _ -> None)
    let currentLevelsCount = max 1 currentLevels.Length
    let currentNodesCount = currentLevels |> List.sumBy (fun l -> l.Tree |> List.collect id |> List.length)

    div {
        attr.``class`` "u-flex-col u-items-center u-gap-xl u-p-lg u-w-full u-max-w-800"
        div {
            attr.``class`` "teach-intro-section"
            h2 { attr.``class`` "teach-intro-title"; text "Architectural Data Collection" }
            p { attr.``class`` "teach-intro-text"; text "Help generate a robust architectural dataset by tagging your design intent. The fields below should be filled with respect to your currently defined workflow, training the underlying spatial logic to learn complex hierarchical layouts." }
            div {
                attr.style "display: flex; gap: 8px; justify-content: center; margin-top: 8px;"
                span {
                    attr.style "background: #f1f3f5; color: #495057; padding: 3px 8px; border-radius: 4px; font-size: 0.8rem; font-weight: 600;"
                    text (sprintf "%d %s" currentLevelsCount (if currentLevelsCount = 1 then "Level" else "Levels"))
                }
                span {
                    attr.style "background: #f1f3f5; color: #495057; padding: 3px 8px; border-radius: 4px; font-size: 0.8rem; font-weight: 600;"
                    text (sprintf "%d %s" currentNodesCount (if currentNodesCount = 1 then "Node" else "Nodes"))
                }
            }
        }
        div {
            attr.``class`` "teach-objective-section"
            div {
                attr.``class`` "teach-select-row"
                span { attr.``class`` "hywe-label"; text "Author*" }
                input {
                    attr.``class`` "hywe-input"
                    attr.placeholder "Author name..."
                    attr.value model.TeachMetadata.Author
                    on.input (fun e -> dispatch (UpdateMetadata (fun m -> { m with Author = unbox<string> e.Value })))
                }
            }
            div {
                attr.``class`` "teach-select-row"
                div {
                    attr.style "display: flex; justify-content: space-between; align-items: center;"
                    span { attr.``class`` "hywe-label"; text "Exploration Description*" }
                    if expWords > 0 && expWords < 3 then
                        span { attr.style "font-size: 0.75rem; color: #e67e22; font-style: italic;"; text (sprintf "%d/3 words" expWords) }
                }
                input {
                    attr.``class`` "hywe-input"
                    attr.placeholder "Describe your design idea (at least 3 words, e.g. Courtyard villa with pool)..."
                    attr.value model.TeachMetadata.ExplorationDescription
                    on.input (fun e -> dispatch (UpdateMetadata (fun m -> { m with ExplorationDescription = unbox<string> e.Value })))
                }
                match repeatIter with
                | Some iter ->
                    div {
                        attr.style "font-size: 0.76rem; color: #2980b9; margin-top: 3px;"
                        text (sprintf "ℹ Existing exploration found for this author — will be recorded as iteration #%d." iter)
                    }
                | None -> ()
            }
            selectField model dispatch "Typology*" model.TeachMetadata.Typology [ "Residential"; "Commercial"; "Institutional" ] typoDescs (fun m v -> { m with Typology = v })
            selectField model dispatch "Scale" model.TeachMetadata.Scale [ "Layout"; "Building"; "Masterplan" ] scaleDescs (fun m v -> { m with Scale = v })
            selectField model dispatch "Flow" model.TeachMetadata.Flow [ "Sequential"; "Radial"; "Hierarchical" ] flowDescs (fun m v -> { m with Flow = v })
            selectField model dispatch "Ambience" model.TeachMetadata.Ambience [ "Organic"; "Structured"; "Intimate" ] ambiDescs (fun m v -> { m with Ambience = v })
            selectField model dispatch "Stage" model.TeachMetadata.Stage [ "Ideation"; "Zoning"; "Massing" ] stageDescs (fun m v -> { m with Stage = v })
        }
        div {
            attr.style "width: 100%; margin-top: 0.8rem; display: flex; flex-direction: column; gap: 0.35rem;"
            div {
                attr.style "display: flex; justify-content: space-between; align-items: center; width: 100%;"
                div {
                    attr.style "display: flex; align-items: baseline; gap: 0.5rem;"
                    label { 
                        attr.style "font-size: 0.85rem; font-weight: 600; color: #2c3e50;"
                        text "Architectural Intent & Spatial Narrative" 
                    }
                }
                div {
                    attr.style "display: flex; align-items: center; gap: 0.4rem;"
                    button {
                        attr.``class`` (match model.IsRecording with | true -> "mic-button recording" | false -> "mic-button")
                        attr.title "Start Voice Capture"
                        on.click (fun _ -> dispatch StartVoiceCapture)
                        rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z"></path><path d="M19 10v2a7 7 0 0 1-14 0v-2"></path><line x1="12" y1="19" x2="12" y2="23"></line><line x1="8" y1="23" x2="16" y2="23"></line></svg>"""
                    }
                    if not (String.IsNullOrWhiteSpace model.UserDescription) then
                        button {
                            attr.``class`` "mic-button"
                            attr.style "color: #e74c3c;"
                            attr.title "Clear Description"
                            on.click (fun _ -> dispatch (SetDescription ""))
                            rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"></polyline><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path><line x1="10" y1="11" x2="10" y2="17"></line><line x1="14" y1="11" x2="14" y2="17"></line></svg>"""
                        }
                }
            }
            textarea {
                attr.id "hynteract-desc-input"
                attr.``class`` "hywe-input"
                attr.style "min-height: 75px; resize: vertical;"
                attr.placeholder "Describe your design nuances, circulation logic, or spatial atmosphere in your own words (or use voice capture to dictate)..."
                attr.value model.UserDescription
                on.input (fun e -> dispatch (SetDescription (unbox<string> e.Value)))
            }
        }
        div {
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; gap: 0.5rem; margin-top: 0.8rem;"
            let containsClientUrl (s: string) =
                if String.IsNullOrWhiteSpace s then false
                else
                    let lower = s.ToLowerInvariant()
                    lower.Contains("http://") || lower.Contains("https://") || lower.Contains("www.") || 
                    lower.Contains(".com") || lower.Contains(".org") || lower.Contains(".net") || lower.Contains(".io")

            let hasUrlWarning = 
                containsClientUrl model.TeachMetadata.Author || 
                containsClientUrl model.TeachMetadata.ExplorationDescription || 
                containsClientUrl model.TeachMetadata.Typology ||
                containsClientUrl model.TeachMetadata.Scale ||
                containsClientUrl model.TeachMetadata.Flow ||
                containsClientUrl model.TeachMetadata.Ambience ||
                containsClientUrl model.TeachMetadata.Stage ||
                containsClientUrl model.UserDescription

            let levelLabel = if currentLevelsCount = 1 then "level" else "levels"
            let totalLayouts = currentLevelsCount * 24
            let cachedLayoutsCount =
                if currentLevels.IsEmpty then 0
                else
                    currentLevels
                    |> List.sumBy (fun lvl ->
                        [ 0 .. 23 ]
                        |> List.filter (fun i -> Cache.get lvl.Marker i model.LayoutCache |> Option.isSome)
                        |> List.length)

            let hasAuthor = not (String.IsNullOrWhiteSpace model.TeachMetadata.Author)
            let hasExploration = expWords >= 3 && model.TeachMetadata.ExplorationDescription.Trim().Length >= 8
            let hasTypology = not (String.IsNullOrWhiteSpace model.TeachMetadata.Typology) && model.TeachMetadata.Typology <> "Other"
            let canCommit = hasAuthor && hasExploration && hasTypology && not hasUrlWarning
            let isBusy = model.IsSavingToHynteract
            p { 
                attr.style "font-size: 0.85em; color: #7f8c8d; font-style: italic; text-align: center; margin: 0; max-width: 80%;"
                if hasUrlWarning then
                    text "⚠ External URLs and links are prohibited in dataset submissions."
                elif canCommit then 
                    if String.IsNullOrWhiteSpace model.UserDescription then
                        text "Ready to commit. Sharing your spatial insights above greatly enriches the dataset."
                    else
                        text "Ready to commit. Your spatial narrative will be paired with the structural breakdown."
                else
                    let missing = [
                        if not hasAuthor then "Author"
                        if expWords = 0 then "Exploration Description"
                        elif expWords < 3 then sprintf "Exploration Description (min 3 words, currently %d)" expWords
                        elif model.TeachMetadata.ExplorationDescription.Trim().Length < 8 then "Exploration Description (min 8 characters)"
                        if not hasTypology then "Typology"
                    ]
                    text (sprintf "%s required to enable commitment" (String.concat ", " missing))
            }
            div {
                attr.style "display: flex; align-items: center; justify-content: center; gap: 6px; margin-top: 2px;"
                if cachedLayoutsCount = totalLayouts then
                    span {
                        attr.style "color: #27ae60; font-size: 0.78rem; font-weight: 600;"
                        text (sprintf "⚡ All %d configurations cached & ready (24 × %d %s)" totalLayouts currentLevelsCount levelLabel)
                    }
                else
                    span {
                        attr.style "color: #7f8c8d; font-size: 0.78rem;"
                        text (sprintf "⚙ %d/%d configurations cached (24 × %d %s), remaining will compute on commit" cachedLayoutsCount totalLayouts currentLevelsCount levelLabel)
                    }
            }
            button {
                attr.``class`` ("hywe-btn hywe-btn-dark hywe-btn-lg u-w-full u-max-w-800 u-mt-md" + (if isBusy || not canCommit then " disabled" else " active"))
                attr.style (if not canCommit then "opacity: 0.5; cursor: not-allowed;" else "")
                attr.title (if not canCommit then "Please fill required fields (Author, Exploration Description, Typology)" else "Commit this intent to the dataset")
                attr.disabled (isBusy || not canCommit)
                on.click (fun _ -> dispatch RecordToHynteract)
                match isBusy with | true -> text "Committing..." | false -> text "Commit to Dataset"
            }
            match model.TeachErrorMessage with
            | Some errMsg ->
                div {
                    attr.style "background: #fdf2f2; border: 1px solid #f8b4b4; border-radius: 6px; padding: 8px 14px; margin-top: 8px; width: 100%; max-width: 600px; text-align: center;"
                    span {
                        attr.style "color: #c81e1e; font-size: 0.85rem; font-weight: 600;"
                        text (sprintf "✕ %s" errMsg)
                    }
                }
            | None ->
                match model.ShowSuccessMessage with
                | true ->
                    div {
                        attr.style "background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 6px; padding: 8px 14px; margin-top: 8px; width: 100%; max-width: 600px; text-align: center;"
                        span {
                            attr.style "color: #166534; font-size: 0.85rem; font-weight: 600;"
                            text "✓ Spatial intent successfully submitted for review."
                        }
                    }
                | false -> ()
        }
    }
