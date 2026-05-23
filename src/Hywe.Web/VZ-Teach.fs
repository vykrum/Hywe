module Hywe.Teach

open System
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open ModelTypes
open Hywe.Site
open Hywe.Site.State
open Hywe.Site.View
open Hywe.Core
open Hywe.Core.Lexel
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Node
open FileManager
open Cache
open Page

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

    let describeLevel (level: int) (root: TreeNode) =
        let header = 
            if level = 0 then "\nBase Level: "
            else 
                match levelToAnchor |> Map.tryFind level with
                | Some anchorId -> sprintf "\nLevel %d (Elevated): " level
                | None -> sprintf "\nLevel %d: " level
        
        let body = getTreeSummary root
        if String.IsNullOrWhiteSpace body then header + sprintf "Starting from %s." root.Name
        else header + body

    let authorPart = 
        if String.IsNullOrWhiteSpace meta.Author then ""
        else sprintf " by %s" (meta.Author.Trim())

    let projectPart = 
        if String.IsNullOrWhiteSpace meta.ProjectTitle then ""
        else sprintf " for the '%s' project" (meta.ProjectTitle.Trim())

    let intro = sprintf "This is a %s stage %s %s project%s%s with a %s flow and %s ambience. " 
                    (meta.Stage.ToLower()) (meta.Scale.ToLower()) (meta.Typology.ToLower()) authorPart projectPart (meta.Flow.ToLower()) (meta.Ambience.ToLower())

    let levelsContent = 
        tree.Levels 
        |> Map.toList 
        |> List.sortBy fst
        |> List.map (fun (lvl, root) -> describeLevel lvl root)
        |> String.concat "\n"

    (intro + "\n" + levelsContent).Trim()

let generateHynteractPayload (model: Model) : string[] =
    let b36 (v: int) = Hexel.toBase36 (int64 v)
    
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
                            coords |> Array.map (fun h -> let x,y,_ = hxlCrd h in b36 x + "," + b36 y) |> String.concat ","
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
                            coords |> Array.map (fun h -> let x,y,_ = hxlCrd h in b36 x + "," + b36 y) |> String.concat ","
                        | None -> ""
                    )
                variations.Add(roomCoords |> String.concat ";")
            | None -> variations.Add("")
            
        results.Add(sprintf "%s | %s)" header (String.concat " | " variations))
        
    results.ToArray()

let update (js: IJSRuntime) (msg: Message) (model: Model) : (Model * Cmd<Message>) option =
    match msg with
    | SetDescription d -> Some ({ model with UserDescription = d }, Cmd.none)
    | SuggestDescription -> Some ({ model with UserDescription = generateSuggestion model }, Cmd.none)
    | RecordResult (success, cache) ->
        let newModel = 
            { model with 
                IsSavingToHynteract = false
                ShowSuccessMessage = success
                UserDescription = if success then "" else model.UserDescription 
                LayoutCache = cache
            }
        let cmd = if success then Cmd.OfAsync.perform (fun () -> Async.Sleep 3000) () (fun _ -> StartHyweave) else Cmd.none
        Some (newModel, cmd)
    | UpdateMetadata f -> Some ({ model with TeachMetadata = f model.TeachMetadata }, Cmd.none)
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
        let currentDesc = model.UserDescription
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
                                    let cfg = Cache.fromFullLayout fullData Hexel.sqnArray.[i] lvlIdx
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
                                        let sqnStr = sprintf "%A" Hexel.sqnArray.[i]
                                        match Nexel.generateNestLayout nestBlock host nestBlock.Attributes.Thickness rootCfg.cxCxl1 (Some sqnStr) with
                                        | Some (ncxls, _, _) ->
                                            let nestCfg = 
                                                {| sqnName = sqnStr
                                                   shapes = [||] 
                                                   w = 0.0; h = 0.0
                                                   cxCxl1 = ncxls
                                                   cxElv1 = [||]; cxlAvl = [||]; cxOuIl = [||]
                                                   cxAdj1 = ([||], [||]); cxB36 = [||]; cxRto1 = [||]; cxClr1 = [||] |}
                                            currentCache <- Cache.update nestMarker i nestCfg currentCache
                                        | None -> ()
                                    | None -> ()

                    let updatedModel = { model with LayoutCache = currentCache }
                    let payloadArray = generateHynteractPayload updatedModel
                    
                    let payload = {| definition = currentSrc; description = currentDesc; configuration = payloadArray |}
                    let! success = js.InvokeAsync<bool>("recordToHynteract", "https://hynteract.vercel.app/api/record", payload).AsTask() |> Async.AwaitTask
                    return success, currentCache
                with e ->
                    do! js.InvokeVoidAsync("console.error", "Error recording to Hynteract: " + e.Message).AsTask() |> Async.AwaitTask
                    return false, model.LayoutCache
            }) () RecordResult
        Some (newModel, cmd)
    | _ -> None

// --- UI COMPONENTS ---

let private selectField (model: Model) dispatch (label: string) (current: string) (options: string list) (descriptions: Map<string, string>) updater =
    let isPredefined = options |> List.contains current
    let rowTips = descriptions |> Map.toSeq |> Seq.map snd |> Set.ofSeq
    let currentTip = 
        match model.HoveredInfo with
        | Some (tip: string) when rowTips.Contains tip -> Some tip
        | Some (tip: string) when tip.Contains(label.ToLower()) -> Some tip
        | _ -> if isPredefined then descriptions |> Map.tryFind current else Some $"Custom {label.ToLower()} tag applied."
    div {
        attr.``class`` "teach-select-row"
        span { attr.``class`` "teach-field-label"; text label }
        div {
            attr.``class`` "teach-option-group"
            for opt in options do
                let activeClass = if current = opt then "hywe-btn-gray active teach-option" else "hywe-btn-light teach-option"
                button {
                    attr.``class`` ("hywe-btn hywe-btn-sm " + activeClass)
                    on.mouseover (fun _ -> dispatch (SetHoveredInfo (descriptions |> Map.tryFind opt)))
                    on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                    on.click (fun _ -> dispatch (UpdateMetadata (fun m -> updater m opt)))
                    text opt
                }
            let otherActiveClass = if not isPredefined then "hywe-btn-gray active teach-option" else "hywe-btn-light teach-option"
            button {
                attr.``class`` ("hywe-btn hywe-btn-sm " + otherActiveClass)
                on.mouseover (fun _ -> dispatch (SetHoveredInfo (Some $"Enter a custom {label.ToLower()} tag.")))
                on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                on.click (fun _ -> dispatch (UpdateMetadata (fun m -> updater m "")))
                text "Other..."
            }
        }
        match currentTip with | Some tip -> div { attr.``class`` "teach-row-tip"; text tip } | None -> ()
        if not isPredefined then
            input {
                attr.``class`` "teach-custom-input"
                attr.placeholder (sprintf "Enter custom %s..." (label.ToLower()))
                attr.value current
                on.input (fun e -> dispatch (UpdateMetadata (fun m -> updater m (unbox<string> e.Value))))
            }
    }

let view model dispatch =
    let flowDescs = Map [ "Sequential", "A 'deep' flow where spaces lead into one another in a chain."; "Radial", "A 'shallow' flow where most spaces branch directly from a single central hub."; "Hierarchical", "A multi-level tree where primary spaces lead to secondary clusters." ]
    let ambiDescs = Map [ "Open", "Minimizes walls to maximize visual and spatial continuity."; "Modular", "Highly partitioned with distinct, repeatable, or enclosed spaces."; "Minimal", "Stripped back to essential connections and clean flow." ]
    let stageDescs = Map [ "Ideation", "Initial loose clustering and spatial relationship mapping."; "Zoning", "Structured grouping of distinct functional areas."; "Massing", "Defined volumetric proportions and 3D stacking logic." ]
    let scaleDescs = Map [ "Layout", "Single-level spatial arrangement or individual unit logic."; "Building", "Multi-level structure with vertical hierarchical dependencies."; "Masterplan", "Large-scale arrangement or multi-building planning." ]
    let typoDescs = Map [ "Residential", "Homes, apartments, or private living quarters."; "Commercial", "Workspaces, retail, or corporate environments."; "Institutional", "Healthcare, educational, or civic facilities." ]

    div {
        attr.``class`` "teach-panel-container"
        div {
            attr.``class`` "teach-intro-section"
            h2 { attr.``class`` "teach-intro-title"; text "Architectural Data Collection" }
            p { attr.``class`` "teach-intro-text"; text "Help generate a robust architectural dataset by tagging your design intent. The fields below should be filled with respect to your currently defined workflow, training the underlying spatial logic to learn complex hierarchical layouts." }
        }
        div {
            attr.``class`` "teach-objective-section"
            div {
                attr.``class`` "teach-select-row"
                span { attr.``class`` "teach-field-label"; text "Author" }
                input {
                    attr.``class`` "teach-custom-input"
                    attr.placeholder "Optional author name..."
                    attr.value model.TeachMetadata.Author
                    on.input (fun e -> dispatch (UpdateMetadata (fun m -> { m with Author = unbox<string> e.Value })))
                }
            }
            div {
                attr.``class`` "teach-select-row"
                span { attr.``class`` "teach-field-label"; text "Project" }
                input {
                    attr.``class`` "teach-custom-input"
                    attr.placeholder "Optional project title..."
                    attr.value model.TeachMetadata.ProjectTitle
                    on.input (fun e -> dispatch (UpdateMetadata (fun m -> { m with ProjectTitle = unbox<string> e.Value })))
                }
            }
            selectField model dispatch "Scale" model.TeachMetadata.Scale [ "Layout"; "Building"; "Masterplan" ] scaleDescs (fun m v -> { m with Scale = v })
            selectField model dispatch "Typology" model.TeachMetadata.Typology [ "Residential"; "Commercial"; "Institutional" ] typoDescs (fun m v -> { m with Typology = v })
            selectField model dispatch "Flow" model.TeachMetadata.Flow [ "Sequential"; "Radial"; "Hierarchical" ] flowDescs (fun m v -> { m with Flow = v })
            selectField model dispatch "Ambience" model.TeachMetadata.Ambience [ "Open"; "Modular"; "Minimal" ] ambiDescs (fun m v -> { m with Ambience = v })
            selectField model dispatch "Stage" model.TeachMetadata.Stage [ "Ideation"; "Zoning"; "Massing" ] stageDescs (fun m v -> { m with Stage = v })
        }
        div {
            attr.style "width: 100%; margin-top: 0.8rem; display: flex; flex-direction: column; gap: 0.3rem;"
            div {
                attr.style "display: grid; grid-template-columns: 40px 1fr 40px; align-items: center; gap: 0.5rem; width: 100%;"
                button {
                    attr.``class`` (match model.IsRecording with | true -> "mic-button recording" | false -> "mic-button")
                    attr.style "justify-self: start;"
                    attr.title "Start Voice Capture"
                    on.click (fun _ -> dispatch StartVoiceCapture)
                    rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z"></path><path d="M19 10v2a7 7 0 0 1-14 0v-2"></path><line x1="12" y1="19" x2="12" y2="23"></line><line x1="8" y1="23" x2="16" y2="23"></line></svg>"""
                }
                button {
                    attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-dark"
                    attr.style "justify-self: center;"
                    attr.title "Generate Summary"
                    on.click (fun _ -> dispatch SuggestDescription)
                    text "Generate Summary"
                }
                button {
                    attr.``class`` "mic-button"
                    attr.style "justify-self: end; color: #e74c3c;"
                    attr.title "Clear Description"
                    on.click (fun _ -> dispatch (SetDescription ""))
                    rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"></polyline><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path><line x1="10" y1="11" x2="10" y2="17"></line><line x1="14" y1="11" x2="14" y2="17"></line></svg>"""
                }
            }
            textarea {
                attr.id "hynteract-desc-input"
                attr.``class`` "hywe-input"
                attr.placeholder "Enter spatial narrative or unique design nuances..."
                attr.value model.UserDescription
                on.input (fun e -> dispatch (SetDescription (unbox<string> e.Value)))
            }
        }
        div {
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; gap: 0.5rem; margin-top: 0.8rem;"
            let hasSummary = not (String.IsNullOrWhiteSpace model.UserDescription)
            let isBusy = model.IsSavingToHynteract
            p { 
                attr.style "font-size: 0.85em; color: #7f8c8d; font-style: italic; text-align: center; margin: 0; max-width: 80%;"
                if hasSummary then text "Review summary and add any relevant input/details" else text "A spatial summary is required to enable commitment"
            }
            button {
                attr.``class`` ("hywe-btn hywe-btn-dark record-submit-btn" + (if isBusy || not hasSummary then " disabled" else " active"))
                attr.style (if not hasSummary then "opacity: 0.5; cursor: not-allowed;" else "")
                attr.title (if not hasSummary then "Please generate or enter a summary first" else "Commit this intent to the dataset")
                attr.disabled (isBusy || not hasSummary)
                on.click (fun _ -> dispatch RecordToHynteract)
                match isBusy with | true -> text "Committing..." | false -> text "Commit to Dataset"
            }
        }
    }
