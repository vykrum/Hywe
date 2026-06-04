module Page

open System
open Bolero.Html
open Hywe.Core
open Graphics
open ModelTypes
open Hywe

[<AutoOpen>]
module Elements =
    /// <summary> Helper to parse a sequence name string into a Sqn type. </summary>
    let parseSqn (name: string) =
        Hexel.sqnArray |> Array.tryFind (fun x -> (sprintf "%A" x).Equals(name, StringComparison.OrdinalIgnoreCase))

    // SVG Icons

    // Panel Icons (Original Silhouette Style)
    let pathBoundary = "M3 3h18v18H3V3zm16 16V5H5v14h14zM7 7h10v10H7V7z"
    let pathLayout   = "M12 2l3.5 2v4l-3.5 2-3.5-2V4l3.5-2z M7 11.5l3.5 2v4l-3.5 2-3.5-2v-4l3.5-2z M17 11.5l3.5 2v4l-3.5 2-3.5-2v-4l3.5-2z"
    let pathAnalyze  = "M5 9.2h3V19H5zM10.6 5h2.8v14h-2.8zm5.6 8H19v6h-2.8z"
    let path3D       = "M21 16.5c0 .38-.21.71-.53.88l-7.97 4.65c-.31.18-.69.18-1 0l-7.97-4.65c-.32-.17-.53-.5-.53-.88V7.5c0-.38.21-.71.53-.88l7.97-4.65c.31-.18.69-.18 1 0l7.97 4.65c.32.17.53.5.53.88v9zM12 4.15L6.04 7.5 12 10.85l5.96-3.35L12 4.15zM5 15.91l6 3.5v-6.71L5 9.21v6.7zm14 0v-6.7l-6 3.49v6.71l6-3.5z"
    let pathBatch    = "M4 11h5V5H4v6zm0 7h5v-6H4v6zm6 0h5v-6h-5v6zm6 0h5v-6h-5v6zm-6-7h5V5h-5v6zm6-6v6h5V5h-5z"
    let pathTeach    = "M5 13.18v2.81c0 .73.4 1.41 1.05 1.76l5 2.63c.59.32 1.29.32 1.88 0l5-2.63c.65-.35 1.05-1.03 1.05-1.76v-2.81l-6 3.16c-.63.33-1.37.33-2 0l-6-3.16zM12 3L1 9l11 6 9-4.91V17h2V9L12 3z"
    let pathReport   = "M18 2H6c-1.1 0-2 .9-2 2v16c0 1.1.9 2 2 2h12c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zm0 18H6V4h12v16z"

    // Menu Icons (Refined Outlines)
    let pathSwitchNode = "M21 12l-4.5 7.79h-9L3 12l4.5-7.79h9z"
    let pathSwitchCode = "M16 18l6-6-6-6M8 6l-6 6 6 6"
    let pathSave       = "M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2zM7 3v5h8"
    let pathLoad       = "M12 3v12m0 0l-4-4m4 4l4-4M4 17v2a2 2 0 002 2h12a2 2 0 002-2v-2"
    let pathShare      = "M18 8a3 3 0 100-6 3 3 0 000 6zM6 15a3 3 0 100-6 3 3 0 000 6zM18 22a3 3 0 100-6 3 3 0 000 6zM8.59 13.51l7.82 4.55M16.41 5.94l-7.82 4.55"
    let pathReset      = "M1 4v6h6M3.51 15a9 9 0 102.13-9.36L1 10"
    let pathUndo       = "M10 15l-5-5 5-5 M5 10h12a5 5 0 015 5v2"
    let pathRedo       = "M14 15l5-5-5-5 M19 10H7a5 5 0 00-5 5v2"
    let pathLock       = "M19 11H5a2 2 0 00-2 2v7a2 2 0 002 2h14a2 2 0 002-2v-7a2 2 0 00-2-2z M7 11V7a5 5 0 0110 0v4"
    let pathUnlock     = "M19 11H5a2 2 0 00-2 2v7a2 2 0 002 2h14a2 2 0 002-2v-7a2 2 0 00-2-2z M7 11V7a5 5 0 019.9-1"


    let drawIcon path =
        svg {
            "viewBox" => "0 0 24 24"
            attr.style "width: 1.3rem; height: 1.3rem; fill: currentColor;"
            elt "path" { "d" => path }
        }

    let drawMenuIcon path =
        svg {
            "viewBox" => "0 0 24 24"
            attr.style "width: 14px !important; height: 14px !important; fill: none !important; stroke: #555 !important; stroke-width: 1.5 !important; stroke-linecap: round; stroke-linejoin: round;"
            elt "path" { "d" => path }
        }

    // Default Syntax
    let start = "L0(Q=VRCCNE/L=0/X=1/B=0/E=0)(1/75/Root)"

    let beeyond = "L0(Q=VRCCNE/L=0/X=1/B=0/E=0)(1/105/Dock)(1.1/85/Logistics)(1.2/95/Lab)(1.3/65/Habitation)(1.4/75/Power)"

    let beedroom = "L0(Q=HRCCNW/L=0/W=30/H=30/X=0/E=0/B=0/O=0,0,30,0,30,30,0,30/I=/T=3)(1/12/Foyer)(1.1/12/Living)(1.1.1/18/Dining)" +
                    "(1.1.1.1/15/Kitchen)(1.1.1.1.1/6/Utility)(1.1.1.2/14/Bed-1)(1.1.1.2.1/8/Bath-1)(1.1.1.3/18/Bed-2)" +
                    "(1.1.1.3.1/10/Closet-2)(1.1.1.3.1.1/10/Bath-2)(1.1.1.4/18/Bed-3)(1.1.1.4.1/11/Closet-3)(1.1.1.4.2/10/Bath-3)" +
                    "(1.1.2/12/Staircase)(1.2/12/Study)"

    let stacked = "L0(Q=VRCCNE/L=0/W=30/H=30/X=1/E=0/B=0/O=/I=)(1/75/Lobby)(1.1/88/Retail)(1.2/54/Toilets)(1.3/67/Retail)(1.4/94/Retail)" +
                        "L1(Q=VRCCNE/L=3/E=1/B=1)(1/75/Lobby)(1.1/43/Office)(1.2/123/Office)(1.2.1/34/Toilets)(1.3/52/Office)" + 
                        "L2(Q=VRCCNE/L=6/E=1/B=1/T=5)(1/75/Lobby)(1.1/99/Suite)"


    /// Sqn selection via slider
    let allSqns : string list = [
        "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
        "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
    ]
    let indexToSqn i = allSqns.[i]
    let sqnToIndex sqn = allSqns |> List.findIndex ((=) sqn)

    /// Forces all levels in the source string to match the target sequence index.
    let ensureCategory (src: string) (targetIdx: int) =
        let targetSqnStr = allSqns.[targetIdx]
        let sqns = Lexel.extractSequences src
        (src, sqns) ||> Map.fold (fun s lvl _ ->
            Lexel.injectSqn s lvl targetSqnStr
        )

    // Label string (24 characters)
    let labelPhrase = "alternATE◦CONFIGURATions"

    // Sequence Slider Component
    let sequenceSlider (selected: string) (minIdx: int) (maxIdx: int) (dispatch: int -> unit) =
        let currentIndex = sqnToIndex selected
        
        div {
            attr.``class`` "slider-wrapper"

            // Labels
            div {
                yield attr.``class`` "slider-labels"
                for i in 0 .. 23 -> 
                    let inRange = i >= minIdx && i <= maxIdx
                    span {
                        attr.``class`` (
                            match i = currentIndex, inRange with 
                            | true, true  -> "slider-label active"
                            | true, false -> "slider-label active inactive"
                            | false, true -> "slider-label"
                            | false, false -> "slider-label inactive"
                        )
                        text (labelPhrase.[i].ToString())
                    }
            }

            // Slider track
            div {
                attr.``class`` "slider-track-container"
                
                // Precise math to align slider thumb (14px wide) with label centers (16px labels, space-between)
                let leftCalc = $"calc(1px + ({minIdx}.0 / 23.0) * (100%% - 16px))"
                let widthCalc = $"calc(14px + (({maxIdx}.0 - {minIdx}.0) / 23.0) * (100%% - 16px))"
                
                input {
                    attr.``type`` "range"
                    attr.``class`` "custom-slider"
                    attr.min (string minIdx)
                    attr.max (string maxIdx)
                    attr.step "1"
                    attr.value (string currentIndex)
                    attr.style $"left: {leftCalc}; width: {widthCalc}; position: absolute;"
                    on.input (fun ev ->
                        match ev.Value with
                        | :? string as s ->
                            match System.Int32.TryParse(s) with
                            | true, i -> 
                                let clamped = max minIdx (min maxIdx i)
                                dispatch clamped
                            | _ -> ()
                        | _ -> ()
                    )
                }

                // Inactive area masks to disallow direct selection/clicks on deactivated fields
                let totalSteps = 23.0
                if minIdx > 0 then
                    let leftWidth = (float minIdx / totalSteps) * 100.0
                    div {
                        attr.style $"position: absolute; left: 0; top: 0; bottom: 0; width: {leftWidth}%%; z-index: 5; cursor: default;"
                        "onpointerdown:stopPropagation" => true
                        "onclick:stopPropagation" => true
                    }
                if maxIdx < 23 then
                    let rightOffset = (float (maxIdx + 1) / totalSteps) * 100.0
                    let rightWidth = 100.0 - rightOffset
                    div {
                        attr.style $"position: absolute; left: {rightOffset}%%; top: 0; bottom: 0; width: {rightWidth}%%; z-index: 5; cursor: default;"
                        "onpointerdown:stopPropagation" => true
                        "onclick:stopPropagation" => true
                    }
            }
        }


    /// The top navigation bar with tab switching
    let viewTabs (active: EditorTab) (dispatch: Message -> unit) =
        let isBoundary = match active with Boundary -> true | _ -> false
        let isEditor   = match active with Editor _ -> true | _ -> false
        
        div {
            attr.``class`` "hywe-tabs-container"
            
            button {
                attr.``class`` (if isBoundary then "hywe-tab-btn active" else "hywe-tab-btn")
                on.click (fun _ -> dispatch (ToggleConfirm (Some (ConfirmAction.SwitchTo Boundary))))
                drawIcon pathBoundary
                text "BOUNDARY"
            }

            button {
                attr.``class`` (if isEditor then "hywe-tab-btn active" else "hywe-tab-btn")
                on.click (fun _ -> dispatch (ToggleConfirm (Some (ConfirmAction.SwitchTo (Editor false)))))
                drawIcon pathLayout
                text "HIERARCHY"
            }
        }

    /// Progress bar shown during batch generation
    let viewBatchProgress (progress: int) (total: int) =
        div {
            attr.``class`` "hywe-batch-progress-container"
            div {
                attr.``class`` "hywe-batch-progress-bar"
                attr.style (sprintf "width: %d%%;" (progress * 100 / total))
            }
            span { text (sprintf "Generating %d / %d..." progress total) }
        }

    /// Footer with scale information
    let viewFooter (model: Model) =
        footer {
            attr.``class`` "hywe-footer"
            div {
                attr.``class`` "hywe-scale-info"
                text (sprintf "Scale: %s | Elevation: %d" model.TeachMetadata.Scale model.Elevation)
            }
        }

    /// Draggable resize handle for panel widths
    let viewResizeHandle (dispatch: Message -> unit) =
        div {
            attr.``class`` "hywe-resize-handle"
            on.mousedown (fun _ -> ()) // Logic handled via JS normally
        }

    /// Overlay to catch pointer events during certain operations
    let viewPointerOverlay (totalSteps: float) (maxIdx: int) =
        concat {
            if maxIdx >= 0 then
                let rightOffset = (float (maxIdx + 1) / totalSteps) * 100.0
                let rightWidth = 100.0 - rightOffset
                div {
                    attr.style $"position: absolute; left: {rightOffset}%%; top: 0; bottom: 0; width: {rightWidth}%%; z-index: 5; cursor: default;"
                    "onpointerdown:stopPropagation" => true
                    "onclick:stopPropagation" => true
                }
        }

    // --- ICONS & GRAPHICS HELPERS ---



module Help =
    type TooltipDef = {
        Title: string
        Content: string
        TargetId: string
        Position: string
    }

    let getOnboardingStepData step =
        match step with
        | Welcome ->
            { Title = "Welcome to HYWE"
              Content = "The interactive tree on the right is the primary interface. This is where you will design space hierarchies."
              TargetId = "hywe-save-btn"
              Position = "top-left-header" }
        | BoundaryGuide ->
            { Title = "Unbound by Default"
              Content = "Activate Boundary to manipulate the interactive editor and specify the Entry zone. Activate Relative to reproportion sizes to fit inside."
              TargetId = "hywe-polygon-editor"
              Position = "lower-left-slider" }
        | NodeGuide ->
            { Title = "Chart your Design Intent"
              Content = "Change labels and sizes inline, click + to Add a Child Node."
              TargetId = "hywe-input-interactive"
              Position = "top-left-header" }
        | NodeMenuGuide ->
            { Title = "Additional Options"
              Content = "Click the ☰ icon on any node to reveal more actions like Delete or Elevate."
              TargetId = "hywe-input-interactive"
              Position = "top-left-header" }
        | ElevateGuide ->
            { Title = "Vertical Hierarchies"
              Content = "Elevate a node to create a new level. Use LEVELS to switch between them and design layered stacks."
              TargetId = "hywe-input-interactive"
              Position = "top-left-header" }
        | MoveNodeGuide ->
            { Title = "Organize Hierarchy"
              Content = "Drag and drop nodes to reorganize. Dropping a node onto another in a different branch makes it a sibling."
              TargetId = "hywe-input-interactive"
              Position = "top-left-header" }
        | LayoutGuide ->
            { Title = "The Procedural Variations"
              Content = "Slide to explore the 24 alternate configurations generated for the tree structure defined above."
              TargetId = "hywe-sequence-selector"
              Position = "lower-left-slider" }
        | Finish ->
            { Title = "Now let's give HYWE a try!"
              Content = "Explore the presets or create a fresh workflow. Click 'hyWEAVE' to generate any updated configuration."
              TargetId = "hywe-hyweave"
              Position = "top-left-header" }

    /// Helper to render text with colored + and -/- symbols
    let renderFormattedContent (content: string) =
        let words = content.Split([| ' ' |], System.StringSplitOptions.None)
        span {
            for i = 0 to words.Length - 1 do
                let word = words.[i]
                let cleanWord = word.TrimEnd([| '.'; ','; ';'; ':' |])
                if cleanWord = "Boundary" || cleanWord = "Relative" then
                    strong { text word }
                else
                    for j = 0 to word.Length - 1 do
                        let c = word.[j]
                        match c with
                        | '+' -> span { attr.``class`` "onb-green"; text "+" }
                        | 'x' | '×' when cleanWord.Length = 1 -> span { attr.``class`` "onb-orange"; text (string c) }
                        | '☰' -> span { attr.``class`` "onb-blue"; text "☰" }
                        | 'E' when cleanWord = "Entry" -> strong { text "E" }
                        | _ -> text (string c)
                if i < words.Length - 1 then text " "
        }

    let viewHelp (state: OnboardingState) (dispatch: Message -> unit) =
        let data = getOnboardingStepData state.CurrentStep
        let stepId = 
            match state.CurrentStep with
            | Welcome -> "onboarding-step-Welcome"
            | BoundaryGuide -> "onboarding-step-BoundaryGuide"
            | NodeGuide -> "onboarding-step-NodeGuide"
            | NodeMenuGuide -> "onboarding-step-NodeMenuGuide"
            | ElevateGuide -> "onboarding-step-ElevateGuide"
            | MoveNodeGuide -> "onboarding-step-MoveNodeGuide"
            | LayoutGuide -> "onboarding-step-LayoutGuide"
            | Finish -> "onboarding-step-Finish"

        div {
            attr.``class`` "onboarding-overlay"
            attr.style "z-index: 9999;"
            
            div {
                attr.id stepId
                attr.``class`` ("onboarding-tooltip " + data.Position)
                
                div {
                    attr.``class`` "tooltip-glass"

                    if not (System.String.IsNullOrWhiteSpace data.Title) then
                        h4 {
                            attr.style "margin: 0 0 4px 0; font-size: 0.75rem; font-weight: 700; color: #2E86C1; letter-spacing: -0.2px; text-align: center;"
                            text data.Title
                        }

                    p { 
                        attr.style "margin: 0 0 8px 0; font-size: 0.7rem; line-height: 1.3; color: #555; text-align: center;"
                        renderFormattedContent data.Content
                    }

                    div {
                        attr.style "display: flex; justify-content: space-between; align-items: center; padding: 0 4px;"
                        
                        button {
                            attr.``class`` "onboarding-link"
                            attr.style (if state.CurrentStep = Welcome then "opacity: 0; pointer-events: none;" else "font-weight: bold;")
                            on.click (fun _ -> dispatch PreviousOnboardingStep)
                            text "<"
                        }

                        button {
                            attr.``class`` "onboarding-skip-link"
                            attr.style "font-weight: bold; font-size: 0.8rem; color: #e67e22;"
                            on.click (fun _ -> dispatch SkipOnboarding)
                            text "x"
                        }

                        button {
                            attr.``class`` "onboarding-link"
                            attr.style "font-weight: bold;"
                            on.click (fun _ -> dispatch NextOnboardingStep)
                            text ">"
                        }
                    }
                }
            }
        }

module TreeFiltering =
    let getHierarchicalIdMap (tree: Hywe.Node.SubModel) =
        let rec traverse (m: string) (prefix: string) (node: Hywe.Node.TreeNode) =
            seq {
                yield node.Id, $"{m}.{prefix}"
                yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
            }
        
        seq {
            for kvp in tree.Levels do
                yield! traverse $"L{kvp.Key}" "1" kvp.Value
            for kvp in tree.Nests do
                yield! traverse $"N{kvp.Key}" "1" kvp.Value
        } |> Map.ofSeq

    let getValidIdsForMarker (tree: Hywe.Node.SubModel) (marker: string) =
        let rec getIds (m: string) (prefix: string) (node: Hywe.Node.TreeNode) =
            seq {
                yield $"{m}.{prefix}"
                yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getIds m $"{prefix}.{i + 1}" child)
            }
        
        if marker.StartsWith("N") then
            let nestId = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
            match tree.Nests |> Map.tryFind nestId with
            | Some nestNode -> getIds marker "1" nestNode |> Set.ofSeq
            | None -> Set.empty
        else
            let lvl = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0
            match tree.Levels |> Map.tryFind lvl with
            | Some levelNode -> getIds marker "1" levelNode |> Set.ofSeq
            | None -> Set.empty

    let getValidIds (tree: Hywe.Node.SubModel) =
        match tree.ActiveNest with
        | Some nestId -> getValidIdsForMarker tree $"N{nestId}"
        | None -> getValidIdsForMarker tree (match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}")

    let filterBatchConfigForMarker (computeExpensive: bool) (tree: Hywe.Node.SubModel) (marker: string) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
        let validIds = getValidIdsForMarker tree marker
        if validIds.IsEmpty then config
        else
            let indexed = 
                config.cxCxl1 
                |> Array.indexed 
                |> Array.filter (fun (_, (c: Hywe.Core.Coxel.Cxl)) -> validIds.Contains(Hywe.Core.Coxel.prpVlu c.Rfid))
            
            let cxls = indexed |> Array.map (fun (i, _) -> config.cxCxl1.[i])
            let clrs = indexed |> Array.map (fun (i, _) -> config.cxClr1.[i])
            let avls = indexed |> Array.map (fun (i, _) -> config.cxlAvl.[i])
            let b36s = indexed |> Array.map (fun (i, _) -> config.cxB36.[i])
            
            let validNames = cxls |> Array.map (fun c -> Hywe.Core.Coxel.prpVlu c.Name) |> Set.ofArray
            let shapes = config.shapes |> Array.filter (fun s -> validNames.Contains(s.name))
            
            let adj = if computeExpensive then Hywe.Core.Coxel.cxlAdj cxls else config.cxAdj1

            {| config with 
                cxCxl1 = cxls
                cxClr1 = clrs
                cxlAvl = avls
                shapes = shapes
                cxAdj1 = adj
                cxB36 = b36s
                cxSol1 = config.cxSol1 |}

    let filterBatchConfig (computeExpensive: bool) (tree: Hywe.Node.SubModel) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
        filterBatchConfigForMarker computeExpensive tree (match tree.ActiveNest with | Some nestId -> $"N{nestId}" | None -> match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}") config
