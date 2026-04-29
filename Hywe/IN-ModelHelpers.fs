module ModelHelpers

open Microsoft.JSInterop
open Elmish
open Layout
open Page
open NodeCode
open PolygonEditor
open ModelTypes
open Bolero.Html

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

let downloadFile (js: IJSRuntime) (filename: string) (content: string) (contentType: string) =
    async {
        do! js.InvokeVoidAsync("eval", sprintf """
            (function() {
                const blob = new Blob([`%s`], { type: '%s' });
                const url = URL.createObjectURL(blob);
                const anchor = document.createElement('a');
                anchor.href = url;
                anchor.download = '%s';
                document.body.appendChild(anchor);
                anchor.click();
                document.body.removeChild(anchor);
                URL.revokeObjectURL(url);
            })()
        """ content contentType filename).AsTask() |> Async.AwaitTask
    }

let downloadSvg (js: IJSRuntime) (svgId: string) (filename: string) =
    async {
        let! source = js.InvokeAsync<string>("eval", sprintf "document.getElementById('%s').outerHTML" svgId).AsTask() |> Async.AwaitTask
        let clean = 
            source
                .Replace("onclick:", "")
                .Replace("onmouseover:", "")
                .Replace("onmouseout:", "")
                .Replace(" xmlns=\"http://www.w3.org/2000/svg\"", "") // Prevent double xmlns
                .Insert(4, " xmlns=\"http://www.w3.org/2000/svg\"") // Add it back cleanly
        
        let xmlHeader = "<?xml version=\"1.0\" standalone=\"no\"?>\r\n"
        let finalSvg = xmlHeader + clean
        
        do! downloadFile js filename finalSvg "image/svg+xml;charset=utf-8"
    }

let handleSetActivePanel (model: Model) (panel: ActivePanel) : Model * Cmd<Message> =
    match panel with
    | BatchPanel ->
        let isStale = Some model.SrcOfTrth <> model.LastBatchSrc
        match isStale with
        | true -> 
            // Initialize progress tracking and trigger recursive generation
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
                            Parse.generateCxlArray currentSrc sqnCase currentOuter currentIslands model.PolygonExport.EntryStr [||]
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
                    Typology = model.TeachMetadata.Typology
                    Flow = model.TeachMetadata.Flow
                    Ambience = model.TeachMetadata.Ambience
                    Complexity = model.TeachMetadata.Complexity
                    Scale = model.TeachMetadata.Scale
                |}
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
            Derived = deriveData clean model.PolygonExport.EntryStr 0 
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

// View helpers
let private viewNodeCodeButtons (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    let nodeCodeButtonText =
        match model.EditorMode with
        | Syntax -> "Node"
        | Interactive -> "Code"

    div {
        attr.style "display:flex; width: 100%; gap:10px; padding: 0 10px; justify-content: space-between; align-items: center;"
        
        div {
            attr.style "display:flex; gap:4px; align-items: center;"
            button {
                attr.id "hywe-save-btn"
                attr.``class`` "hywe-toggle-btn"
                on.click (fun _ -> dispatch SaveRequested)
                text "Save"
            }

            button {
                attr.``class`` "hywe-toggle-btn"
                on.click (fun _ -> dispatch ImportRequested)
                text "Load"
            }

            input {
                attr.id "hyw-import-hidden"
                attr.``type`` "file"
                attr.style "display:none"
                attr.accept ".hyw"
                on.change (fun e ->
                    async {
                        let! content = js.InvokeAsync<string>("readHywFile", "hyw-import-hidden").AsTask() |> Async.AwaitTask
                        dispatch (FileImported content)
                    } |> Async.StartImmediate
                )
            }
            
            button {
                attr.``class`` "hywe-toggle-btn"
                on.click (fun _ -> dispatch ToggleEditorMode)
                text nodeCodeButtonText
            }
        }
    }

let private viewEditorPanel (model: Model) (dispatch: Message -> unit) =
    match model.EditorMode with
    | Syntax ->
        div {
            attr.id "hywe-input-syntax"
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 5px 10px;"
            textarea {
                attr.``class`` "hyweSyntax"
                attr.key (model.SrcOfTrth.GetHashCode().ToString())
                attr.value model.SrcOfTrth
                on.change (fun e -> dispatch (SetSrcOfTrth (unbox<string> e.Value)))
            }
        }
    | Interactive ->
        div {
            attr.id "hywe-input-interactive"
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px; gap: 5px; flex: 1; overflow: hidden;"
            viewTreeEditor model.Tree (TreeMsg >> dispatch)
        }

let private viewHyweButton (model: Model) (dispatch: Message -> unit) =
    let syntaxAltered = model.NeedsHyweave && not model.IsHyweaving
    
    let buttonClass = 
        match model.IsHyweaving with
        | true -> "hyWeaveButton stop-state" 
        | false -> 
            match syntaxAltered with
            | true -> "hyWeaveButton needs-update"
            | false -> "hyWeaveButton"

    div {
        attr.``class`` "hyweave-container"
        button {
            attr.id "hywe-hyweave"
            attr.``class`` buttonClass
            attr.disabled model.IsCancelling 

            on.click (fun _ -> 
                match model.IsHyweaving with
                | true -> dispatch CancelBatch
                | false -> dispatch StartHyweave)
            
            match model.IsHyweaving with
            | true ->
                span { attr.key "hy-spinner"; attr.``class`` "spinner" }
                span { 
                    attr.key "hy-labels"
                    attr.``class`` "label-stack"
                    span { attr.key "weaving-lbl"; attr.``class`` "weaving-label"; text " h y W E A V E i n g . . ." }
                    span { 
                        attr.key "stop-lbl"
                        attr.``class`` "stop-label"
                        span { attr.style "color: #E67E22; font-weight: bold;white-space: pre"; text " S T O P " } 
                        text "h y W E A V E i n g" 
                    }
                }
            | false -> 
                match syntaxAltered with
                | true ->
                    span { attr.``class`` "hyweave-prompt"; text "syntax altered" }
                    span { attr.``class`` "hyweave-main-text"; text "h y W E A V E" }
                    span { attr.``class`` "hyweave-prompt"; text "to regenerate" }
                | false -> 
                    text "h y W E A V E"
        }
    }

let private viewHyweTabs (model: Model) (dispatch: Message -> unit) =
    div {
        attr.``class`` "hywe-tab-strip"
        
        let tab title path panel =
            let isActive = model.ActivePanel = panel
            let activeClass = match isActive with true -> " active" | false -> ""

            button {
                attr.title title 
                attr.``class`` ("hywe-tab-btn" + activeClass)
                on.click (fun _ -> dispatch (SetActivePanel panel))
                
                // Show text if active, icon if inactive
                match isActive with
                | true -> text title
                | false -> drawIcon path
            }

        tab "Boundary" iconBoundary BoundaryPanel
        tab "Layout"   iconLayout   LayoutPanel
        tab "Analyze"    iconAnalyze    AnalyzePanel
        tab "3D"       icon3D       ViewPanel
        tab "Batch"    iconBatch    BatchPanel
        tab "Teach"    iconTeach    TeachPanel
    }

let private viewHywePanels (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    div {
        attr.style "padding: 10px; min-height: 400px;"
        
        match model.ActivePanel with
        | BoundaryPanel ->
            let currentInner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            div { 
                attr.id "hywe-polygon-editor"
                PolygonEditor.view currentInner (PolygonEditorMsg >> dispatch) js 
            }

        | LayoutPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-svg-container"
                    svgCoxels model.Derived.cxCxl1 model.Derived.cxOuIl model.Tree.ActiveLevel model.Derived.cxClr1 10 (Some "layout-svg-output")
                }
                button {
                    attr.``class`` "layout-download-btn"
                    on.click (fun _ ->
                        let datePart = System.DateTime.Now.ToString("yyMMddmm")
                        let fileName = "HyweLayout_" + datePart + ".svg"
                        async {
                            do! downloadSvg js "layout-svg-output" fileName
                        } |> Async.StartImmediate
                    )
                    text "Download SVG"
                }
            }
        
        | AnalyzePanel ->
            let elv = model.Tree.ActiveLevel
            let filteredIdx = 
                model.Derived.cxCxl1 
                |> Array.indexed 
                |> Array.filter (fun (_, c) -> 
                    let (_, _, z) = Hexel.hxlCrd c.Base
                    z = elv)
                |> Array.map fst
            
            let fCxls = filteredIdx |> Array.map (fun i -> model.Derived.cxCxl1.[i])
            let fClrs = filteredIdx |> Array.map (fun i -> model.Derived.cxClr1.[i])
            let fAvls = filteredIdx |> Array.map (fun i -> model.Derived.cxlAvl.[i])

            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    Analyze.viewHyweAnalyze model.Sequence fCxls fClrs fAvls
                }
            }

        | ViewPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px; width: 100%; overflow-x: hidden;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "padding: 8px 0;width: 100%; max-width: 100vw;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }                
                div {
                    attr.style "width: 95%; max-width: 100%; aspect-ratio: 3/2; position: relative; overflow: hidden; background: #f9f9f9; border-radius: 8px;"
                    canvas { 
                        attr.id "hywe-extruded-polygon"
                        attr.style "width: 100%; height: 100%; display: block; touch-action: none;" 
                    }
                    async { do! ThreeD.extrudePolygons js "hywe-extruded-polygon" model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxElv1 model.ViewLocked }
                    |> Async.StartImmediate
                }

                div {
                    attr.style "display: flex; gap: 15px; margin-top: 40px; align-items: center;"
                    div {
                        attr.``class`` ("view-lock-trigger" + (if model.ViewLocked then " locked" else ""))
                        attr.style "position: relative; top: 1.125rem;"
                        attr.title (if model.ViewLocked then "Unlock View" else "Lock View")
                        on.click (fun _ -> dispatch ToggleViewLock)
                        drawIconSized "1.5rem" (if model.ViewLocked then iconLocked else iconUnlocked)
                    }
                    button {
                        attr.``class`` "layout-download-btn"
                        attr.disabled (not model.ViewLocked)
                        attr.title (if model.ViewLocked then "Download View as SVG" else "Lock view to enable SVG export")
                        on.click (fun _ -> dispatch Download3DSvg)
                        text "Download SVG"
                    }
                }
                    
                // Restored delicate 3D Legend
                div {
                    attr.style "display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; padding: 15px 10px; width: 100%; border-top: 1px solid #f0f0f0; margin-top: 5px;"
                    for i in 0 .. (min model.Derived.cxCxl1.Length model.Derived.cxClr1.Length - 1) do
                        let name = Coxel.prpVlu model.Derived.cxCxl1.[i].Name
                        let color = model.Derived.cxClr1.[i]
                        div {
                            attr.style "display: flex; align-items: center; gap: 6px; font-size: 11px; font-family: 'Inter', sans-serif; color: #666;"
                            div {
                                attr.style (sprintf "width: 12px; height: 12px; border-radius: 2px; background: %s;" color)
                            }
                            text name
                        }
                }
            }

        | BatchPanel ->
            div {
                attr.style "width: 100vw; margin-left: calc(-50vw + 50%); min-height: 500px; display: flex; flex-direction: column; align-items: center; background: #ffffff;"
                cond model.BatchPreview <| function
                    | Some results -> 
                        alternateConfigurations 
                            results 
                            model.SelectedPreviewIndex 
                            TapBatchPreview                   
                            dispatch                   
                            (fun () -> dispatch (SetActivePanel LayoutPanel)) js
                    | None -> 
                        div { 
                            attr.style "text-align:center; padding: 40px 20px; color: #888; width: 100%; display: flex; flex-direction: column; align-items: center;"
                            
                            // Text Above - Multi-line, width constrained to match 4x6 grid (156px)
                            div {
                                attr.style "font-family: 'Inter', sans-serif; font-size: 1.1em; letter-spacing: 0.5px; color: #666; width: 156px; margin-bottom: 15px; text-align: center; font-weight: 500; line-height: 1.3;"
                                text "Generating Configurations"
                            }

                            span { attr.``class`` "spinner"; attr.style "display: block; margin-bottom: 25px;" }
                            
                            // Progress Grid (4x6) - Delicate filleted squares
                            div {
                                // 4 columns * 14px + 3 gaps * 14px = 56 + 42 = 98px
                                attr.style "display: grid; grid-template-columns: repeat(4, 14px); grid-template-rows: repeat(6, 14px); gap: 14px; margin: 0 auto; justify-content: center; width: 98px;"
                                for i in 0 .. 23 do
                                    let isComplete = i < model.BatchProgress
                                    div {
                                        attr.style (sprintf "width: 14px; height: 14px; border: 1px solid #e0e0e0; border-radius: 3px; background: %s; transition: all 0.5s ease;" 
                                            (if isComplete then "rgba(136, 136, 136, 0.4)" else "transparent"))
                                    }
                            }
                        }
            }

        | TeachPanel ->
            let selectField (label: string) (current: string) (options: string list) (descriptions: Map<string, string>) updater =
                let isPredefined = options |> List.contains current
                let rowTips = descriptions |> Map.toSeq |> Seq.map snd |> Set.ofSeq
                
                let currentTip = 
                    match model.HoveredInfo with
                    | Some (tip: string) when rowTips.Contains tip -> Some tip
                    | Some (tip: string) when tip.Contains(label.ToLower()) -> Some tip // For "Other..." tips
                    | _ -> 
                        if isPredefined then descriptions |> Map.tryFind current
                        else Some $"Custom {label.ToLower()} tag applied."

                div {
                    attr.``class`` "teach-select-row"
                    span { attr.``class`` "teach-field-label"; text label }
                    div {
                        attr.``class`` "teach-option-group"
                        for opt in options do
                            button {
                                attr.``class`` (if current = opt then "teach-option active" else "teach-option")
                                on.mouseover (fun _ -> dispatch (SetHoveredInfo (descriptions |> Map.tryFind opt)))
                                on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                                on.click (fun _ -> dispatch (UpdateMetadata (fun m -> updater m opt)))
                                text opt
                            }
                        button {
                            attr.``class`` (if not isPredefined then "teach-option active" else "teach-option")
                            on.mouseover (fun _ -> dispatch (SetHoveredInfo (Some $"Enter a custom {label.ToLower()} tag.")))
                            on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                            on.click (fun _ -> dispatch (UpdateMetadata (fun m -> updater m "")))
                            text "Other..."
                        }
                    }
                    
                    match currentTip with
                    | Some tip -> div { attr.``class`` "teach-row-tip"; text tip }
                    | None -> ()

                    if not isPredefined then
                        input {
                            attr.``class`` "teach-custom-input"
                            attr.placeholder (sprintf "Enter custom %s..." (label.ToLower()))
                            attr.value current
                            on.input (fun e -> dispatch (UpdateMetadata (fun m -> updater m (unbox<string> e.Value))))
                        }
                }

            let flowDescs = Map [
                "Sequential", "A 'deep' flow where spaces lead into one another in a chain."
                "Hub-Spoke", "A 'shallow' flow where most spaces branch directly from a single hub."
                "Hierarchical", "A balanced tree where primary spaces lead to secondary clusters."
                "Spine", "Organized along a primary corridor node that anchors all branches."
                "Cluster", "Independent groups of spaces connected only at the highest level."
            ]

            let typoDescs = Map [
                "Residential", "Homes, apartments, or private living quarters."
                "Office", "Workspaces, studios, or corporate environments."
                "Retail", "Shops, boutiques, or commercial display spaces."
                "Clinic", "Healthcare, consulting, or wellness spaces."
                "Studio", "Open creative spaces or specialized workshops."
            ]

            let ambiDescs = Map [
                "Modern", "Balanced, functional, and contemporary arrangement."
                "Cellular", "Highly partitioned with many private, enclosed spaces."
                "Open", "Minimizes walls to maximize visual and spatial continuity."
                "Minimal", "Stripped back to essential connections and clean flow."
                "Luxurious", "Generous circulation, focal points, and grand proportions."
            ]

            let compDescs = Map [
                "Simple", "Clear, direct relationships with few nodes."
                "Medium", "Standard architectural complexity with nested zones."
                "Complex", "Intricate multi-level or multi-wing arrangements."
                "Expert", "Highly specialized or experimental spatial logic."
            ]

            let scaleDescs = Map [
                "Building", "Multi-level structures or entire building envelopes."
                "Layout", "Single-level spatial arrangements and room logic."
                "Master Plan", "Multiple buildings or campus-level planning."
                "Urban", "Large scale city blocks or neighborhood logic."
            ]

            div {
                attr.``class`` "teach-panel-container"
                
                div {
                    attr.``class`` "teach-objective-section"
                    
                    selectField "Scale" model.TeachMetadata.Scale [ "Building"; "Layout"; "Master Plan"; "Urban" ] scaleDescs (fun m v -> { m with Scale = v })
                    selectField "Typology" model.TeachMetadata.Typology [ "Residential"; "Office"; "Retail"; "Clinic"; "Studio" ] typoDescs (fun m v -> { m with Typology = v })
                    selectField "Flow" model.TeachMetadata.Flow [ "Sequential"; "Hub-Spoke"; "Hierarchical"; "Spine"; "Cluster" ] flowDescs (fun m v -> { m with Flow = v })
                    selectField "Ambience" model.TeachMetadata.Ambience [ "Modern"; "Cellular"; "Open"; "Minimal"; "Luxurious" ] ambiDescs (fun m v -> { m with Ambience = v })
                    selectField "Complexity" model.TeachMetadata.Complexity [ "Simple"; "Medium"; "Complex"; "Expert" ] compDescs (fun m v -> { m with Complexity = v })
                }

                div {
                    attr.style "width: 100%; margin-top: 1.5rem;"
                    textarea {
                        attr.id "hynteract-desc-input"
                        attr.``class`` "teach-textarea small"
                        attr.placeholder "Optional: Describe any unique spatial nuances..."
                        attr.value model.UserDescription
                        on.input (fun e -> dispatch (SetDescription (unbox<string> e.Value)))
                    }
                }

                div {
                    attr.``class`` "teach-action-bar"

                    div {
                        attr.style "display: flex; gap: 0.5rem;"
                        button {
                            attr.``class`` (match model.IsRecording with | true -> "mic-button recording" | false -> "mic-button")
                            attr.title "Start Voice Capture"
                            on.click (fun _ -> dispatch StartVoiceCapture)
                            
                            rawHtml """
                                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <path d="M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z"></path>
                                    <path d="M19 10v2a7 7 0 0 1-14 0v-2"></path>
                                    <line x1="12" y1="19" x2="12" y2="23"></line>
                                    <line x1="8" y1="23" x2="16" y2="23"></line>
                                </svg>"""
                        }

                        button {
                            attr.``class`` "mic-button"
                            attr.style "color: #3498db;"
                            attr.title "Suggest Architectural Story"
                            on.click (fun _ -> dispatch SuggestDescription)
                            
                            rawHtml """
                                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <polygon points="12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2"></polygon>
                                </svg>"""
                        }

                        button {
                            attr.``class`` "mic-button"
                            attr.style "color: #e74c3c;"
                            attr.title "Clear Description"
                            on.click (fun _ -> dispatch (SetDescription ""))
                            
                            rawHtml """
                                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <polyline points="3 6 5 6 21 6"></polyline>
                                    <path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path>
                                    <line x1="10" y1="11" x2="10" y2="17"></line>
                                    <line x1="14" y1="11" x2="14" y2="17"></line>
                                </svg>"""
                        }
                    }

                    button {
                        let isBusy = model.IsSavingToHynteract
    
                        attr.``class`` (
                            match isBusy with 
                            | true -> "record-submit-btn disabled" 
                            | false -> "record-submit-btn active"
                        )
    
                        let btnText = 
                            match model.IsSavingToHynteract with 
                            | true -> "Committing..." 
                            | false -> "Commit to Dataset"
    
                        attr.disabled isBusy
                        on.click (fun _ -> dispatch RecordToHynteract)
                        text btnText
                    }
                }

                match model.ShowSuccessMessage with
                | true ->
                    div {
                        attr.style "margin-top: 1rem; text-align: center;"
                        span { 
                            attr.style "color: #27ae60; font-size: 0.9em; font-weight: 600;"
                            text "✓ Spatial intent successfully committed to dataset." 
                        }
                    }
                | false -> ()
            }
    }

let view model dispatch (js: IJSRuntime) =
    concat {
        viewNodeCodeButtons model dispatch js
        viewEditorPanel model dispatch
        viewHyweButton model dispatch
        viewHyweTabs model dispatch 
        viewHywePanels model dispatch js
    }
