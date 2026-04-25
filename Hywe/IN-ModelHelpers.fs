module ModelHelpers

open Microsoft.JSInterop
open Elmish
open Layout
open Page
open NodeCode
open PolygonEditor
open ModelTypes
open Bolero.Html

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
                    match i >= 24 || token.IsCancellationRequested with 
                    | true -> return acc
                    | false ->
                        let sqnStr = indexToSqn i
                        let forcedStr = injectSqn m.SrcOfTrth sqnStr
                        let cxls, _ = Parse.generateCxlLayout forcedStr None None None [||]
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
                { Root = laidOut; ConfirmingId = None; DraggingId = None; PendingDragId = None; DropTargetId = None; SvgInfo = None; PointerDownPos = None; LastMoveMs = None }
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
                let cxls, _ = Parse.generateCxlLayout forcedStr None None None [||]
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
                    { Root = laidOut; ConfirmingId = None; DraggingId = None; PendingDragId = None; DropTargetId = None; SvgInfo = None; PointerDownPos = None; LastMoveMs = None }
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
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px; gap: 5px;"
            div {
                attr.style "flex:1; overflow-y:hidden; display: flex; justify-content: center; width: 100%;"
                viewTreeEditor model.Tree (TreeMsg >> dispatch)
            }
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
                span { attr.``class`` "spinner" }
                span { 
                    attr.``class`` "label-stack"
                    span { attr.``class`` "weaving-label"; text " h y W E A V E i n g . . ." }
                    span { 
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
        tab "Table"    iconTable    TablePanel
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
                    svgCoxels model.Derived.cxCxl1 model.Derived.cxOuIl elv model.Derived.cxClr1 10 (Some "layout-svg-output")
                }
                button {
                    attr.``class`` "layout-download-btn"
                    on.click (fun _ ->
                        let datePart = System.DateTime.Now.ToString("yyMMddmm")
                        let fileName = "HyweLayout_" + datePart + ".svg"
                        js.InvokeVoidAsync("downloadSvgFile", "layout-svg-output", fileName).AsTask()
                        |> ignore
                    )
                    text "Download SVG"
                }
            }
        
        | TablePanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    Table.viewHyweTable model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxlAvl
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
                        attr.style "width: 100%; height: 100%; display: block;" 
                    }
                    
                    svg {
                        attr.id "webgl-labels-overlay"
                        attr.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; overflow: visible;"
                        
                        for i, cxl in model.Derived.cxCxl1 |> Array.indexed do
                            let name = Coxel.prpVlu cxl.Name
                            let massColor = 
                                match model.Derived.cxClr1 <> null && i < model.Derived.cxClr1.Length with 
                                | true -> model.Derived.cxClr1.[i] 
                                | false -> "#666666"
                            let fw = match i = 0 with | true -> "700" | false -> "400"
                            let clr = match i = 0 with | true -> "#333333" | false -> "#666666"
                            let pId = sprintf "webgl-path-%d" i
                            
                            elt "g" {
                                "id" => sprintf "mass-label-g-%d" i
                                "display" => "none"
                                
                                elt "path" {
                                    "id" => pId
                                    "d" => "M -60 -5 Q 0 -25 60 -5"
                                    "fill" => "transparent"
                                }
                                elt "text" {
                                    "font-family" => "Verdana"; "font-size" => "10px"; "font-weight" => fw; "fill" => clr
                                    elt "textPath" {
                                        "href" => ("#" + pId)
                                        "startOffset" => "50%"; "text-anchor" => "middle"
                                        text name
                                    }
                                }
                                elt "circle" {
                                    "cx" => "0"; "cy" => "0"; "r" => "4"; "fill" => massColor; "stroke" => "#ffffff"; "stroke-width" => "1.5"
                                }
                            }
                    }

                    async { do! ThreeD.extrudePolygons js "hywe-extruded-polygon" model.Derived.cxCxl1 model.Derived.cxClr1 3.0 0 } 
                    |> Async.StartImmediate
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
                            attr.style "text-align:center; padding: 100px; color: #888; width: 100%;"
                            span { attr.``class`` "spinner"; attr.style "display: block; margin: 0 auto 20px;" }
                            text "Generating 24 variations..." 
                        }
            }

        | TeachPanel ->
            div {
                attr.``class`` "teach-panel-container"
                div {
                    attr.style "width: 100%; max-width: 800px;"
                    textarea {
                        attr.id "hynteract-desc-input"
                        attr.``class`` "teach-textarea"
                        attr.placeholder "Help Build a Dataset for Learning Building Layouts\n\nDescribe the space flow: Identify the main spaces, note which spaces contain smaller sub-spaces, and explain how movement branches from one area to another. Include a brief note on the overall ambience or character of the spaces.\n\nExample: Entry opens into the Living Room (30) which acts as the main gathering space of the residence. From the Living, one side leads to the Kitchen (12), which further connects to a Utility (6). The other side leads to a Passage (8) that connects the bedrooms. Along this passage are three rooms: a Master Bedroom (18) with an attached Toilet (5), a Guest Room (14), and Children's Room (12). A Common Toilet (5) also opens from the passage. The arrangement keeps the living as the central space, with kitchen activities on one side and the bedrooms grouped together along the passage."
                        attr.value model.UserDescription
                        on.input (fun e -> dispatch (SetDescription (unbox<string> e.Value)))
                    }
                }

                div {
                    attr.``class`` "teach-action-bar"

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
                        let isBusy = model.IsSavingToHynteract || System.String.IsNullOrWhiteSpace model.UserDescription
    
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
                    span { 
                        attr.style "color: #27ae60; font-size: 0.85em; font-weight: 500;"
                        text "? Spatial relationship successfully mapped to model." 
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
