module ModelHelpers

open System
open Microsoft.JSInterop
open Layout
open Hywe
open Page
open Hywe.Node
open Hywe.Site
open ModelTypes
open Bolero.Html
open Hywe.Core

let viewConfirmOverlay (model: Model) (dispatch: Message -> unit) =
    match model.PendingConfirm with
    | None -> empty()
    | Some action ->
        let title, msg, confirmMsg, onConfirm =
            match action with
            | ConfirmAction.ResetWorkspace ->
                "Reset Layout?", ["Current layout will be replaced."], "Reset", HardReset
            | ConfirmAction.LoadPreset (name, label) ->
                (sprintf "Load %s preset?" label), ["Current layout will be replaced."], "Load", SelectPreset name
            | ConfirmAction.LoadGallery (name, rowId) ->
                (sprintf "Load %s?" name), ["Current layout will be replaced."], "Load", LoadGalleryDefinition (name, rowId)
            | ConfirmAction.SwitchTo tab ->
                "Switch View", ["Switch to this view?"], "Switch", SetActivePanel (match tab with Boundary -> BoundaryPanel | _ -> LayoutPanel)

        div {
            attr.style "position: fixed; inset: 0; background: rgba(255,255,255,0.7); backdrop-filter: blur(4px); z-index: 10000; display: flex; align-items: center; justify-content: center; animation: fadeIn 0.3s ease;"
            on.pointerdown (fun _ -> dispatch (ToggleConfirm None))
            
            div {
                attr.style "background: #fff; border: 1px solid #eee; padding: 20px 24px; border-radius: 8px; width: 220px; box-shadow: 0 10px 30px rgba(0,0,0,0.05); display: flex; flex-direction: column; gap: 12px; text-align: center; pointer-events: auto; justify-content: center;"
                "onclick:stopPropagation" => true
                
                let baseTitle, suffix =
                    if title.EndsWith("?") then
                        title.Substring(0, title.Length - 1), "?"
                    else
                        title, ""
                
                div {
                    attr.style "display: flex; justify-content: center; width: 100%; font-weight: 600; font-size: 1.1rem; color: #333;"
                    div {
                        attr.style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;"
                        text baseTitle
                    }
                    if suffix <> "" then
                        div {
                            attr.style "flex-shrink: 0;"
                            text suffix
                        }
                }
                div {
                    attr.style "font-size: 0.9rem; color: #666; line-height: 1.3; display: flex; flex-direction: column; gap: 4px;"
                    for line in msg do
                        div { text line }
                }
                div {
                    attr.style "display: flex; flex-direction: column; gap: 8px; margin-top: 8px;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                        on.pointerdown (fun _ -> dispatch (ToggleConfirm None))
                        text "Cancel"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-dark"
                        on.pointerdown (fun _ -> dispatch onConfirm)
                        text confirmMsg
                    }
                }
            }
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
        do! js.InvokeVoidAsync("downloadSvgFile", svgId, filename).AsTask() |> Async.AwaitTask
    }

// View helpers
let private iconSwitchNode model = drawMenuIcon (match model.EditorMode with Syntax -> pathSwitchNode | Interactive -> pathSwitchCode)
let private iconSave             = drawMenuIcon pathSave
let private iconLoad             = drawMenuIcon pathLoad
let private iconShare model      = drawMenuIcon pathShare
let private iconReset            = drawMenuIcon pathReset
let private iconUndo             = drawMenuIcon pathUndo
let private iconRedo             = drawMenuIcon pathRedo

let private toolbarBtn (title: string) (msg: Message option) (icon: Bolero.Node) (dispatch: Message -> unit) (cls: string) (style: string) =
    match msg with
    | Some m ->
        button {
            attr.``class`` ("hywe-btn hywe-btn-sm hywe-btn-flat " + cls)
            attr.style ("padding: 2px; " + style)
            attr.title title
            on.click (fun _ -> dispatch m)
            icon
        }
    | None ->
        button {
            attr.``class`` ("hywe-btn hywe-btn-sm hywe-btn-flat " + cls)
            attr.style ("padding: 2px; " + style)
            attr.title title
            icon
        }

// View helpers
let private drawerActionBtn (title: string) (label: string) (msg: Message option) (icon: Bolero.Node) (dispatch: Message -> unit) (style: string) =
    let btnCls = "hywe-btn hywe-btn-sm hywe-btn-flat"
    let content =
        concat {
            icon
            span {
                attr.style "font-size: 7px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.6px; margin-top: 1px; color: inherit; opacity: 0.8;"
                text label
            }
        }
    match msg with
    | Some m ->
        button {
            attr.``class`` btnCls
            attr.style ("padding: 4px 2px; display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 38px; gap: 2px; " + style)
            attr.title title
            on.click (fun _ -> dispatch m)
            content
        }
    | None ->
        button {
            attr.``class`` btnCls
            attr.style ("padding: 4px 2px; display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 38px; gap: 2px; opacity: 0.3; pointer-events: none; " + style)
            attr.title title
            content
        }

// View helpers
let private viewNodeCodeButtons (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    concat {
        div {
            attr.style "display:flex; width: 100%; gap:0px; padding: 0 4px; justify-content: flex-start; align-items: center; position: relative; z-index: 3000; pointer-events: none;"
            
            // 1. Editor Toggle & Undo/Redo (on the top left edge)
            div {
                attr.style "margin-left: 4px; margin-top: 2px; pointer-events: auto; display: flex; align-items: center; gap: 4px;"
                
                toolbarBtn 
                    (match model.EditorMode with Syntax -> "Switch to Node Editor" | Interactive -> "Switch to Code Editor")
                    (Some ToggleEditorMode)
                    (iconSwitchNode model)
                    dispatch "" ""

                // Undo/Redo
                let canUndo = model.UndoStack <> []
                let canRedo = model.RedoStack <> []

                toolbarBtn 
                    "Undo (Ctrl+Z)" 
                    (match canUndo with true -> Some Undo | false -> None) 
                    iconUndo 
                    dispatch 
                    "" (sprintf "opacity: %s;" (match canUndo with true -> "1" | false -> "0.3"))

                toolbarBtn 
                    "Redo (Ctrl+Y)" 
                    (match canRedo with true -> Some Redo | false -> None) 
                    iconRedo 
                    dispatch 
                    "" (sprintf "opacity: %s;" (match canRedo with true -> "1" | false -> "0.3"))
            }

            match model.InstallPromptAvailable with
            | false -> empty()
            | true ->
                div {
                    attr.style "margin-left: auto; margin-right: 10px; margin-top: 6px; pointer-events: auto; display: flex; align-items: center;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet"
                        attr.title "Install as an App"
                        attr.style "display: flex; align-items: center; gap: 2px; padding: 1px 4px; border: 1px solid rgba(0,0,0,0.1); background: transparent; box-shadow: none; opacity: 0.6; transition: opacity 0.2s ease;"
                        on.click (fun _ -> dispatch InstallRequested)
                        
                        rawHtml """<svg width="9" height="9" viewBox="0 0 24 24" fill="none" stroke="#666" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path><polyline points="7 10 12 15 17 10"></polyline><line x1="12" y1="15" x2="12" y2="3"></line></svg>"""
                        span { attr.style "font-size: 8.5px; font-weight: 400; color: #666; letter-spacing: -0.3px;"; text "Install" }
                    }
                }
        }
        
        // 2. Exchange Drawer (Always available, top drawer)
        let isWorkspaceCollapsed = model.IsWorkspaceCollapsed
        concat {
            div {
                attr.style (match isWorkspaceCollapsed with true -> "display: none;" | false -> "position: fixed; inset: 0; z-index: 1800; background: transparent; pointer-events: auto;")
                on.click (fun _ -> dispatch ToggleWorkspaceCollapse)
            }

            div {
                attr.``class`` (match isWorkspaceCollapsed with true -> "preset-drawer collapsed" | false -> "preset-drawer")
                attr.style "top: 65px;"
                
                div {
                    attr.``class`` "preset-drawer-content"
                    attr.style "min-width: 100px;"
                    
                    div {
                        attr.style "display: grid; grid-template-columns: repeat(2, 1fr); gap: 4px;"
                        
                        // Save
                        drawerActionBtn "Save" "Save" (Some SaveRequested) iconSave dispatch ""

                        // Load
                        drawerActionBtn "Load" "Load" (Some ImportRequested) iconLoad dispatch ""

                        // Share
                        drawerActionBtn 
                            (match model.ShowLinkCopied with true -> "Link Shared!" | false -> "Share Link")
                            (match model.ShowLinkCopied with true -> "Copied" | false -> "Share")
                            (Some ShareLink)
                            (iconShare model)
                            dispatch
                            (sprintf "color: %s;" (match model.ShowLinkCopied with true -> "#27ae60" | false -> "#555"))

                        // Reset
                        drawerActionBtn 
                            "Hard Reset" 
                            "Reset"
                            (Some (ToggleConfirm (Some ConfirmAction.ResetWorkspace))) 
                            iconReset 
                            dispatch 
                            ""
                    }

                    div {
                        attr.style "margin-top: 6px; display: flex; flex-direction: column; gap: 2px; align-items: flex-start;"
                        
                        // Header row with Presets
                        div {
                            attr.style "font-size: 10px; font-weight: 700; color: #999; text-transform: uppercase; letter-spacing: 0.5px; text-align: left; border-bottom: 1px solid #e0e0e0; margin-bottom: 2px; padding-bottom: 4px; width: 100%;"
                            text "Presets"
                        }
                        
                        let presetLink name label isActive =
                            if isActive then
                                a {
                                    attr.style "cursor: pointer; color: #666; font-size: 0.85rem; text-decoration: none;"
                                    "onclick:stopPropagation" => true
                                    on.click (fun _ -> dispatch (ToggleConfirm (Some (ConfirmAction.LoadPreset (name, label)))))
                                    text label
                                }
                            else
                                span {
                                    attr.style "color: #bbb; font-size: 0.85rem; cursor: not-allowed;"
                                    text label
                                }

                        div {
                            attr.style "display: grid; grid-template-columns: 1fr 1fr; gap: 4px; width: 100%; margin-top: 2px;"
                            presetLink "Simple" "Simple" true
                            presetLink "Branched" "Branch" true
                            presetLink "Stacked" "Stack" true
                            presetLink "Nest" "Nest" false
                        }
                        
                        a {
                            attr.style "font-size: 10px; font-weight: 700; color: #999; text-transform: uppercase; letter-spacing: 0.5px; text-align: left; cursor: pointer; text-decoration: none; margin-top: 4px; padding-top: 4px; border-top: 1px solid #e0e0e0; width: 100%; display: block;"
                            "onclick:stopPropagation" => true
                            on.click (fun _ -> 
                                dispatch ToggleGallery
                                dispatch ToggleWorkspaceCollapse
                            )
                            text "Gallery"
                        }
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
                }

                div {
                    attr.``class`` "preset-drawer-handle"
                    on.click (fun _ -> dispatch ToggleWorkspaceCollapse)
                    span { 
                        attr.style "white-space: pre;"
                        text "Waggle  Exchange" 
                    }
                }
            }
        }


    }

let private viewEditorPanel (model: Model) (dispatch: Message -> unit) =
    match model.EditorMode with
    | Syntax ->
        div {
            attr.id "hywe-input-syntax"
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 5px 10px 5px 30px;"
            textarea {
                attr.``class`` "hyweSyntax"
                attr.style "min-height: 185px;"
                attr.key (model.SrcOfTrth.GetHashCode().ToString())
                attr.value model.SrcOfTrth
                on.change (fun e -> dispatch (SetSrcOfTrth (unbox<string> e.Value)))
            }
        }
    | Interactive ->
        div {
            attr.id "hywe-input-interactive"
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; box-sizing: border-box; padding: 0 10px; gap: 5px; flex: 1; overflow: hidden;"
            NodeTree.viewTreeEditor model.Tree [||] (TreeMsg >> dispatch)
        }

let private viewHyweButton (model: Model) (dispatch: Message -> unit) =
    let syntaxAltered = model.NeedsHyweave && not model.IsHyweaving
    
    let buttonClass = 
        let baseClass = "hywe-btn hywe-btn-lg hywe-btn-dark hyWeaveButton"
        match model.IsHyweaving with
        | true -> baseClass + " stop-state" 
        | false -> 
            match syntaxAltered with
            | true -> baseClass + " needs-update"
            | false -> baseClass

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

        tab "Boundary" pathBoundary BoundaryPanel
        tab "Layout"   pathLayout   LayoutPanel
        tab "Analyze"  pathAnalyze  AnalyzePanel
        tab "3D"       path3D       ViewPanel
        tab "Batch"    pathBatch    BatchPanel
        tab "Teach"    pathTeach    TeachPanel
        tab "Report"   pathReport   ReportPanel
    }


let private viewHywePanels (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    let baseSqn = model.Sequences |> Map.tryFind 0 |> Option.defaultValue allSqns.[11]
    let currentSqn = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue baseSqn
    let minIdx, maxIdx = 
        match model.Tree.ActiveLevel, baseSqn.StartsWith "V" with
        | 0, _ -> 0, 23
        | _, true -> 0, 11
        | _, false -> 12, 23

    let getFilteredGeometries () =
        let rec getIds (marker: string) (prefix: string) (node: Hywe.Node.TreeNode) =
            seq {
                yield $"{marker}.{prefix}"
                yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getIds marker $"{prefix}.{i + 1}" child)
            }
        let validIds =
            match model.Tree.ActiveNest with
            | Some nestId ->
                match model.Tree.Nests |> Map.tryFind nestId with
                | Some nestNode -> getIds $"N{nestId}" "1" nestNode |> Set.ofSeq
                | None -> Set.empty
            | None ->
                match model.Tree.Levels |> Map.tryFind model.Tree.ActiveLevel with
                | Some levelNode ->
                    let marker = match model.Tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}"
                    getIds marker "1" levelNode |> Set.ofSeq
                | None -> Set.empty
        
        let indexed = 
            model.Derived.cxCxl1 
            |> Array.indexed 
            |> Array.filter (fun (_, c) -> validIds.Contains(Hywe.Core.Coxel.prpVlu c.Rfid))
            
        let cxls = indexed |> Array.map (fun (i, _) -> model.Derived.cxCxl1.[i])
        let clrs = indexed |> Array.map (fun (i, _) -> model.Derived.cxClr1.[i])
        let avls = indexed |> Array.map (fun (i, _) -> model.Derived.cxlAvl.[i])
        
        let bgCxl = 
            match model.Tree.ActiveNest with
            | Some nestId ->
                match model.Tree.Nests |> Map.tryFind nestId with
                | Some nestNode ->
                    let isParentCxl (rfid: string) =
                        match nestNode.Base with
                        | Some targetId -> rfid = targetId || rfid.EndsWith("." + targetId)
                        | None -> false
                    model.Derived.cxCxl1 |> Array.tryFind (fun c -> isParentCxl (Hywe.Core.Coxel.prpVlu c.Rfid))
                | None -> None
            | None -> None
            
        let wtmkCxls = 
            match model.Tree.ActiveNest with
            | Some _ ->
                match model.Tree.Levels |> Map.tryFind model.Tree.ActiveLevel with
                | Some levelNode ->
                    let marker = match model.Tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}"
                    let levelIds = getIds marker "1" levelNode |> Set.ofSeq
                    let bgCxlId = bgCxl |> Option.map (fun c -> Hywe.Core.Coxel.prpVlu c.Rfid)
                    model.Derived.cxCxl1 
                    |> Array.filter (fun c -> 
                        let id = Hywe.Core.Coxel.prpVlu c.Rfid
                        levelIds.Contains(id) && (Some id <> bgCxlId))
                | None -> [||]
            | None -> [||]

        cxls, clrs, avls, bgCxl, wtmkCxls

    div {
        attr.style "padding: 10px; min-height: 400px;"
        
        let currentInner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
        div { 
            attr.id "hywe-polygon-editor"
            attr.style (match model.ActivePanel = BoundaryPanel with true -> "display: block;" | false -> "display: none;")
            View.view currentInner (PolygonEditorMsg >> dispatch) js 
        }

        match model.ActivePanel with
        | BoundaryPanel -> empty()

        | LayoutPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }
                
                let filteredCxls, filteredClrs, _, bgCxl, wtmkCxls = getFilteredGeometries ()
                
                let bdrToPass = 
                    match bgCxl with
                    | Some bg -> 
                        let (_, _, z) = Hywe.Core.Hexel.hxlCrd bg.Base
                        [| Hywe.Core.Coxel.cxlPrm bg z |> Hywe.Core.Goxel.cleanPolygon bg.Seqn |]
                    | None -> model.Derived.cxOuIl
                
                div {
                    attr.id "hywe-svg-wrapper"; attr.style "width: 100%;"
                    svgCoxels filteredCxls bdrToPass wtmkCxls model.Tree.ActiveLevel filteredClrs 20 (Some "layout-svg-output")
                }
                div {
                    attr.style "display: flex; gap: 10px; margin-top: 10px; justify-content: center;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light layout-download-btn"
                        on.pointerdown (fun _ ->
                            let datePart = System.DateTime.Now.ToString("yyMMddmm")
                            let fileName = "HyweLayout_" + datePart + ".svg"
                            async {
                                let elv = model.Tree.ActiveLevel
                                let currentSqnIdx = sqnToIndex currentSqn
                                let toMarker lvl = match lvl with 0 -> "L0" | _ -> sprintf "L%d" lvl
                                match Cache.get (toMarker elv) currentSqnIdx model.LayoutCache with
                                | Some cfg ->
                                    let svgString = Layout.generateSvgFromBatchConfig cfg 20.0
                                    do! js.InvokeVoidAsync("downloadFile", fileName, svgString, "image/svg+xml;charset=utf-8").AsTask() |> Async.AwaitTask
                                | None ->
                                    // Fallback to DOM scraper if cache is missing
                                    do! downloadSvg js "layout-svg-output" fileName
                            } |> Async.StartImmediate
                        )
                        text "SVG"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light layout-download-btn"
                        on.pointerdown (fun _ ->
                            let datePart = System.DateTime.Now.ToString("yyMMddmm")
                            let fileName = "HyweLayout_" + datePart + ".png"
                            async {
                                let elv = model.Tree.ActiveLevel
                                let currentSqnIdx = sqnToIndex currentSqn
                                let toMarker lvl = match lvl with 0 -> "L0" | _ -> sprintf "L%d" lvl
                                match Cache.get (toMarker elv) currentSqnIdx model.LayoutCache with
                                | Some cfg ->
                                    let svgString = Layout.generateSvgFromBatchConfig cfg 20.0
                                    do! js.InvokeVoidAsync("downloadSvgAsPng", fileName, svgString).AsTask() |> Async.AwaitTask
                                | None ->
                                    // If cache missing, fall back to SVG scraper and pass to PNG converter
                                    do! js.InvokeVoidAsync("downloadSvgElementAsPng", "layout-svg-output", fileName).AsTask() |> Async.AwaitTask 
                            } |> Async.StartImmediate
                        )
                        text "PNG"
                    }

                }
            }
        
        | AnalyzePanel ->
            let elv = model.Tree.ActiveLevel
            let currentSqnIdx = sqnToIndex currentSqn
            let toMarker lvl = match lvl with 0 -> "L0" | _ -> sprintf "L%d" lvl
            
            let fCxls, fClrs, fAvls, fAdj, fSol = 
                match Cache.get (toMarker elv) currentSqnIdx model.LayoutCache with
                | Some cfg -> 
                    let filtered = Page.TreeFiltering.filterBatchConfig true model.Tree cfg
                    filtered.cxCxl1, filtered.cxClr1, filtered.cxlAvl, filtered.cxAdj1, filtered.cxSol1
                | None ->
                    // Fallback to on-the-fly filtering if not yet cached
                    let fCxls, fClrs, fAvls, _, _ = getFilteredGeometries ()
                    fCxls, fClrs, fAvls, Coxel.cxlAdj fCxls, model.Derived.cxSol1

            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }
                
                let editor = match model.PolygonEditor with Stable m | FreshlyImported m -> m
                
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    Analyze.viewHyweAnalyze dispatch currentSqn fCxls fClrs fAvls fSol fAdj (model.Derived.cxRto1 |> Array.tryItem elv |> Option.defaultValue 1.0) elv model.IsCoordsVisible editor.UseMapBase
                }
            }

        | ViewPanel ->
            let idMap = Page.TreeFiltering.getHierarchicalIdMap model.Tree
            let hostIds = 
                model.Tree.NestAnchors 
                |> Map.toSeq 
                |> Seq.choose (fun (_, guid) -> Map.tryFind guid idMap) 
                |> Set.ofSeq

            let viewCxls, viewClrs =
                match hostIds.IsEmpty with
                | true ->
                    model.Derived.cxCxl1, model.Derived.cxClr1
                | false ->
                    let validIndices = 
                        model.Derived.cxCxl1 
                        |> Array.indexed 
                        |> Array.filter (fun (_, c) -> not (hostIds.Contains(Hywe.Core.Coxel.prpVlu c.Rfid)))
                        |> Array.map fst
                    
                    validIndices |> Array.map (fun i -> model.Derived.cxCxl1.[i]),
                    validIndices |> Array.map (fun i -> model.Derived.cxClr1.[i])

            let sideEffect = async { do! ThreeD.extrudePolygons js "hywe-extruded-polygon" viewCxls viewClrs model.Derived.cxElv1 model.ViewLocked } |> Async.StartImmediate
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 8px; width: 100%; overflow-x: hidden;"
                
                div {
                    attr.id "hywe-sequence-selector"; attr.style "padding: 4px 0; width: 100%; max-width: 100vw; margin-top: 5px;"
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }                

                div {
                    attr.style "width: 95%; max-width: 100%; aspect-ratio: 3/2; max-height: 70vh; position: relative; overflow: hidden; background: #f9f9f9; border-radius: 8px;"
                    
                    // Floating Lock button
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-circle hywe-btn-flat layout-download-btn" + (match model.ViewLocked with true -> " active" | false -> ""))
                        attr.title (match model.ViewLocked with true -> "View Locked: Captured for cover" | false -> "Lock 3D view for report cover")
                        attr.style "position: absolute; top: 10px; right: 10px; width: 34px; height: 34px; padding: 0; border-radius: 50%; z-index: 10; border: none; background: rgba(255,255,255,0.6); backdrop-filter: blur(4px);"
                        on.pointerdown (fun _ -> dispatch ToggleViewLock)
                        match model.ViewLocked with
                        | true -> drawMenuIcon pathLock
                        | false -> drawMenuIcon pathUnlock
                    }

                    canvas { 
                        attr.id "hywe-extruded-polygon"
                        attr.style "width: 100%; height: 100%; display: block; touch-action: none;" 
                    }
                }

                // Export buttons
                div {
                    attr.style "display: flex; gap: 8px; margin-top: 10px; align-items: center;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-ghost layout-download-btn"
                        attr.title "Download Layout as SVG"
                        on.pointerdown (fun _ -> dispatch Download3DSvg)
                        text "SVG"
                    }

                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-ghost layout-download-btn"
                        attr.title "Download View as PNG"
                        on.pointerdown (fun _ -> dispatch Download3DPng)
                        text "PNG"
                    }
                }

              
                div {
                    attr.style "display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; padding: 15px 10px; width: 100%; border-top: 1px solid #f0f0f0; margin-top: 5px;"
                    forEach [0 .. (min model.Derived.cxCxl1.Length model.Derived.cxClr1.Length - 1)] (fun i ->
                        let name = Coxel.prpVlu model.Derived.cxCxl1.[i].Name
                        let color = model.Derived.cxClr1.[i]
                        div {
                            attr.style "display: flex; align-items: center; gap: 6px; font-size: 11px; font-family: 'Outfit', system-ui, sans-serif; color: #666;"
                            div {
                                attr.style (sprintf "width: 12px; height: 12px; border-radius: 2px; background: %s;" color)
                            }
                            text name
                        }
                    )
                }
            }

        | BatchPanel ->
            div {
                attr.style "width: 100vw; margin-left: calc(-50vw + 50%); min-height: 500px; display: flex; flex-direction: column; align-items: center; background: #ffffff;"
                let toMarker lvl = match lvl with 0 -> "L0" | _ -> sprintf "L%d" lvl
                let rawResults = Cache.getAllVariations (toMarker model.Tree.ActiveLevel) model.LayoutCache
                match rawResults.Length > 0 && model.BatchProgress = 24 with
                | true ->
                    let results = rawResults |> Array.map (Page.TreeFiltering.filterBatchConfig false model.Tree)
                    alternateConfigurations 
                        results 
                        model.SelectedPreviewIndex 
                        TapBatchPreview                   
                        dispatch                   
                        (fun () -> dispatch (SetActivePanel LayoutPanel)) js
                | false ->
                    div { 
                        attr.style "text-align:center; padding: 40px 20px; color: #888; width: 100%; display: flex; flex-direction: column; align-items: center;"
                        
                        // Text Above - Multi-line, width constrained to match 4x6 grid (156px)
                        div {
                            attr.style "font-family: 'Outfit', system-ui, sans-serif; font-size: 1.1em; letter-spacing: 0.5px; color: #666; width: 156px; margin-bottom: 15px; text-align: center; font-weight: 500; line-height: 1.3;"
                            text "Generating Configurations"
                        }

                        span { attr.``class`` "spinner"; attr.style "display: block; margin-bottom: 25px;" }
                        
                        // Progress Grid (4x6) - Delicate filleted squares
                        div {
                            // 4 columns * 14px + 3 gaps * 14px = 56 + 42 = 98px
                            attr.style "display: grid; grid-template-columns: repeat(4, 14px); grid-template-rows: repeat(6, 14px); gap: 14px; margin: 0 auto; justify-content: center; width: 98px;"
                            forEach [0 .. 23] (fun i ->
                                let isComplete = i < model.BatchProgress
                                div {
                                    attr.style (sprintf "width: 14px; height: 14px; border: 1px solid #e0e0e0; border-radius: 3px; background: %s; transition: all 0.5s ease;" 
                                        (match isComplete with true -> "rgba(136, 136, 136, 0.4)" | false -> "transparent"))
                                }
                            )
                        }
                    }
            }

        | TeachPanel ->
            Teach.view model dispatch
            
        | ReportPanel ->
            Hywe.Report.viewReport model dispatch
    }


let viewGalleryModal (model: Model) (dispatch: Message -> unit) =
    if not model.ShowGallery then empty()
    else
        div {
            attr.style "position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 10000; display: flex; align-items: center; justify-content: center;"
            
            // Sibling 1: Backdrop
            div {
                attr.style "position: absolute; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.8);"
                on.click (fun _ -> dispatch ToggleGallery)
            }
            
            // Sibling 2: Modal Content
            div {
                attr.style "position: relative; background: #fff; width: 90%; max-width: 1100px; max-height: 80vh; border-radius: 8px; display: flex; flex-direction: column; overflow: hidden; box-shadow: 0 10px 30px rgba(0,0,0,0.5);"
                
                div {
                    attr.style "padding: 15px 20px; border-bottom: 1px solid #eee; display: flex; justify-content: space-between; align-items: center; background: #fafafa;"
                    h2 { 
                        attr.style "margin: 0; font-size: 1.2rem; color: #333; font-weight: 600;"
                        text "Community Gallery" 
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-ghost"
                        attr.style "font-size: 1.2rem; padding: 0 5px; line-height: 1;"
                        on.click (fun _ -> dispatch ToggleGallery)
                        text "×"
                    }
                }
                
                div {
                    attr.style "flex: 1; overflow-y: auto; padding: 15px 20px; display: flex; flex-direction: column; gap: 10px; background: #fdfdfd;"
                    
                    if model.IsLoadingGallery then
                        div {
                            attr.style "text-align: center; padding: 30px; color: #777; font-style: italic;"
                            text "Loading latest configurations from Hugging Face..."
                        }
                    else
                        match model.GalleryEntries with
                        | None | Some [] -> 
                            div {
                                attr.style "text-align: center; padding: 30px; color: #777;"
                                text "Gallery is syncing... Please check back in a few minutes."
                            }
                        | Some entries ->
                            let filterText = if System.String.IsNullOrWhiteSpace(model.GalleryFilter) then "" else model.GalleryFilter.ToLower()
                            let filteredEntries = 
                                if filterText = "" then entries
                                else entries |> List.filter (fun e -> 
                                    (not (System.String.IsNullOrWhiteSpace e.ExplorationDescription) && e.ExplorationDescription.ToLower().Contains(filterText)) || 
                                    (not (System.String.IsNullOrWhiteSpace e.Author) && e.Author.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Description) && e.Description.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Typology) && e.Typology.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Flow) && e.Flow.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Stage) && e.Stage.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Scale) && e.Scale.ToLower().Contains(filterText)))
                                    
                            let pagedEntries = 
                                filteredEntries 
                                |> List.skip (min model.GalleryOffset (max 0 (filteredEntries.Length - 1))) 
                                |> List.truncate 8

                            div {
                                attr.style "width: 100%; display: flex; flex-direction: column;"
                                
                                input {
                                    attr.``class`` "hywe-input"
                                    attr.style "margin-bottom: 12px; width: 100%; padding: 8px 12px; border-radius: 4px; border: 1px solid #ddd;"
                                    "onclick:stopPropagation" => true
                                    "onpointerdown:stopPropagation" => true
                                    attr.placeholder "Search by exploration, author, typology, flow..."
                                    attr.value model.GalleryFilter
                                    on.input (fun e -> dispatch (UpdateGalleryFilter (unbox<string> e.Value)))
                                }
                                
                                if pagedEntries.IsEmpty then
                                    div {
                                        attr.style "text-align: center; padding: 30px; color: #777;"
                                        text "No configurations match your search."
                                    }
                                else
                                    div {
                                        attr.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(min(100%, 340px), 1fr)); gap: 10px; margin-bottom: 8px;"
                                        for entry in pagedEntries do
                                            div {
                                                attr.style "display: flex; align-items: stretch; border-radius: 8px; border: 1px solid #e9ecef; background: #ffffff; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: hidden; transition: border-color 0.15s ease;"
                                                
                                                // Content Container (Thumbnail + Information)
                                                div {
                                                    attr.style "flex: 1; display: flex; gap: 10px; align-items: center; padding: 8px 10px; min-width: 0;"

                                                    // Left: 60x60 SVG Thumbnail
                                                    div {
                                                        attr.style "width: 60px; height: 60px; min-width: 60px; border-radius: 6px; overflow: hidden; background: #f8f9fa; border: 1px solid #dee2e6; display: flex; align-items: center; justify-content: center; padding: 2px;"
                                                        if not (String.IsNullOrWhiteSpace entry.SvgThumbnail) then
                                                            rawHtml entry.SvgThumbnail
                                                        else
                                                            rawHtml """<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#adb5bd" stroke-width="1.5"><rect x="3" y="3" width="18" height="18" rx="2"/><path d="M3 9h18M9 21V9"/></svg>"""
                                                    }

                                                    // Middle: Content Column
                                                    div {
                                                        attr.style "flex: 1; display: flex; flex-direction: column; gap: 4px; overflow: hidden; min-width: 0;"
                                                        
                                                        // Exploration Description (Title)
                                                        div {
                                                            attr.style "font-weight: 600; color: #1a1a1a; font-size: 0.92rem; line-height: 1.25; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
                                                            attr.title entry.ExplorationDescription
                                                            text (if String.IsNullOrWhiteSpace entry.ExplorationDescription then "Untitled Exploration" else entry.ExplorationDescription)
                                                        }

                                                        // Author and Badges row
                                                        div {
                                                            attr.style "display: flex; align-items: center; flex-wrap: wrap; gap: 4px; font-size: 0.75rem;"
                                                            let dateSuffix =
                                                                if String.IsNullOrWhiteSpace entry.CreatedAt then ""
                                                                else
                                                                    match DateTime.TryParse entry.CreatedAt with
                                                                    | true, dt ->
                                                                        let span = DateTime.UtcNow - dt.ToUniversalTime()
                                                                        if span.TotalMinutes < 1.0 then " • just now"
                                                                        elif span.TotalHours < 1.0 then sprintf " • %dm ago" (int span.TotalMinutes)
                                                                        elif span.TotalDays < 1.0 then sprintf " • %dh ago" (int span.TotalHours)
                                                                        elif span.TotalDays < 30.0 then sprintf " • %dd ago" (int span.TotalDays)
                                                                        else sprintf " • %s" (dt.ToString("MMM d"))
                                                                    | false, _ -> ""
                                                            span {
                                                                attr.style "color: #6c757d; white-space: nowrap; margin-right: 2px;"
                                                                text (sprintf "by %s%s" (if String.IsNullOrWhiteSpace entry.Author then "Anonymous" else entry.Author) dateSuffix)
                                                            }
                                                            if entry.LevelsCount > 0 then
                                                                span { attr.style "background: #f1f3f5; color: #495057; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text (sprintf "%d %s" entry.LevelsCount (if entry.LevelsCount = 1 then "Level" else "Levels")) }
                                                            if entry.SpacesCount > 0 then
                                                                span { attr.style "background: #f1f3f5; color: #495057; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text (sprintf "%d %s" entry.SpacesCount (if entry.SpacesCount = 1 then "Node" else "Nodes")) }
                                                            if not (String.IsNullOrWhiteSpace entry.Typology) && entry.Typology <> "N/A" then
                                                                span { attr.style "background: #e7f1ff; color: #0d6efd; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text entry.Typology }
                                                            if not (String.IsNullOrWhiteSpace entry.Flow) && entry.Flow <> "N/A" then
                                                                span { attr.style "background: #f1f3f5; color: #495057; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text entry.Flow }
                                                        }
                                                    }
                                                }

                                                // Right: Vertical Full-Height Load Button (16px width matching Waggle & About handles)
                                                button {
                                                    attr.``class`` "hywe-btn hywe-btn-dark"
                                                    attr.style "align-self: stretch; width: 16px; min-width: 16px; border: none; border-left: 1px solid #dee2e6; border-radius: 0; display: flex; align-items: center; justify-content: center; padding: 0; cursor: pointer; transition: background 0.15s ease; box-sizing: border-box;"
                                                    attr.title "Load configuration into workspace"
                                                    "aria-label" => sprintf "Load %s" (if String.IsNullOrWhiteSpace entry.ExplorationDescription then "configuration" else entry.ExplorationDescription)
                                                    on.click (fun _ -> dispatch (ToggleConfirm (Some (ConfirmAction.LoadGallery (entry.ExplorationDescription, entry.Id)))))
                                                    span {
                                                        attr.style "writing-mode: vertical-rl; transform: rotate(180deg); font-size: 8px; font-weight: 600; letter-spacing: 1.2px; text-transform: uppercase;"
                                                        text "LOAD"
                                                    }
                                                }
                                            }
                                    }
                            }
                            
                            // Pagination Footer
                            div {
                                attr.style "display: flex; justify-content: space-between; align-items: center; padding-top: 15px; margin-top: auto;"
                                button {
                                    attr.``class`` "hywe-btn hywe-btn-sm"
                                    if model.GalleryOffset = 0 then
                                        attr.disabled true
                                        attr.style "opacity: 0.5; cursor: not-allowed; background: #eee; color: #aaa;"
                                    else
                                        attr.style "background: #eee; color: #333;"
                                        on.click (fun _ -> dispatch PrevGalleryPage)
                                    text "Previous"
                                }
                                span {
                                    attr.style "font-size: 0.85rem; color: #777;"
                                    let currentStart = if filteredEntries.IsEmpty then 0 else model.GalleryOffset + 1
                                    text (sprintf "Showing %d - %d of %d" currentStart (min filteredEntries.Length (model.GalleryOffset + pagedEntries.Length)) filteredEntries.Length)
                                }
                                button {
                                    attr.``class`` "hywe-btn hywe-btn-sm"
                                    if model.GalleryOffset + 8 >= filteredEntries.Length then
                                        attr.disabled true
                                        attr.style "opacity: 0.5; cursor: not-allowed; background: #eee; color: #aaa;"
                                    else
                                        attr.style "background: #eee; color: #333;"
                                        on.click (fun _ -> dispatch NextGalleryPage)
                                    text "Next"
                                }
                            }
                }
            }
        }

let view model dispatch (js: IJSRuntime) =
    concat {
        viewNodeCodeButtons model dispatch js
        viewEditorPanel model dispatch
        viewHyweButton model dispatch
        viewHyweTabs model dispatch 
        viewHywePanels model dispatch js
        viewConfirmOverlay model dispatch
    }
