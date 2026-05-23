module ModelHelpers

open System
open Microsoft.JSInterop
open Elmish
open Layout
open Hywe
open Page
open Graphics
open Hywe.Node
open Hywe.Site
open Hywe.Site.State
open Hywe.Site.View
open ModelTypes
open Bolero.Html
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Lexel

let viewConfirmOverlay (model: Model) (dispatch: Message -> unit) =
    match model.PendingConfirm with
    | None -> empty()
    | Some action ->
        let title, msg, confirmMsg, onConfirm =
            match action with
            | ConfirmAction.ResetWorkspace ->
                "Reset Layout", "Current Layout will be replaced.", "Reset", HardReset
            | ConfirmAction.LoadPreset (name, label) ->
                "Load Preset", (sprintf "Load %s preset? Current layout will be replaced." label), "Load", SelectPreset name
            | ConfirmAction.SwitchTo tab ->
                "Switch View", "Switch to this view?", "Switch", SetActivePanel (match tab with Boundary -> BoundaryPanel | _ -> LayoutPanel)

        div {
            attr.style "position: fixed; inset: 0; background: rgba(255,255,255,0.7); backdrop-filter: blur(4px); z-index: 10000; display: flex; align-items: center; justify-content: center; animation: fadeIn 0.3s ease;"
            on.pointerdown (fun _ -> dispatch (ToggleConfirm None))
            
            div {
                attr.style "background: #fff; border: 1px solid #eee; padding: 24px; border-radius: 8px; width: 300px; box-shadow: 0 10px 30px rgba(0,0,0,0.05); display: flex; flex-direction: column; gap: 16px; text-align: center; pointer-events: auto;"
                "onclick:stopPropagation" => true
                
                div {
                    attr.style "font-weight: 600; font-size: 1.1rem; color: #333;"
                    text title
                }
                div {
                    attr.style "font-size: 0.9rem; color: #666; line-height: 1.4;"
                    text msg
                }
                div {
                    attr.style "display: flex; gap: 10px; margin-top: 8px;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-dark"
                        attr.style "flex: 1;"
                        on.pointerdown (fun _ -> dispatch onConfirm)
                        text confirmMsg
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                        attr.style "flex: 1;"
                        on.pointerdown (fun _ -> dispatch (ToggleConfirm None))
                        text "Cancel"
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
            attr.style ("padding: 4px 2px; display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 44px; gap: 2px; " + style)
            attr.title title
            on.click (fun _ -> dispatch m)
            content
        }
    | None ->
        button {
            attr.``class`` btnCls
            attr.style ("padding: 4px 2px; display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 44px; gap: 2px; opacity: 0.3; pointer-events: none; " + style)
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
                    (if canUndo then Some Undo else None) 
                    iconUndo 
                    dispatch 
                    "" (sprintf "opacity: %s;" (if canUndo then "1" else "0.3"))

                toolbarBtn 
                    "Redo (Ctrl+Y)" 
                    (if canRedo then Some Redo else None) 
                    iconRedo 
                    dispatch 
                    "" (sprintf "opacity: %s;" (if canRedo then "1" else "0.3"))
            }

            if model.InstallPromptAvailable then
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
                attr.style (if isWorkspaceCollapsed then "display: none;" else "position: fixed; inset: 0; z-index: 1800; background: transparent; pointer-events: auto;")
                on.click (fun _ -> dispatch ToggleWorkspaceCollapse)
            }

            div {
                attr.``class`` (if isWorkspaceCollapsed then "preset-drawer collapsed" else "preset-drawer")
                attr.style "top: 65px;"
                
                div {
                    attr.``class`` "preset-drawer-content"
                    attr.style "min-width: 100px;"
                    
                    div {
                        attr.style "display: grid; grid-template-columns: repeat(2, 1fr); gap: 6px;"
                        
                        // Save
                        drawerActionBtn "Save" "Save" (Some SaveRequested) iconSave dispatch ""

                        // Load
                        drawerActionBtn "Load" "Load" (Some ImportRequested) iconLoad dispatch ""

                        // Share
                        drawerActionBtn 
                            (if model.ShowLinkCopied then "Link Shared!" else "Share Link")
                            (if model.ShowLinkCopied then "Copied" else "Share")
                            (Some ShareLink)
                            (iconShare model)
                            dispatch
                            (sprintf "color: %s;" (if model.ShowLinkCopied then "#27ae60" else "#555"))

                        // Reset
                        drawerActionBtn 
                            "Hard Reset" 
                            "Reset"
                            (Some (ToggleConfirm (Some ConfirmAction.ResetWorkspace))) 
                            iconReset 
                            dispatch 
                            ""
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
                    span { text "Exchange" }
                }
            }
        }

        // 3. Presets Drawer (Always available, below Exchange)
        let isPresetsCollapsed = model.IsPresetsCollapsed
        concat {
            div {
                attr.style (if isPresetsCollapsed then "display: none;" else "position: fixed; inset: 0; z-index: 1800; background: transparent; pointer-events: auto;")
                on.click (fun _ -> dispatch TogglePresetsCollapse)
            }

            div {
                attr.``class`` (if isPresetsCollapsed then "preset-drawer collapsed" else "preset-drawer")
                attr.style "top: 180px;"
                
                div {
                    attr.``class`` "preset-drawer-content"
                    attr.style "min-width: 100px;"
                    
                    let presetButton name label isSelected =
                        button {
                            attr.``class`` ("hywe-btn hywe-btn-sm hywe-btn-fillet " + (if isSelected then "hywe-btn-dark active" else "hywe-btn-light"))
                            attr.style "width: 100%; text-align: center; justify-content: center;"
                            on.pointerdown (fun _ -> dispatch (ToggleConfirm (Some (ConfirmAction.LoadPreset (name, label)))))
                            text label
                        }

                    presetButton "Simple" "Simple" (model.SelectedPreset = Some "Simple")
                    presetButton "Branched" "Branch" (model.SelectedPreset = Some "Branched")
                    presetButton "Stacked" "Stack" (model.SelectedPreset = Some "Stacked")
                }

                div {
                    attr.``class`` "preset-drawer-handle"
                    on.click (fun _ -> dispatch TogglePresetsCollapse)
                    span { text "Presets" }
                }
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
        if model.Tree.ActiveLevel = 0 then 0, 23
        else if baseSqn.StartsWith "V" then 0, 11
        else 12, 23

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
            
        cxls, clrs, avls, bgCxl

    div {
        attr.style "padding: 10px; min-height: 400px;"
        
        match model.ActivePanel with
        | BoundaryPanel ->
            let currentInner = match model.PolygonEditor with Stable m | FreshlyImported m -> m
            div { 
                attr.id "hywe-polygon-editor"
                View.view currentInner (PolygonEditorMsg >> dispatch) js 
            }

        | LayoutPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }
                
                let filteredCxls, filteredClrs, _, bgCxl = getFilteredGeometries ()
                
                let bdrToPass = 
                    match bgCxl with
                    | Some bg -> 
                        let (_, _, z) = Hywe.Core.Hexel.hxlCrd bg.Base
                        [| Hywe.Core.Coxel.cxlPrm bg z |> Hywe.Core.Goxel.cleanPolygon bg.Seqn |]
                    | None -> model.Derived.cxOuIl
                
                div {
                    attr.id "hywe-svg-wrapper"; attr.style "width: 100%;"
                    svgCoxels filteredCxls bdrToPass model.Tree.ActiveLevel filteredClrs 20 (Some "layout-svg-output")
                }
                div {
                    attr.style "display: flex; gap: 10px; margin-top: 10px; justify-content: center;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light layout-download-btn"
                        on.pointerdown (fun _ ->
                            let datePart = System.DateTime.Now.ToString("yyMMddmm")
                            let fileName = "HyweLayout_" + datePart + ".svg"
                            async {
                                do! downloadSvg js "layout-svg-output" fileName
                            } |> Async.StartImmediate
                        )
                        text "SVG"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light layout-download-btn"
                        on.pointerdown (fun _ -> dispatch DownloadDxf)
                        text "DXF"
                    }
                }
            }
        
        | AnalyzePanel ->
            let elv = model.Tree.ActiveLevel
            let currentSqnIdx = sqnToIndex currentSqn
            let toMarker lvl = if lvl = 0 then "L0" else sprintf "L%d" lvl
            
            let fCxls, fClrs, fAvls, fAdj = 
                match Cache.get (toMarker elv) currentSqnIdx model.LayoutCache with
                | Some cfg -> 
                    let filtered = Page.TreeFiltering.filterBatchConfig true model.Tree cfg
                    filtered.cxCxl1, filtered.cxClr1, filtered.cxlAvl, filtered.cxAdj1
                | None ->
                    // Fallback to on-the-fly filtering if not yet cached
                    let fCxls, fClrs, fAvls, _ = getFilteredGeometries ()
                    fCxls, fClrs, fAvls, cxlAdj fCxls

            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    Analyze.viewHyweAnalyze dispatch currentSqn fCxls fClrs fAvls fAdj (model.Derived.cxRto1 |> Array.tryItem elv |> Option.defaultValue 1.0) elv model.IsCoordsVisible
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
                if hostIds.IsEmpty then
                    model.Derived.cxCxl1, model.Derived.cxClr1
                else
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
                    attr.style "width: 95%; max-width: 100%; aspect-ratio: 3/2; position: relative; overflow: hidden; background: #f9f9f9; border-radius: 8px;"
                    
                    // Floating Lock button
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-circle hywe-btn-flat layout-download-btn" + (if model.ViewLocked then " active" else ""))
                        attr.title (if model.ViewLocked then "View Locked: Captured for cover" else "Lock 3D view for report cover")
                        attr.style "position: absolute; top: 10px; right: 10px; width: 34px; height: 34px; padding: 0; border-radius: 50%; z-index: 10; border: none; background: rgba(255,255,255,0.6); backdrop-filter: blur(4px);"
                        on.pointerdown (fun _ -> dispatch ToggleViewLock)
                        if model.ViewLocked then
                            drawMenuIcon pathLock
                        else
                            drawMenuIcon pathUnlock
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
                        attr.title "Download View as SVG"
                        on.pointerdown (fun _ -> dispatch Download3DSvg)
                        text "SVG"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-ghost layout-download-btn"
                        on.pointerdown (fun _ -> dispatch DownloadDxf)
                        text "DXF"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-ghost layout-download-btn"
                        on.pointerdown (fun _ -> dispatch DownloadObj)
                        text "OBJ"
                    }
                }

              
                div {
                    attr.style "display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; padding: 15px 10px; width: 100%; border-top: 1px solid #f0f0f0; margin-top: 5px;"
                    for i in 0 .. (min model.Derived.cxCxl1.Length model.Derived.cxClr1.Length - 1) do
                        let name = Coxel.prpVlu model.Derived.cxCxl1.[i].Name
                        let color = model.Derived.cxClr1.[i]
                        div {
                            attr.style "display: flex; align-items: center; gap: 6px; font-size: 11px; font-family: 'Outfit', system-ui, sans-serif; color: #666;"
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
                let toMarker lvl = if lvl = 0 then "L0" else sprintf "L%d" lvl
                let rawResults = Cache.getAllVariations (toMarker model.Tree.ActiveLevel) model.LayoutCache
                if rawResults.Length > 0 && model.BatchProgress = 24 then
                    let results = rawResults |> Array.map (Page.TreeFiltering.filterBatchConfig false model.Tree)
                    alternateConfigurations 
                        results 
                        model.SelectedPreviewIndex 
                        TapBatchPreview                   
                        dispatch                   
                        (fun () -> dispatch (SetActivePanel LayoutPanel)) js
                else
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
            concat {
                Teach.view model dispatch
                if model.ShowSuccessMessage then
                    div {
                        attr.style "margin-top: 1rem; text-align: center;"
                        span { 
                            attr.style "color: #27ae60; font-size: 0.9em; font-weight: 600;"
                            text "✓ Spatial intent successfully committed to dataset." 
                        }
                    }
            }
            
        | ReportPanel ->
            Hywe.Report.viewReport model dispatch
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
