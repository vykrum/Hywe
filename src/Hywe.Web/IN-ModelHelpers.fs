module ModelHelpers

open System
open Microsoft.JSInterop
open Elmish
open Layout
open Hywe
open Page
open NodeCode
open PolygonEditor
open ModelTypes
open Bolero.Html
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Paxel

open PageHelpers

let viewConfirmOverlay (model: Model) (dispatch: Message -> unit) =
    match model.PendingConfirm with
    | None -> empty()
    | Some action ->
        let title, msg, confirmMsg, onConfirm =
            match action with
            | ConfirmAction.ResetWorkspace ->
                "Reset Workspace", "Reset entire workspace? All unsaved changes will be lost.", "Reset", HardReset
            | ConfirmAction.LoadPreset (name, label) ->
                "Load Preset", (sprintf "Load %s preset? Current layout will be replaced." label), "Load", SelectPreset name

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
let private viewNodeCodeButtons (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    concat {
        let nodeCodeButtonText =
            match model.EditorMode with
            | Syntax -> "Node"
            | Interactive -> "Code"

        div {
            attr.style "display:flex; width: 100%; gap:0px; padding: 0 4px; justify-content: flex-start; align-items: center; position: relative; z-index: 3000; pointer-events: none;"
            
            div {
                attr.style "display:flex; gap:0px; align-items: center;"
                
                div {
                    attr.``class`` "node-code-toolbar"
                    attr.style "display:flex; gap:0px; align-items: center; pointer-events: auto;"
                    
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                        attr.style "padding: 3px;"
                        attr.title (match model.EditorMode with | Syntax -> "Switch to Node Editor" | Interactive -> "Switch to Code Editor")
                        on.click (fun _ -> dispatch ToggleEditorMode)
                        match model.EditorMode with
                        | Syntax -> rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M 12 4 L 20 8 L 20 16 L 12 20 L 4 16 L 4 8 Z"></path></svg>"""
                        | Interactive -> rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="16 18 22 12 16 6"></polyline><polyline points="8 6 2 12 8 18"></polyline></svg>"""
                    }

                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                        attr.style "padding: 3px;"
                        attr.title "Save"
                        on.click (fun _ -> dispatch SaveRequested)
                        rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z"></path><polyline points="17 21 17 13 7 13 7 21"></polyline><polyline points="7 3 7 8 15 8"></polyline></svg>"""
                    }

                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                        attr.style "padding: 3px;"
                        attr.title "Load"
                        on.click (fun _ -> dispatch ImportRequested)
                        rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path><polyline points="17 8 12 3 7 8"></polyline><line x1="12" y1="3" x2="12" y2="15"></line></svg>"""
                    }

                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                        attr.style "padding: 3px; color: #E67E22;"
                        attr.title "Hard Reset"
                        on.pointerdown (fun _ -> dispatch (ToggleConfirm (Some ConfirmAction.ResetWorkspace)))
                        rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"></path><path d="M3 3v5h5"></path></svg>"""
                    }
                    
                    let canUndo = model.UndoStack <> []
                    let canRedo = model.RedoStack <> []

                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                        attr.style (sprintf "padding: 3px; opacity: %s;" (if canUndo then "1" else "0.3"))
                        attr.title "Undo (Ctrl+Z)"
                        on.click (fun _ -> dispatch (if canUndo then Undo else NoOp))
                        rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 7v6h6"></path><path d="M3 13C5 7 11 4 17 6a9 9 0 0 1 4 13"></path></svg>"""
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                        attr.style (sprintf "padding: 3px; opacity: %s;" (if canRedo then "1" else "0.3"))
                        attr.title "Redo (Ctrl+Y)"
                        on.click (fun _ -> dispatch (if canRedo then Redo else NoOp))
                        rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 7v6h-6"></path><path d="M21 13C19 7 13 4 7 6a9 9 0 0 0-4 13"></path></svg>"""
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
            }
            if model.InstallPromptAvailable then
                div {
                    attr.style "margin-left: auto; margin-right: 10px; pointer-events: auto; display: flex; align-items: center;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet"
                        attr.title "Install as an App"
                        attr.style "display: flex; align-items: center; gap: 2px; padding: 1px 4px; border: 1px solid rgba(0,0,0,0.1); background: transparent; box-shadow: none; opacity: 0.6; transition: opacity 0.2s ease;"
                        on.click (fun _ -> 
                            dispatch InstallRequested
                        )
                        
                        rawHtml """<svg width="9" height="9" viewBox="0 0 24 24" fill="none" stroke="#666" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path><polyline points="7 10 12 15 17 10"></polyline><line x1="12" y1="15" x2="12" y2="3"></line></svg>"""
                        span { attr.style "font-size: 8.5px; font-weight: 400; color: #666; letter-spacing: -0.3px;"; text "Install" }
                    }
                }
        }
        if model.EditorMode = Interactive then
            let isCollapsed = model.IsPresetsCollapsed
            concat {
                div {
                    attr.style (if isCollapsed then "display: none;" else "position: fixed; inset: 0; z-index: 1800; background: transparent; pointer-events: auto;")
                    on.click (fun _ -> dispatch TogglePresetsCollapse)
                }

                div {
                    attr.``class`` (if isCollapsed then "preset-drawer collapsed" else "preset-drawer")
                    
                    div {
                        attr.``class`` "preset-drawer-content"
                        
                        let presetButton name label isSelected =
                            button {
                                attr.``class`` ("hywe-btn hywe-btn-sm hywe-btn-fillet " + (if isSelected then "hywe-btn-dark active" else "hywe-btn-light"))
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
        else
            empty()
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
            let colorMap = 
                if model.Derived.cxCxl1.Length > 0 then
                    Array.zip model.Derived.cxCxl1 model.Derived.cxClr1
                    |> Array.collect (fun (c, clr) -> 
                        let name = Coxel.prpVlu c.Name
                        [| name.Trim(), clr; name.Trim().ToLower(), clr |])
                    |> Map.ofArray
                else Map.empty
            viewTreeEditor model.Tree colorMap (TreeMsg >> dispatch)
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

        tab "Boundary" iconBoundary BoundaryPanel
        tab "Layout"   iconLayout   LayoutPanel
        tab "Analyze"  iconAnalyze  AnalyzePanel
        tab "3D"       icon3D       ViewPanel
        tab "Batch"    iconBatch    BatchPanel
        tab "Teach"    iconTeach    TeachPanel
        tab "Report"   iconReport   ReportPanel
    }


let private viewHywePanels (model: Model) (dispatch: Message -> unit) (js: IJSRuntime) =
    let baseSqn = model.Sequences |> Map.tryFind 0 |> Option.defaultValue allSqns.[11]
    let currentSqn = model.Sequences |> Map.tryFind model.Tree.ActiveLevel |> Option.defaultValue baseSqn
    let minIdx, maxIdx = 
        if model.Tree.ActiveLevel = 0 then 0, 23
        else if baseSqn.StartsWith "V" then 0, 11
        else 12, 23
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
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-svg-container"
                    svgCoxels model.Derived.cxCxl1 model.Derived.cxOuIl model.Tree.ActiveLevel model.Derived.cxClr1 20 (Some "layout-svg-output")
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
            let fAdj  = cxlAdj fCxls

            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider currentSqn minIdx maxIdx (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    Analyze.viewHyweAnalyze dispatch currentSqn fCxls fClrs fAvls fAdj (model.Derived.cxRto1 |> Array.tryItem elv |> Option.defaultValue 1.0) elv
                }
            }

        | ViewPanel ->
            let sideEffect = async { do! ThreeD.extrudePolygons js "hywe-extruded-polygon" model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxElv1 model.ViewLocked } |> Async.StartImmediate
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
                            rawHtml """<svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="#555" stroke-width="2.5" style="display:block;"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 10 0v4"></path></svg>"""
                        else
                            rawHtml """<svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="#888" stroke-width="2.5" style="display:block;"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 9.9-1"></path></svg>"""
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
