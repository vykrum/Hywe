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
open Hywe


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
    let nodeCodeButtonText =
        match model.EditorMode with
        | Syntax -> "Node"
        | Interactive -> "Code"

    div {
        attr.style "display:flex; width: 100%; gap:10px; padding: 0 10px; justify-content: space-between; align-items: center; position: relative;"
        
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
        div {
            attr.``class`` "preset-btn-stack"
            
            let isSimple = model.SelectedPreset = Some "Simple"
            let isBranched = model.SelectedPreset = Some "Branched"
            let isStacked = model.SelectedPreset = Some "Stacked"

            button {
                attr.``class`` (if isSimple then "preset-btn active" else "preset-btn")
                on.click (fun _ -> dispatch (SelectPreset "Simple"))
                text "Simple"
            }
            button {
                attr.``class`` (if isBranched then "preset-btn active" else "preset-btn")
                on.click (fun _ -> dispatch (SelectPreset "Branched"))
                text "Branched"
            }
            button {
                attr.``class`` (if isStacked then "preset-btn active" else "preset-btn")
                on.click (fun _ -> dispatch (SelectPreset "Stacked"))
                text "Stacked"
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
        tab "Analyze"  iconAnalyze  AnalyzePanel
        tab "3D"       icon3D       ViewPanel
        tab "Batch"    iconBatch    BatchPanel
        tab "Teach"    iconTeach    TeachPanel
        tab "Report"   iconReport   ReportPanel
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
                div {
                    attr.style "display: flex; gap: 10px; margin-top: 10px; justify-content: center;"
                    button {
                        attr.``class`` "layout-download-btn"
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
                        attr.``class`` "layout-download-btn"
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

            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                div {
                    attr.id "hywe-sequence-selector"; attr.style "width: 100%;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }
                div {
                    attr.id "hywe-table-wrapper"; attr.style "width: 100%; overflow-x: auto;"
                    Analyze.viewHyweAnalyze dispatch model.Sequence fCxls fClrs fAvls
                }
            }

        | ViewPanel ->
            div {
                attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px; width: 100%; overflow-x: hidden;"
                
                div {
                    attr.id "hywe-sequence-selector"; attr.style "padding: 8px 0;width: 100%; max-width: 100vw; margin-top: 10px;"
                    sequenceSlider model.Sequence (fun i -> SetSqnIndex i |> dispatch)
                }                

                // Lock button below slider, right-aligned
                div {
                    attr.style "width: 95%; display: flex; justify-content: flex-end; margin-top: -10px; margin-bottom: 5px;"
                    button {
                        attr.``class`` (if model.ViewLocked then "layout-download-btn active" else "layout-download-btn")
                        attr.title (if model.ViewLocked then "View Locked: Captured for cover" else "Lock 3D view for report cover")
                        attr.style "display: flex; align-items: center; justify-content: center; width: 36px; height: 36px; padding: 0; border-radius: 50%%;"
                        on.pointerdown (fun _ -> dispatch ToggleViewLock)
                        if model.ViewLocked then
                            rawHtml """<svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" style="display:block;"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 10 0v4"></path></svg>"""
                        else
                            rawHtml """<svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" style="display:block;"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 9.9-1"></path></svg>"""
                    }
                }

                div {
                    attr.style "width: 95%; max-width: 100%; aspect-ratio: 3/2; position: relative; overflow: hidden; background: #f9f9f9; border-radius: 8px;"
                    canvas { 
                        attr.id "hywe-extruded-polygon"
                        attr.style "width: 100%; height: 100%; display: block; touch-action: none;" 
                    }
                    async { do! ThreeD.extrudePolygons js "hywe-extruded-polygon" model.Derived.cxCxl1 model.Derived.cxClr1 model.Derived.cxElv1 model.ViewLocked }
                    |> Async.Start
                }

                // Export buttons back to bottom
                div {
                    attr.style "display: flex; gap: 10px; margin-top: 20px; align-items: center;"
                    button {
                        attr.``class`` "layout-download-btn"
                        attr.title "Download View as SVG"
                        on.pointerdown (fun _ -> dispatch Download3DSvg)
                        text "SVG"
                    }
                    button {
                        attr.``class`` "layout-download-btn"
                        on.pointerdown (fun _ -> dispatch DownloadDxf)
                        text "DXF"
                    }
                    button {
                        attr.``class`` "layout-download-btn"
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
    }