namespace Hywe.Site

open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components.Web
open Microsoft.JSInterop
open State

module View =

    // ---------- View ----------
    type bdrPgn = Template<"""<polygon class="${cs}" points="${pt}" stroke-width="${sw}"/>""">
    type bdrCrl = Template<"""<circle class="${cs}" cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
    type vtxTxt = Template<"""<text class="${tc}" x="${x}" y="${y}" font-size="${tf}" text-anchor="middle" dominant-baseline="auto">${nm}</text>""">
    type ghstVtx = Template<"""<g style="pointer-events: none;"><circle class="ghostVertex" cx="${cx}" cy="${cy}" r="${cr}" fill="none" stroke="#2563eb" stroke-width="2" stroke-dasharray="3,3"/><circle cx="${cx}" cy="${cy}" r="3" fill="#2563eb"/><text x="${cx}" y="${ty}" font-size="${tf}" font-weight="bold" fill="#2563eb" text-anchor="middle">+</text></g>""">
    type selHlo = Template<"""<circle class="selectedVertexHalo" cx="${cx}" cy="${cy}" r="${cr}" fill="none" stroke="#2563eb" stroke-width="2.5" stroke-dasharray="3,3" style="pointer-events: none;" />""">

    // Control panel with transposed horizontal rows for toggles and dimensions
    let controlAndInstructions model dispatch (js: IJSRuntime) =
        let renderNumericInput labelText (value: float) msg isHeight =
            div {
                attr.``class`` "field-group"
                attr.style "display: flex; align-items: center; justify-content: space-between; gap: 8px;"
                label { attr.``class`` "hywe-label"; text labelText }
                input {
                    attr.``class`` "boundaryInput"
                    attr.``type`` "number"
                    attr.min "10"
                    attr.max "100"
                    attr.value (string (System.Math.Round(value)))
                    attr.disabled (not model.UseBoundary || model.UseMapBase)
                    on.change (fun ev ->
                        let factor = 10.0 // Inputs are disabled in Map Mode, so edits are only manual (factor 10.0)
                        match System.Double.TryParse (string ev.Value) with
                        | (true, v) -> dispatch (msg (v * factor))
                        | _ -> ()
                    )
                }
            }

        div {
            attr.``class`` "boundary-toolbar"

            // Help Button with sleek Help SVG icon (positioned at toolbar top-right to preserve two-column alignment)
            button {
                attr.id "hywe-boundary-guide-btn"
                attr.``type`` "button"
                attr.title "Boundary Controls & Mode Help"
                "aria-label" => "Boundary Controls & Mode Help"
                attr.``class`` ("hywe-btn hywe-btn-circle hywe-btn-sm " + (if model.ShowInstructions then "hywe-btn-dark active" else "hywe-btn-flat"))
                attr.style "position: absolute; top: 10px; right: 12px; width: 26px; height: 26px; display: inline-flex; align-items: center; justify-content: center; padding: 0; z-index: 2;"
                on.click (fun _ -> dispatch ToggleInstructions)
                svg {
                    "viewBox" => "0 0 24 24"
                    attr.style "width: 15px; height: 15px; display: block;"
                    elt "circle" { "cx" => "12"; "cy" => "12"; "r" => "10"; "stroke" => "currentColor"; "stroke-width" => "1.8"; "fill" => "none" }
                    elt "path" { "d" => "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"; "stroke" => "currentColor"; "stroke-width" => "1.8"; "stroke-linecap" => "round"; "stroke-linejoin" => "round"; "fill" => "none" }
                    elt "circle" { "cx" => "12"; "cy" => "17"; "r" => "1.1"; "fill" => "currentColor" }
                }
            }

            // Col 1: Segmented Pill Toggles
            div {
                attr.``class`` "toggle-column"

                // Site
                div {
                    attr.``class`` "hywe-row"
                    span { attr.``class`` "hywe-label"; attr.style "flex-shrink: 0; min-width: 45px;"; text "Site:" }
                    div {
                        attr.``class`` "hywe-btn-group"
                        button {
                            attr.``class`` (if not model.UseBoundary then "hywe-btn hywe-btn-sm hywe-btn-dark active" else "hywe-btn hywe-btn-sm hywe-btn-flat")
                            attr.style "padding-left: 8px; padding-right: 8px;"
                            on.click (fun _ -> if model.UseBoundary then dispatch (ToggleBoundary false))
                            text "None"
                        }
                        button {
                            attr.``class`` (if model.UseBoundary then "hywe-btn hywe-btn-sm hywe-btn-dark active" else "hywe-btn hywe-btn-sm hywe-btn-flat")
                            attr.style "padding-left: 8px; padding-right: 8px;"
                            on.click (fun _ -> if not model.UseBoundary then dispatch (ToggleBoundary true))
                            text "Boundary"
                        }
                    }
                }

                // Count
                div {
                    attr.``class`` ("hywe-row" + (if model.UseBoundary then "" else " disabled"))
                    span { attr.``class`` "hywe-label"; attr.style "flex-shrink: 0; min-width: 45px;"; text "Count:" }
                    div {
                        attr.``class`` "hywe-btn-group"
                        button {
                            attr.``class`` (if not model.UseAbsolute then "hywe-btn hywe-btn-sm hywe-btn-dark active" else "hywe-btn hywe-btn-sm hywe-btn-flat")
                            attr.style "padding-left: 8px; padding-right: 8px;"
                            on.click (fun _ -> if model.UseAbsolute then dispatch (ToggleAbsolute false))
                            text "Relative"
                        }
                        button {
                            attr.``class`` (if model.UseAbsolute then "hywe-btn hywe-btn-sm hywe-btn-dark active" else "hywe-btn hywe-btn-sm hywe-btn-flat")
                            attr.style "padding-left: 8px; padding-right: 8px;"
                            on.click (fun _ -> if not model.UseAbsolute then dispatch (ToggleAbsolute true))
                            text "Absolute"
                        }
                    }
                }

                // Base
                div {
                    attr.``class`` ("hywe-row" + (if model.UseBoundary then "" else " disabled"))
                    span { attr.``class`` "hywe-label"; attr.style "flex-shrink: 0; min-width: 45px;"; text "Base:" }
                    div {
                        attr.``class`` "hywe-btn-group"
                        button {
                            attr.``class`` (if not model.UseMapBase then "hywe-btn hywe-btn-sm hywe-btn-dark active" else "hywe-btn hywe-btn-sm hywe-btn-flat")
                            attr.style "padding-left: 8px; padding-right: 8px;"
                            on.click (fun _ ->
                                if model.UseMapBase then dispatch (ToggleMapBase false)
                            )
                            text "None"
                        }
                        button {
                            attr.``class`` (if model.UseMapBase then "hywe-btn hywe-btn-sm hywe-btn-dark active" else "hywe-btn hywe-btn-sm hywe-btn-flat")
                            attr.style "padding-left: 8px; padding-right: 8px;"
                            on.click (fun _ ->
                                if not model.UseMapBase then
                                    dispatch (ToggleMapBase true)
                                    js.InvokeVoidAsync("Hymap.init").AsTask() |> ignore
                            )
                            text "Map"
                        }
                    }
                }
            }

            // Col 2: Dimensions & Scale
            div {
                attr.``class`` ("dimension-fields" + (if not model.UseBoundary || model.UseMapBase then " disabled" else ""))
                attr.style (
                    if not model.UseBoundary || model.UseMapBase then
                        "opacity: 0.3; pointer-events: none;"
                    else
                        ""
                )

                renderNumericInput "Width:" model.DisplayWidth UpdateLogicalWidth false
                renderNumericInput "Height:" model.DisplayHeight UpdateLogicalHeight true

                div {
                    attr.``class`` "field-group"
                    span { attr.``class`` "hywe-label"; text "Scale:" }
                    span { 
                        attr.style "font-size: 0.95rem; font-weight: 600; color: #666; font-family: 'Segoe UI', sans-serif; text-align: right; padding-right: 4px;"
                        text (sprintf "%d : 1" (int (if model.UseMapBase then model.MapScale else 1.0))) 
                    }
                }
            }
        }

    // Instructions Modal / Card (Text-only, strictly zero icons)
    let instructionsModal model dispatch (js: IJSRuntime) =
        if model.ShowInstructions then
            let closeGuide () =
                dispatch ToggleInstructions
                js.InvokeVoidAsync("eval", "var b = document.getElementById('hywe-boundary-guide-btn'); if(b){b.focus({preventScroll:true});}else if(document.activeElement){document.activeElement.blur();}") |> ignore

            div {
                attr.``class`` "boundary-instructions-overlay"
                attr.style "position: fixed; inset: 0; background: rgba(0,0,0,0.35); z-index: 3000; display: flex; align-items: center; justify-content: center; padding: 20px; box-sizing: border-box;"
                on.click (fun _ -> closeGuide ())

                div {
                    attr.``class`` "boundary-instructions-card"
                    attr.style "width: 100%; max-width: 440px; max-height: 85vh; overflow-y: auto; background: #ffffff; border-radius: 8px; box-shadow: 0 12px 30px rgba(0,0,0,0.25); padding: 20px 22px; font-family: 'Segoe UI', system-ui, sans-serif; box-sizing: border-box;"
                    "onclick:stopPropagation" => true
                    "onpointerdown:stopPropagation" => true

                    div {
                        attr.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 14px; border-bottom: 1px solid #e0e0e0; padding-bottom: 8px;"
                        h4 { attr.style "margin: 0; font-size: 1.05rem; color: #111; font-weight: 600;"; text "Boundary & Controls Guide" }
                        button {
                            attr.``type`` "button"
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                            attr.style "padding: 2px 8px; font-size: 0.85rem;"
                            on.click (fun _ -> closeGuide ())
                            text "Close"
                        }
                    }

                    div {
                        attr.style "display: flex; flex-direction: column; gap: 14px; font-size: 0.86rem; color: #444; line-height: 1.45;"

                        // Section 1: Modes & Toggles
                        div {
                            attr.style "display: flex; flex-direction: column; gap: 8px;"
                            div {
                                attr.style "font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 700; color: #777;"
                                text "Toolbar Modes"
                            }

                            div {
                                span { attr.style "font-weight: 600; color: #111;"; text "Site (None / Boundary): " }
                                text "None generates unconstrained layouts without perimeter boundaries. Boundary constrains space generation strictly within your custom perimeter and interior islands."
                            }

                            div {
                                span { attr.style "font-weight: 600; color: #111;"; text "Count (Relative / Absolute): " }
                                text "Relative dynamically reproportions space area weights to fit the available site area. Absolute allocates exact specified module/hexel counts."
                            }

                            div {
                                span { attr.style "font-weight: 600; color: #111;"; text "Base (None / Map): " }
                                text "None uses a blank canvas with manual dimensions (Width, Height, Scale). Map loads an interactive OpenStreetMap underlay with geographic scaling."
                            }
                        }

                        div { attr.style "border-top: 1px solid #eee; margin: 2px 0;" }

                        // Section 2: Canvas & Vertex Controls
                        div {
                            attr.style "display: flex; flex-direction: column; gap: 8px;"
                            div {
                                attr.style "font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 700; color: #777;"
                                text "Canvas Controls"
                            }

                            div { span { attr.style "font-weight: 600; color: #111;"; text "Select vertex: " }; text "Click or tap vertex (press Delete / Backspace to remove)" }
                            div { span { attr.style "font-weight: 600; color: #111;"; text "Delete vertex: " }; text "Double-click / double-tap, or press Delete / Backspace when selected" }
                            div { span { attr.style "font-weight: 600; color: #111;"; text "Add vertex: " }; text "Hover near boundary edge and click / tap" }
                            div { span { attr.style "font-weight: 600; color: #111;"; text "Move island: " }; text "Drag inside island body" }
                            div { span { attr.style "font-weight: 600; color: #111;"; text "Relocate entrance: " }; text "Drag entrance marker" }
                            div { span { attr.style "font-weight: 600; color: #111;"; text "Add island: " }; text "Double-click empty canvas area" }
                            div { span { attr.style "font-weight: 600; color: #111;"; text "Delete island: " }; text "Double-click inside island body" }
                        }
                    }
                }
            }
        else
            empty()

    // Polygon Editor SVG with polygons, vertices, and event handlers
    let polygonEditorSvg model dispatch (js: IJSRuntime) =
        let boundingBoxWithLogical =
            let allPoints = Array.append model.Outer (model.Islands |> Array.collect id)
            match allPoints.Length with
            | 0 -> (0.0, 0.0, model.LogicalWidth, model.LogicalHeight)
            | _ ->
                let minX, maxX, minY, maxY =
                    allPoints
                    |> Array.fold (fun (mnX, mxX, mnY, mxY) p ->
                        (min mnX p.X, max mxX p.X, min mnY p.Y, max mxY p.Y)
                    ) (System.Double.MaxValue, System.Double.MinValue, System.Double.MaxValue, System.Double.MinValue)
                
                let minX' = min 0.0 minX
                let minY' = min 0.0 minY
                let maxX' = max model.LogicalWidth maxX
                let maxY' = max model.LogicalHeight maxY
                (minX', minY', maxX' - minX', maxY' - minY')

        let (x, y, w, h) = boundingBoxWithLogical
        let padX = max 25.0 (w * 0.12)
        let padY = max 25.0 (h * 0.12)
        let padRatio = max (padX / w) (padY / h)
        let padX' = w * padRatio
        let padY' = h * padRatio

        let minX = x - padX'
        let minY = y - padY'
        let safeW = max 1.0 (w + 2.0 * padX')
        let safeH = max 1.0 (h + 2.0 * padY')

        let clientW =
            match model.SvgInfo with
            | Some info when info.ClientW > 0.0 -> info.ClientW
            | _ ->
                try
                    match box js with
                    | :? Microsoft.JSInterop.IJSInProcessRuntime as inProc ->
                        let cw = inProc.Invoke<float>("getSvgWidth", "polygon-editor-svg")
                        if cw > 0.0 then cw else 400.0
                    | _ -> 400.0
                with _ -> 400.0

        let svgScale = safeW / max 200.0 clientW
        let boundRadius = max 4.0 (10.5 * svgScale)
        let boundLabel = max 6.0 (12.5 * svgScale)
        let boundLabelInt = max 1 (int (System.Math.Round(boundLabel)))
        let bndStWdO = max 1 (int (System.Math.Round(max 1.0 (3.5 * svgScale))))
        let bndStWdI = max 1 (int (System.Math.Round(max 1.0 (2.5 * svgScale))))
        let entryScale = 0.8 * svgScale
        let haloCr = sprintf "%.1f" (boundRadius + 5.0 * svgScale)
        let boundRadiusStr = sprintf "%.1f" boundRadius
        let textYOffset = boundRadius + 6.0 * svgScale

        let viewBoxString = sprintf "%f %f %f %f" minX minY safeW safeH

        svg {
            attr.id "polygon-editor-svg"
            attr.``class`` "polygon-editor-svg"
            attr.tabindex -1
            "data-padding-ratio" => (((2.0 * padX') / safeW).ToString(System.Globalization.CultureInfo.InvariantCulture))
            attr.style (match model.UseMapBase with | true -> "background-color: transparent;" | false -> "")
            "viewBox" => viewBoxString

            // Pointer events with pointer capture for unbreakable dragging
            on.pointerdown (fun ev ->
                let ptrId = match box ev with | :? PointerEventArgs as pev -> pev.PointerId | _ -> 1L
                js.InvokeVoidAsync("capturePointer", "polygon-editor-svg", ptrId) |> ignore
                dispatch (PointerDown ev)
            )
            on.pointerup (fun ev ->
                let ptrId = match box ev with | :? PointerEventArgs as pev -> pev.PointerId | _ -> 1L
                js.InvokeVoidAsync("releasePointer", "polygon-editor-svg", ptrId) |> ignore
                dispatch PointerUp
            )
            on.pointermove (fun ev -> dispatch (PointerMove ev))
            on.dblclick (fun ev -> dispatch (DoubleClick ev))
            on.keydown (fun ev -> dispatch (KeyDown ev))

            // Outer polygon
            bdrPgn()
                .cs(match model.UseMapBase with | true -> "outerPolygon mapModeOpacity" | false -> "outerPolygon")
                .pt(model.OuterPointsStr)
                .sw(string bndStWdO)
                .Elt()

            // Islands
            forEach (Array.indexed model.IslandPointsStrs) (fun (i, islandPtsStr) ->
                bdrPgn()
                    .cs(match model.UseMapBase with | true -> "islandPolygon mapModeOpacity" | false -> "islandPolygon")
                    .pt(islandPtsStr)
                    .sw(string bndStWdI)
                    .Elt()
            )

            // Outer vertices
            forEach (Array.indexed model.Outer) (fun (i, rawPt) ->
                let dispPt = model.DisplayOuter.[i]
                let cartX = int (System.Math.Round(dispPt.X))
                let cartY = int (System.Math.Round(dispPt.Y))
                let isSelected =
                    match model.SelectedVertex with
                    | Some sel -> sel.PolyIndex = 0 && sel.VertexIndex = i
                    | None -> false

                concat {
                    if isSelected then
                        selHlo()
                            .cx(sprintf "%.1f" rawPt.X)
                            .cy(sprintf "%.1f" rawPt.Y)
                            .cr(haloCr)
                            .Elt()

                    bdrCrl()
                        .cs(match isSelected with true -> "outerVertex selected" | false -> "outerVertex")
                        .cx(sprintf "%.1f" rawPt.X)
                        .cy(sprintf "%.1f" rawPt.Y)
                        .cr(boundRadiusStr)
                        .cl(match isSelected with true -> "#2563eb" | false -> "#333")
                        .Elt()

                    vtxTxt()
                        .tc("outerVertexLabel")
                        .x(sprintf "%.1f" rawPt.X)
                        .y(sprintf "%.1f" (rawPt.Y - textYOffset))
                        .tf(boundLabelInt)
                        .nm(sprintf "(%d, %d)" cartX cartY)
                        .Elt()
                }
            )

            // Island vertices
            forEach (Array.indexed model.Islands) (fun (i, isl) ->
                forEach (Array.indexed isl) (fun (j, rawPt) ->
                    let dispPt = model.DisplayIslands.[i].[j]
                    let cartX = int (System.Math.Round(dispPt.X))
                    let cartY = int (System.Math.Round(dispPt.Y))
                    let isSelected =
                        match model.SelectedVertex with
                        | Some sel -> sel.PolyIndex = i + 1 && sel.VertexIndex = j
                        | None -> false

                    concat {
                        if isSelected then
                            selHlo()
                                .cx(sprintf "%.1f" rawPt.X)
                                .cy(sprintf "%.1f" rawPt.Y)
                                .cr(haloCr)
                                .Elt()

                        bdrCrl()
                            .cs(match isSelected with true -> "islandVertex selected" | false -> "islandVertex")
                            .cx(sprintf "%.1f" rawPt.X)
                            .cy(sprintf "%.1f" rawPt.Y)
                            .cr(boundRadiusStr)
                            .cl(match isSelected with true -> "#2563eb" | false -> "#333")
                            .Elt()

                        vtxTxt()
                            .tc("islandVertexLabel")
                            .x(sprintf "%.1f" rawPt.X)
                            .y(sprintf "%.1f" (rawPt.Y - textYOffset))
                            .tf(boundLabelInt)
                            .nm(sprintf "(%d, %d)" cartX cartY)
                            .Elt()
                    }
                )
            )

            // Ghost vertex preview on edge hover
            match model.GhostVertex with
            | Some ghost ->
                ghstVtx()
                    .cx(sprintf "%.1f" ghost.Point.X)
                    .cy(sprintf "%.1f" ghost.Point.Y)
                    .ty(sprintf "%.1f" (ghost.Point.Y + boundLabel * 0.35))
                    .cr(sprintf "%.1f" (boundRadius + 2.0 * svgScale))
                    .tf(boundLabelInt)
                    .Elt()
            | None -> ()

            // --- Entry point ---
            elt "g" {
                attr.style (sprintf "transform: translate(%.1fpx, %.1fpx) scale(%.3f);" model.EntryPoint.X model.EntryPoint.Y entryScale)
                bdrPgn()
                    .cs("entryPoint")
                    .pt("-15,25 15,25 15,15 -5,15 -5,5 15,5 15,-5 -5,-5 -5,-15 15,-15 15,-25 -15,-25")
                    .sw("0")
                    .Elt()
            }
        }

    let view model dispatch (js: IJSRuntime) =
        div {
            controlAndInstructions model dispatch js

            // Hidden fields for JS interop callback
            input { attr.id "hymap-data"; attr.``type`` "hidden" }
            button {
                attr.id "hymap-trigger"
                attr.style "display:none;"
                on.click (fun _ -> 
                    async {
                        let! dataStr = js.InvokeAsync<string>("eval", [| box "document.getElementById('hymap-data').value" |]).AsTask() |> Async.AwaitTask
                        if not (System.String.IsNullOrWhiteSpace(dataStr)) then
                            try
                                let doc = System.Text.Json.JsonDocument.Parse(dataStr)
                                let root = doc.RootElement
                                let w = root.GetProperty("widthMeters").GetDouble()
                                let h = root.GetProperty("heightMeters").GetDouble()
                                dispatch (MapTopographyReceived (w, h, dataStr))
                            with ex ->
                                printfn "Error parsing topography: %s" ex.Message
                    } |> Async.StartImmediate
                )
            }

            // Hidden fields for live dimension updates
            input { attr.id "hymap-live-data"; attr.``type`` "hidden" }
            button {
                attr.id "hymap-live-trigger"
                attr.style "display:none;"
                on.click (fun _ -> 
                    async {
                        let! dataStr = js.InvokeAsync<string>("eval", [| box "document.getElementById('hymap-live-data').value" |]).AsTask() |> Async.AwaitTask
                        if not (System.String.IsNullOrWhiteSpace(dataStr)) then
                            try
                                let doc = System.Text.Json.JsonDocument.Parse(dataStr)
                                let root = doc.RootElement
                                let w = root.GetProperty("widthMeters").GetDouble()
                                let h = root.GetProperty("heightMeters").GetDouble()
                                dispatch (UpdateLogicalDimensions (w, h))
                            with ex ->
                                printfn "Error parsing live dimensions: %s" ex.Message
                    } |> Async.StartImmediate
                )
            }

            // Map and SVG Container
            div {
                attr.key "map-and-svg-container"
                attr.id "map-and-svg-container"
                attr.``class`` "boundary-svg-container"
                attr.style (
                    let aspectRatio =
                        if model.UseBoundary && not model.UseMapBase && model.LogicalHeight > 0.0 then
                            sprintf "%.6f" (model.LogicalWidth / model.LogicalHeight)
                        else
                            "1"
                    match model.UseBoundary, model.UseMapBase with
                    | false, false -> "aspect-ratio: 1 / 1; border: none; background: transparent;"
                    | _ -> sprintf "aspect-ratio: %s; border: 1px solid #e0e0e0; background: #f0f0f0;" aspectRatio
                )
                
                // Hymap Layer Wrapper (Handles dynamic state so hymap-container itself is strictly static and NEVER re-rendered by Blazor)
                div {
                    attr.key "hymap-wrapper"
                    attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; z-index: 0; %s" 
                        (if model.UseMapBase then 
                            (if model.IsMapLocked then "pointer-events: none;" else "pointer-events: auto;")
                         else "visibility: hidden;"))
                         
                    // Hymap Layer (Native) - Absolutely no children or dynamic attributes to ensure Leaflet DOM is fully preserved
                    div {
                        attr.key "hymap-container"
                        attr.id "hymap-container"
                        attr.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 0;"
                    }
                }

                // SVG Editor Layer
                div {
                    attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; z-index: 1; pointer-events: %s;" (match model.UseMapBase && not model.IsMapLocked with | true -> "none" | false -> "auto"))
                    match model.PolygonEnabled with
                    | true -> polygonEditorSvg model dispatch js
                    | false ->     div {
                                        attr.style "pointer-events:none; opacity:0.5; width: 100%; height: 100%;"
                                        polygonEditorSvg model dispatch js}
                }

                // Selection action chip (especially convenient on mobile touchscreens without Delete/Backspace key)
                match model.SelectedVertex with
                | Some sel ->
                    div {
                        attr.``class`` "selected-vertex-actions"
                        attr.style "position: absolute; bottom: 12px; left: 50%; transform: translateX(-50%); z-index: 2000; display: flex; align-items: center; gap: 8px; background: rgba(255, 255, 255, 0.96); backdrop-filter: blur(8px); padding: 5px 12px; border-radius: 20px; box-shadow: 0 4px 14px rgba(0,0,0,0.18); border: 1px solid #e0e0e0; pointer-events: auto;"
                        button {
                            attr.``type`` "button"
                            attr.``class`` "hywe-btn hywe-btn-sm"
                            attr.style "background: #fee2e2; color: #dc2626; border: 1px solid #fca5a5; font-weight: 600; border-radius: 12px; padding: 4px 10px; display: inline-flex; align-items: center; gap: 4px; font-size: 0.82rem;"
                            on.click (fun _ -> dispatch DeleteSelectedVertex)
                            rawHtml """<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 6h18M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2M10 11v6M14 11v6"/></svg>"""
                            text "Delete Vertex"
                        }
                        button {
                            attr.``type`` "button"
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-flat"
                            attr.style "font-size: 0.8rem; padding: 4px 8px; color: #666;"
                            on.click (fun _ -> dispatch (SelectVertex None))
                            text "Deselect"
                        }
                    }
                | None -> empty()

                // Lock Icon Overlay (Top Right)
                if model.UseMapBase then
                    div {
                        attr.style "position: absolute; top: 10px; right: 10px; z-index: 2000; cursor: pointer; background: white; width: 34px; height: 34px; border-radius: 4px; box-shadow: 0 1px 4px rgba(0,0,0,0.3); display: flex; align-items: center; justify-content: center; transition: background 0.2s;"
                        on.click (fun _ ->
                            let newState = not model.IsMapLocked
                            dispatch (ToggleMapLock newState)
                            if newState then
                                js.InvokeVoidAsync("Hymap.lockMap").AsTask() |> ignore
                            else
                                js.InvokeVoidAsync("Hymap.unlockMap").AsTask() |> ignore
                        )
                        
                        if model.IsMapLocked then
                            // Locked Icon
                            rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="#e63946" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 10 0v4"></path></svg>"""
                        else
                            // Unlocked Icon
                            rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="#333" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 9.9-1"></path></svg>"""
                    }
            }

            // Bottom Action Bar
            if model.UseMapBase && model.IsMapLocked && model.TopographyData.IsSome then
                div {
                    attr.style "display: flex; justify-content: center; gap: 12px; margin-top: 10px; padding-bottom: 30px;"
                    button { // Download Map Image
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                        on.click (fun _ -> FileManager.exportMapImage js)
                        text "Download Map Image"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                        on.click (fun _ -> FileManager.exportMapData js model.TopographyData.Value "terrain")
                        text "Download Terrain Grid"
                    }
                }
            else
                empty()

            // Instructions Modal / Card (rendered cleanly at boundary view root level)
            instructionsModal model dispatch js
        }
