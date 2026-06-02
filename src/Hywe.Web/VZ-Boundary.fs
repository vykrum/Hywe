namespace Hywe.Site

open Bolero
open Bolero.Html
open Microsoft.JSInterop
open State

module View =

    // ---------- View ----------
    type bdrPgn = Template<"""<polygon class="${cs}" points="${pt}" stroke-width="${sw}"/>""">
    type bdrCrl = Template<"""<circle class="${cs}" cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
    type bdcrPh = Template<"""<path id="${pathid}" fill="none" letter-spacing="0.1" d="M ${sx},${sy} A ${r},${r} 0 1,1 ${ex},${ey} A ${r},${r} 0 1,1 ${sx},${sy}" />""">
    type bdcrTx = Template<"""
        <text id="${pth}" class="${tc}" font-size="${tf}" fill="#808080" text-anchor="middle">
          <textPath href="#${pth}" letter-spacing="0.1px" startOffset="50%">${nm}</textPath>
        </text>
        """>

    // Control and Instructions panel with numeric inputs and checkboxes
    let controlAndInstructions model dispatch (js: IJSRuntime) =
        let renderNumericInput labelText (value: float) msg isHeight =
            div {
                attr.``class`` "field-group"
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
            attr.``class`` "control-and-instructions"
            attr.style "zoom: 0.75; display: flex; flex-direction: row; flex-wrap: nowrap; gap: 16px; align-items: flex-start; justify-content: center; width: fit-content; max-width: 100%; margin: 0 auto; padding: 10px; box-sizing: border-box;"

            // Col 1: Segmented Pill Toggles
            div {
                attr.``class`` "toggle-column"
                attr.style "flex: 1; display: flex; flex-direction: column; gap: 8px;"

                // Boundary
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
                attr.``class`` "control-panel"
                attr.style (
                    if not model.UseBoundary || model.UseMapBase then
                        "flex: 0.5; display: flex; flex-direction: column; gap: 8px; border-left: 1px solid #e0e0e0; border-right: 1px solid #e0e0e0; padding: 0 12px; opacity: 0.3; pointer-events: none;"
                    else
                        "flex: 0.5; display: flex; flex-direction: column; gap: 8px; border-left: 1px solid #e0e0e0; border-right: 1px solid #e0e0e0; padding: 0 12px;"
                )

                // Width
                div {
                    renderNumericInput "Width:" model.DisplayWidth UpdateLogicalWidth false
                }
                // Height
                div {
                    renderNumericInput "Height:" model.DisplayHeight UpdateLogicalHeight true
                }
                // Scale
                // Scale
                div {
                    attr.``class`` "field-group"
                    span { attr.``class`` "hywe-label"; text "Scale:" }
                    span { 
                        attr.style "font-size: 0.95rem; font-weight: 600; color: #666; font-family: 'Segoe UI', sans-serif; text-align: right;"
                        text (sprintf "%d : 1" (int (if model.UseMapBase then model.MapScale else 1.0))) 
                    }
                }
            }

            // Col 3: Editor Instructions
            div {
                attr.``class`` "polygon-editor-instructions"
                attr.style (match model.UseBoundary with | true -> "flex: 1; display: flex; flex-direction: column; gap: 4px; font-size: 0.9rem; color: #555;" | _ -> "flex: 1; display: flex; flex-direction: column; gap: 4px; font-size: 0.9rem; color: #555; opacity: 0.3; pointer-events: none;")
                p { attr.style "margin: 0;"; text "Click edge to add point" }
                p { attr.style "margin: 0;"; text "Double-click point to delete" }
                p { attr.style "margin: 0;"; text "Double-click inside to add island" }
                p { attr.style "margin: 0;"; text "Double-click island to delete" }
            }
            

        }

    // Polygon Editor SVG with polygons, vertices, and event handlers
    let polygonEditorSvg model dispatch =
                let boundScale = match model.LogicalWidth with
                                    | w when w <> fst initBound -> w / fst initBound
                                    | _ -> 1.0            
                let boundRadius = max 1 (int (float model.VertexRadius * boundScale))
                let boundLabel = max 1(int (float (model.VertexRadius + 4) * boundScale))  
                let bndTxtRr = max 1(int(float (model.VertexRadius + 6) * boundScale))
                let bndStWdO = max 1 (int (6.0 * boundScale))
                let bndStWdI = max 1 (int (4.0 * boundScale))            
                
                let boundingBoxWithLogical =
                    let allPoints = Array.append model.Outer (model.Islands |> Array.collect id)
                    if allPoints.Length = 0 then
                        (0.0, 0.0, model.LogicalWidth, model.LogicalHeight)
                    else
                        let mutable minX = System.Double.MaxValue
                        let mutable maxX = System.Double.MinValue
                        let mutable minY = System.Double.MaxValue
                        let mutable maxY = System.Double.MinValue
                        for i = 0 to allPoints.Length - 1 do
                            let p = allPoints.[i]
                            if p.X < minX then minX <- p.X
                            if p.X > maxX then maxX <- p.X
                            if p.Y < minY then minY <- p.Y
                            if p.Y > maxY then maxY <- p.Y
                        
                        let minX' = min 0.0 minX
                        let minY' = min 0.0 minY
                        let maxX' = max model.LogicalWidth maxX
                        let maxY' = max model.LogicalHeight maxY
                        (minX', minY', maxX' - minX', maxY' - minY')

                let (x, y, w, h) = boundingBoxWithLogical
                let padding = 50.0 * boundScale

                // Allow min-x / min-y to go negative
                let minX = x - padding
                let minY = y - padding

                // Ensure width / height never negative or zero
                let safeW = max 1.0 (w + 2.0 * padding)
                let safeH = max 1.0 (h + 2.0 * padding)

                let viewBoxString = sprintf "%f %f %f %f" minX minY safeW safeH

                svg {
                attr.id "polygon-editor-svg"
                attr.``class`` "polygon-editor-svg"
                "data-padding-ratio" => (((2.0 * padding) / safeW).ToString(System.Globalization.CultureInfo.InvariantCulture))
                attr.style (match model.UseMapBase with | true -> "margin: 0; background-color: transparent; width: 100%; height: 100%;" | false -> "")
                "viewBox" => viewBoxString

                // Pointer events
                on.pointerdown (fun ev -> dispatch (PointerDown ev))
                on.pointerup (fun _ -> dispatch PointerUp)
                on.pointermove (fun ev -> dispatch (PointerMove ev))
                on.dblclick (fun ev -> dispatch (DoubleClick ev))

                // Outer polygon
                bdrPgn()
                    .cs(match model.UseMapBase with | true -> "outerPolygon mapModeOpacity" | false -> "outerPolygon")
                    .pt(model.OuterPointsStr)
                    .sw(string bndStWdO)
                    .Elt()

                // Islands
                for i = 0 to model.DisplayIslands.Length - 1 do
                    bdrPgn()
                        .cs(match model.UseMapBase with | true -> "islandPolygon mapModeOpacity" | false -> "islandPolygon")
                        .pt(model.IslandPointsStrs.[i])
                        .sw(string bndStWdI)
                        .Elt()

                // Outer vertices
                for i = 0 to model.Outer.Length - 1 do
                    let rawPt = model.Outer.[i]
                    let dispPt = model.DisplayOuter.[i]
                    let cartX = int (System.Math.Round(dispPt.X))
                    let cartY = int (System.Math.Round(dispPt.Y))
                    bdrCrl()
                        .cs("outerVertex")
                        .cx(sprintf "%.1f" rawPt.X)
                        .cy(sprintf "%.1f" rawPt.Y)
                        .cr(string boundRadius)
                        .cl("#333")
                        .Elt()

                    bdcrPh()
                        .pathid(sprintf "outerVertex-%d" i)
                        .sx($"{rawPt.X}")
                        .sy($"{rawPt.Y + float bndTxtRr}")
                        .r($"{bndTxtRr}")
                        .ex($"{rawPt.X}")
                        .ey($"{rawPt.Y - float bndTxtRr}")
                        .Elt()

                    bdcrTx()
                        .pth(sprintf "outerVertex-%d" i)
                        .tc("outerVertexLabel")
                        .tf(boundLabel)
                        .nm(sprintf "(%d, %d)" cartX cartY)
                        .Elt()

                // Island vertices
                for i = 0 to model.Islands.Length - 1 do
                    for j = 0 to model.Islands.[i].Length - 1 do
                        let rawPt = model.Islands.[i].[j]
                        let dispPt = model.DisplayIslands.[i].[j]
                        let cartX = int (System.Math.Round(dispPt.X))
                        let cartY = int (System.Math.Round(dispPt.Y))
                        
                        bdrCrl()    .cs("islandVertex")
                            .cx(sprintf "%.1f" rawPt.X)
                            .cy(sprintf "%.1f" rawPt.Y)
                            .cr(string boundRadius)
                            .cl("#333")
                            .Elt()

                        bdcrPh()
                            .pathid(sprintf "islandVertex-%d-%d" i j)
                            .sx($"{rawPt.X}")
                            .sy($"{rawPt.Y + float bndTxtRr}")
                            .r($"{bndTxtRr}")
                            .ex($"{rawPt.X}")
                            .ey($"{rawPt.Y - float bndTxtRr}")
                            .Elt()

                        bdcrTx()
                            .pth(sprintf "islandVertex-%d-%d" i j) 
                            .tc("islandVertexLabel")
                            .tf(boundLabel)
                            .nm(sprintf "(%d, %d)" cartX cartY)
                            .Elt()

                // --- Entry point ---
                let scale = boundScale * 0.3
                elt "g" {
                    attr.style (sprintf "transform: translate(%.1fpx, %.1fpx) scale(%.3f);" model.EntryPoint.X model.EntryPoint.Y scale)
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
                attr.style (
                    let aspectRatio =
                        if model.UseBoundary && not model.UseMapBase && model.LogicalHeight > 0.0 then
                            sprintf "%.6f" (model.LogicalWidth / model.LogicalHeight)
                        else
                            "1"
                    match model.UseBoundary, model.UseMapBase with
                    | false, false -> "position: relative; width: 100%; max-width: 800px; aspect-ratio: 1 / 1; margin: 20px auto; border: none; background: transparent;"
                    | _ -> sprintf "position: relative; width: 100%%; max-width: 800px; aspect-ratio: %s; margin: 20px auto; border: 1px solid #e0e0e0; background: #f0f0f0;" aspectRatio
                )
                
                // Hymap Layer Wrapper (Handles dynamic state so hymap-container itself is strictly static and NEVER re-rendered by Blazor)
                div {
                    attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; z-index: 0; %s" 
                        (if model.UseMapBase then 
                            (if model.IsMapLocked then "pointer-events: none;" else "pointer-events: auto;")
                         else "visibility: hidden;"))
                         
                    // Hymap Layer (Native) - Absolutely no children or dynamic attributes to ensure Leaflet DOM is fully preserved
                    div {
                        attr.id "hymap-container"
                        attr.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 0;"
                    }
                }

                // SVG Editor Layer
                div {
                    attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; z-index: 1; pointer-events: %s;" (match model.UseMapBase && not model.IsMapLocked with | true -> "none" | false -> "auto"))
                    match model.PolygonEnabled with
                    | true -> polygonEditorSvg model dispatch
                    | false ->     div {
                                        attr.style "pointer-events:none; opacity:0.5; width: 100%; height: 100%;"
                                        polygonEditorSvg model dispatch}
                }

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
        }
