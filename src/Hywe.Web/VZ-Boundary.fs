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
                label { text labelText }
                input {
                    attr.``class`` "boundaryInput"
                    attr.``type`` "number"
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
            attr.style "display: flex; flex-flow: row wrap; align-items: flex-start; justify-content: center; gap: 8px; width: 100%; padding: 4px;"

            // Col 1: Small Toggles
            div {
                attr.``class`` "toggle-column"
                
                div {
                    attr.``class`` "hywe-switch-container"
                    label {
                        attr.``class`` "hywe-switch"
                        input {
                            attr.``type`` "checkbox"
                            attr.``checked`` model.UseBoundary
                            on.change (fun _ -> dispatch (ToggleBoundary (not model.UseBoundary)))
                        }
                        span { attr.``class`` "hywe-switch-slider" }
                    }
                    span {
                        attr.``class`` "hywe-switch-label"
                        text "Boundary"
                    }
                }

                div {
                    attr.``class`` "hywe-switch-container"
                    attr.style (match model.UseBoundary with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
                    label {
                        attr.``class`` "hywe-switch"
                        input {
                            attr.``type`` "checkbox"
                            attr.``checked`` (not model.UseAbsolute)
                            on.change (fun _ -> dispatch (ToggleAbsolute (not model.UseAbsolute)))
                        }
                        span { attr.``class`` "hywe-switch-slider" }
                    }
                    span {
                        attr.``class`` "hywe-switch-label"
                        text "Relative"
                    }
                }
            }

            // Col 1.5: Map Toggles
            div {
                attr.``class`` "toggle-column"
                attr.style (match model.UseBoundary with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
                
                div {
                    attr.``class`` "hywe-switch-container"
                    label {
                        attr.``class`` "hywe-switch"
                        input {
                            attr.``type`` "checkbox"
                            attr.``checked`` model.UseMapBase
                            on.change (fun _ -> 
                                let newState = not model.UseMapBase
                                dispatch (ToggleMapBase newState)
                                if newState then
                                    js.InvokeVoidAsync("Hymap.init").AsTask() |> ignore
                            )
                        }
                        span { attr.``class`` "hywe-switch-slider" }
                    }
                    span {
                        attr.``class`` "hywe-switch-label"
                        text "Map Base"
                    }
                }

                div {
                    attr.``class`` "hywe-switch-container"
                    attr.style (match model.UseMapBase with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
                    label {
                        attr.``class`` "hywe-switch"
                        input {
                            attr.``type`` "checkbox"
                            attr.``checked`` model.IsMapLocked
                            on.change (fun _ -> 
                                let newState = not model.IsMapLocked
                                dispatch (ToggleMapLock newState)
                                if newState then
                                    js.InvokeVoidAsync("Hymap.lockMap").AsTask() |> ignore
                                else
                                    js.InvokeVoidAsync("Hymap.unlockMap").AsTask() |> ignore
                            )
                        }
                        span { attr.``class`` "hywe-switch-slider" }
                    }
                    span {
                        attr.``class`` "hywe-switch-label"
                        text "Lock Map"
                    }
                }
            }

            // Col 2: Dimensions
            div {
            attr.``class`` "control-panel"
            div {
                attr.style "display: flex; gap: 15px; margin-bottom: 10px;"
                renderNumericInput "Width:" model.DisplayWidth UpdateLogicalWidth false
                renderNumericInput "Length:" model.DisplayHeight UpdateLogicalHeight true
            }
            }

            // Col 3: Tight Instructions
            div {
                attr.``class`` "polygon-editor-instructions"
                attr.style (match model.UseBoundary with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
                p { text "Clk edg: add vtx" }
                p { text "Dbl-clk vtx: del" }
                p { text "Dbl-clk in: island" }
                p { text "Dbl-clk island: del" }
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
                        let minX = allPoints |> Array.minBy (fun p -> p.X)
                        let maxX = allPoints |> Array.maxBy (fun p -> p.X)
                        let minY = allPoints |> Array.minBy (fun p -> p.Y)
                        let maxY = allPoints |> Array.maxBy (fun p -> p.Y)
                        let minX' = min 0.0 minX.X
                        let minY' = min 0.0 minY.Y
                        let maxX' = max model.LogicalWidth maxX.X
                        let maxY' = max model.LogicalHeight maxY.Y
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
                attr.style "position: relative; width: 100%; max-width: 800px; aspect-ratio: 1 / 1; margin: 20px auto; min-height: 400px; border: 1px solid #e0e0e0; background: #f0f0f0;"
                
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
            }

            // Bottom Action Bar
            if model.UseMapBase && model.IsMapLocked && model.TopographyData.IsSome then
                div {
                    attr.style "display: flex; justify-content: center; gap: 12px; margin-top: 10px; padding-bottom: 30px;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                        on.click (fun _ -> FileManager.exportMapData js model.TopographyData.Value "extents")
                        text "Download Map Extents"
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
