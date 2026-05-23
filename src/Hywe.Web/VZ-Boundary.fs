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
        let renderNumericInput labelText value msg isHeight =
            div {
                attr.``class`` "field-group"
                label { text labelText }
                input {
                    attr.``class`` "boundaryInput"
                    attr.``type`` "number"
                    let factor = 10.0
                    attr.value (string (System.Math.Round(value / factor)))
                    attr.disabled (not model.UseBoundary)
                    on.change (fun ev ->
                        match System.Double.TryParse (string ev.Value) with
                        | (true, v) -> dispatch (msg v)
                        | _ -> ()
                    )
                }
            }

        div {
            attr.``class`` "control-and-instructions"
            attr.style "display: flex; flex-flow: row nowrap; align-items: center; justify-content: center; gap: 12px; width: 100%;"

            // Col 1: Small Toggles
            div {
                attr.``class`` "toggle-column"
                
                div {
                    attr.``class`` "toggle-group"
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.UseBoundary with | false -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> dispatch (ToggleBoundary false))
                        text "Unbound"
                    }
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.UseBoundary with | true -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> dispatch (ToggleBoundary true))
                        text "Boundary"
                    }
                }

                div {
                    attr.``class`` "toggle-group"
                    attr.style (match model.UseBoundary with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.UseAbsolute with | false -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> dispatch (ToggleAbsolute false))
                        text "Relative"
                    }
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.UseAbsolute with | true -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> dispatch (ToggleAbsolute true))
                        text "Absolute"
                    }
                }
            }

            // Col 1.5: Map Toggles
            div {
                attr.``class`` "toggle-column"
                
                div {
                    attr.``class`` "toggle-group"
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.UseMapBase with | false -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> dispatch (ToggleMapBase false))
                        text "Manual"
                    }
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.UseMapBase with | true -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> dispatch (ToggleMapBase true))
                        text "Map Base"
                    }
                }

                div {
                    attr.``class`` "toggle-group"
                    attr.style (match model.UseMapBase with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.IsMapLocked with | false -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> 
                            dispatch (ToggleMapLock false)
                            js.InvokeVoidAsync("eval", [| box "var iframe = document.getElementById('hymap-iframe'); if (iframe) iframe.contentWindow.postMessage({ type: 'UNLOCK_MAP' }, '*');" |]).AsTask() |> ignore
                        )
                        text "Unlock"
                    }
                    button {
                        attr.``class`` ("hywe-btn hywe-btn-sm " + (match model.IsMapLocked with | true -> "hywe-btn-dark active toggle-btn" | _ -> "hywe-btn-light toggle-btn"))
                        on.click (fun _ -> 
                            dispatch (ToggleMapLock true)
                            js.InvokeVoidAsync("eval", [| box "var iframe = document.getElementById('hymap-iframe'); if (iframe) iframe.contentWindow.postMessage({ type: 'LOCK_MAP' }, '*');" |]).AsTask() |> ignore
                        )
                        text "Lock"
                    }
                }
            }

            // Col 2: Dimensions
            div {
                attr.``class`` "dimension-fields"
                attr.style (match model.UseMapBase with | true -> "display: none;" | false -> (match model.UseBoundary with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;"))
                renderNumericInput "Width:" model.LogicalWidth UpdateLogicalWidth false
                renderNumericInput "Height:" model.LogicalHeight UpdateLogicalHeight true
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
                    match model.UseMapBase with
                    | true -> 
                        // Strictly lock to Logical Width and Height to match the map exactly
                        (0.0, 0.0, model.LogicalWidth, model.LogicalHeight)
                    | false ->
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

                let viewBoxString =
                    let (x, y, w, h) = boundingBoxWithLogical
                    let padding = match model.UseMapBase with | true -> 0.0 | false -> 50.0 * boundScale

                    // Allow min-x / min-y to go negative
                    let minX = x - padding
                    let minY = y - padding

                    // Ensure width / height never negative or zero
                    let safeW = max 1.0 (w + 2.0 * padding)
                    let safeH = max 1.0 (h + 2.0 * padding)

                    sprintf "%f %f %f %f" minX minY safeW safeH

                svg {
                attr.id "polygon-editor-svg"
                attr.``class`` "polygon-editor-svg"
                attr.style (match model.UseMapBase with | true -> "margin: 0; background-color: transparent; width: 100%; height: 100%;" | false -> "")
                "viewBox" => viewBoxString

                // Pointer events
                on.pointerdown (fun ev -> dispatch (PointerDown ev))
                on.pointerup (fun _ -> dispatch PointerUp)
                on.pointermove (fun ev -> dispatch (PointerMove ev))
                on.dblclick (fun ev -> dispatch (DoubleClick ev))

                // Outer polygon
                bdrPgn()
                    .cs("outerPolygon")
                    .pt(model.OuterPointsStr)
                    .sw(string bndStWdO)
                    .Elt()

                // Islands
                for i = 0 to model.Islands.Length - 1 do
                    bdrPgn()
                        .cs("islandPolygon")
                        .pt(model.IslandPointsStrs.[i])
                        .sw(string bndStWdI)
                        .Elt()

                // Outer vertices
                for i = 0 to model.Outer.Length - 1 do
                    let pt = model.Outer.[i]
                    let id = sprintf "outerVertex-%d" i
                    let cartX = int (System.Math.Round( pt.X / 10.0))
                    let cartY = int (System.Math.Round((model.LogicalHeight - pt.Y) / 10.0))
                    bdrCrl()
                        .cs("outerVertex")
                        .cx(sprintf "%.1f" pt.X)
                        .cy(sprintf "%.1f" pt.Y)
                        .cr(string boundRadius)
                        .cl("#333")
                        .Elt()

                    bdcrPh()
                        .pathid(id)
                        .sx($"{pt.X}")
                        .sy($"{pt.Y + float bndTxtRr}")
                        .r($"{bndTxtRr}")
                        .ex($"{pt.X}")
                        .ey($"{pt.Y - float bndTxtRr}")
                        .Elt()

                    bdcrTx()
                        .pth(id)
                        .tc("outerVertexLabel")
                        .tf(boundLabel)
                        .nm(sprintf "(%d, %d)" cartX cartY)
                        .Elt()

                // Island vertices
                for islandIdx in 0 .. model.Islands.Length - 1 do
                    let island = model.Islands.[islandIdx]
                    for vertexIdx in 0 .. island.Length - 1 do
                        let pt = island.[vertexIdx]
                        let id = sprintf "islandVertex-%d-%d" islandIdx vertexIdx
                        let cartX = int (System.Math.Round( pt.X / 10.0))
                        let cartY = int (System.Math.Round((model.LogicalHeight - pt.Y) / 10.0))

                        bdrCrl()
                            .cs("islandVertex")
                            .cx(sprintf "%.1f" pt.X)
                            .cy(sprintf "%.1f" pt.Y)
                            .cr(string boundRadius)
                            .cl("#333")
                            .Elt()

                        bdcrPh()
                            .pathid(id)
                            .sx($"{pt.X}")
                            .sy($"{pt.Y + float bndTxtRr}")
                            .r($"{bndTxtRr}")
                            .ex($"{pt.X}")
                            .ey($"{pt.Y - float bndTxtRr}")
                            .Elt()

                        bdcrTx()
                            .pth(id) 
                            .tc("islandVertexLabel")
                            .tf(boundLabel)
                            .nm(sprintf "(%d, %d)" cartX cartY)
                            .Elt()

                // --- Entry point ---
                // Scaled down version of the entry icon.
                // Using SVG transform (translate + scale) allows us to move the icon
                // without recalculating its internal coordinates or constructing strings every frame.
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
                                let pts = root.GetProperty("points").GetRawText()
                                dispatch (MapTopographyReceived (w, h, pts))
                            with ex ->
                                printfn "Error parsing topography: %s" ex.Message
                    } |> Async.StartImmediate
                )
            }

            // Map and SVG Container
            div {
                attr.style "position: relative; width: 100%; max-width: 800px; aspect-ratio: 1 / 1; margin: 20px auto; min-height: 400px; border: 1px solid #e0e0e0; background: #f0f0f0;"
                
                // Hymap Iframe Layer
                match model.UseMapBase with
                | true ->
                    iframe {
                        attr.src "http://localhost:8080" // Hosted locally for testing, or replace with github pages URL
                        attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; border: none; z-index: 0; pointer-events: %s;" (match model.IsMapLocked with | true -> "none" | false -> "auto"))
                        attr.id "hymap-iframe"
                    }
                | false -> ()

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
        }
