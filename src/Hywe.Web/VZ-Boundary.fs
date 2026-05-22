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
    let controlAndInstructions model dispatch =
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

            // Col 2: Dimensions
            div {
                attr.``class`` "dimension-fields"
                attr.style (match model.UseBoundary with | true -> "" | _ -> "opacity: 0.3; pointer-events: none;")
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
                    let padding = 50.0 * boundScale

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
            controlAndInstructions model dispatch

            match model.PolygonEnabled with
            | true -> polygonEditorSvg model dispatch
            | false ->     div {
                                attr.style "pointer-events:none; opacity:0.5;"
                                polygonEditorSvg model dispatch}
        }
