module Boundary

open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components.Web
open Microsoft.JSInterop
open State
open Types

// ---------- View Templates ----------
type bdrPgn = Template<"""<polygon class="${cs}" points="${pt}" stroke-width="${sw}"/>""">
type bdrCrl = Template<"""<circle class="${cs}" cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
type vtxTxt = Template<"""<text class="${tc}" x="${x}" y="${y}" font-size="${tf}" text-anchor="middle" dominant-baseline="auto">${nm}</text>""">
type ghstVtx = Template<"""<g style="pointer-events: none;"><circle class="ghostVertex" cx="${cx}" cy="${cy}" r="${cr}" fill="none" stroke="#2563eb" stroke-width="2" stroke-dasharray="3,3"/><circle cx="${cx}" cy="${cy}" r="3" fill="#2563eb"/><text x="${cx}" y="${ty}" font-size="${tf}" font-weight="bold" fill="#2563eb" text-anchor="middle">+</text></g>""">
type selHlo = Template<"""<circle class="selectedVertexHalo" cx="${cx}" cy="${cy}" r="${cr}" fill="none" stroke="#2563eb" stroke-width="2.5" stroke-dasharray="3,3" style="pointer-events: none;" />""">

// ---------- Functional UI Combinators ----------
let concatNodes (nodes: seq<Node>) : Node = forEach nodes id

let classList (classes: (string * bool) list) =
    classes
    |> List.choose (fun (cls, enabled) -> match enabled with true -> Some cls | false -> None)
    |> String.concat " "

let toggleBtn label isActive onSelect =
    button {
        attr.``class`` (classList [ "hywe-btn hywe-btn-sm", true; "hywe-btn-dark active", isActive; "hywe-btn-flat", not isActive ])
        on.click (fun _ -> match isActive with false -> onSelect () | true -> ())
        text label
    }

let toggleRow label isEnabled options =
    div {
        attr.``class`` (classList [ "hywe-row", true; "disabled", not isEnabled ])
        span { attr.``class`` "hywe-label"; text label }
        div {
            attr.``class`` "hywe-btn-group"
            options
            |> List.map (fun (optLabel, isActive, onSelect) ->
                toggleBtn optLabel isActive onSelect
            )
            |> concatNodes
        }
    }

let renderNumericInput labelText (value: float) isDisabled msg =
    div {
        attr.``class`` "field-group"
        label { attr.``class`` "hywe-label"; text labelText }
        input {
            attr.``class`` "boundaryInput"
            attr.``type`` "number"
            attr.min "10"
            attr.max "100"
            attr.value (string (System.Math.Round value))
            attr.disabled isDisabled
            on.change (fun ev ->
                let factor = 10.0 // Inputs are disabled in Map Mode, so edits are only manual (factor 10.0)
                match System.Double.TryParse (string ev.Value) with
                | true, v -> msg (v * factor)
                | _ -> ()
            )
        }
    }

// Control panel with transposed horizontal rows for toggles and dimensions
let controlAndInstructions model dispatch (js: IJSRuntime) canUndo canRedo =
    div {
        attr.``class`` "boundary-toolbar"

        // Col 1: Segmented Pill Toggles
        div {
            attr.``class`` "toggle-column"

            let siteOptions = [
                "None", not model.UseBoundary, (fun () -> dispatch (ToggleBoundary false))
                "Boundary", model.UseBoundary, (fun () -> dispatch (ToggleBoundary true))
            ]
            let countOptions = [
                "Relative", not model.UseAbsolute, (fun () -> dispatch (ToggleAbsolute false))
                "Absolute", model.UseAbsolute, (fun () -> dispatch (ToggleAbsolute true))
            ]
            let baseOptions = [
                "None", not model.UseMapBase, (fun () -> dispatch (ToggleMapBase false))
                "Map", model.UseMapBase, (fun () ->
                    dispatch (ToggleMapBase true)
                    js.InvokeVoidAsync("Hymap.init").AsTask() |> ignore
                )
            ]

            toggleRow "Site:" true siteOptions
            toggleRow "Count:" model.UseBoundary countOptions
            toggleRow "Base:" model.UseBoundary baseOptions
        }

        // Col 2: Dimensions & Scale
        div {
            attr.``class`` "dimension-fields"

            let areDimensionsDisabled = not model.UseBoundary || model.UseMapBase
            let dimInputsClass = classList [ "dimension-inputs", true; "disabled", areDimensionsDisabled ]
            let dimInputsStyle = match areDimensionsDisabled with true -> "opacity: 0.3; pointer-events: none;" | false -> ""
            let scaleRatio = match model.UseMapBase with true -> model.MapScale | false -> 1.0

            div {
                attr.``class`` dimInputsClass
                attr.style dimInputsStyle

                renderNumericInput "Width:" model.DisplayWidth areDimensionsDisabled (UpdateLogicalWidth >> dispatch)
                renderNumericInput "Height:" model.DisplayHeight areDimensionsDisabled (UpdateLogicalHeight >> dispatch)

                div {
                    attr.``class`` "field-group"
                    span { attr.``class`` "hywe-label"; text "Scale:" }
                    span { 
                        attr.``class`` "boundary-scale-text"
                        text (sprintf "%d : 1" (int scaleRatio))
                    }
                }
            }
        }

        // Col 3: Actions
        div {
            attr.``class`` "action-column"
            button {
                attr.id "hywe-boundary-guide-btn"
                attr.``type`` "button"
                attr.``class`` (classList [ "boundary-instructions-link", true; "active", model.ShowInstructions ])
                on.click (fun _ -> dispatch ToggleInstructions)
                text "Boundary Guide"
            }
            button {
                attr.``type`` "button"
                attr.``class`` "boundary-instructions-link"
                attr.disabled (not model.UseBoundary || model.UseMapBase || model.IsLocked)
                on.click (fun _ -> dispatch RequestResetBoundary)
                text "Reset Boundary"
            }
            div {
                attr.``class`` "action-trio-row"
                let (lockTitle, lockText) =
                    match model.IsLocked with
                    | true -> "Unlock boundary editor", "Locked"
                    | false -> "Lock boundary editor to prevent accidental changes", "Lock"

                let trioBtn title disabled isActive onClick content =
                    button {
                        attr.``type`` "button"
                        attr.``class`` (classList [ "boundary-trio-btn", true; "active-locked", isActive ])
                        attr.title title
                        attr.disabled disabled
                        on.click (fun _ -> onClick ())
                        text content
                    }

                trioBtn "Undo last action (Ctrl+Z)" (not canUndo || model.IsLocked || not model.UseBoundary) false (fun () -> dispatch UndoBoundary) "Undo"
                trioBtn "Redo last action (Ctrl+Y)" (not canRedo || model.IsLocked || not model.UseBoundary) false (fun () -> dispatch RedoBoundary) "Redo"
                trioBtn lockTitle (not model.UseBoundary) model.IsLocked (fun () -> dispatch ToggleLock) lockText
            }
        }
    }

// Instructions Modal / Card (Text-only, strictly zero icons)
let instructionsModal model dispatch (js: IJSRuntime) =
    match model.ShowInstructions with
    | false -> empty()
    | true ->
        let closeGuide () =
            dispatch ToggleInstructions
            js.InvokeVoidAsync("eval", "var b = document.getElementById('hywe-boundary-guide-btn'); if(b){b.focus({preventScroll:true});}else if(document.activeElement){document.activeElement.blur();}") |> ignore

        let renderGuideSection title items =
            div {
                attr.style "display: flex; flex-direction: column; gap: 8px;"
                div {
                    attr.style "font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 700; color: #777;"
                    text title
                }
                items
                |> List.map (fun (label, desc) ->
                    div {
                        span { attr.style "font-weight: 600; color: #111;"; text label }
                        text desc
                    }
                )
                |> concatNodes
            }

        let toolbarModes = [
            "Site (None / Boundary): ", "None generates unconstrained layouts without perimeter boundaries. Boundary constrains space generation strictly within your custom perimeter and interior islands."
            "Count (Relative / Absolute): ", "Relative dynamically reproportions space area weights to fit the available site area. Absolute allocates exact specified module/hexel counts."
            "Base (None / Map): ", "None uses a blank canvas with manual dimensions (Width, Height, Scale). Map loads an interactive OpenStreetMap underlay with geographic scaling."
        ]

        let canvasControls = [
            "Select vertex: ", "Click or tap vertex (press Delete / Backspace to remove)"
            "Delete vertex: ", "Double-click / double-tap, or press Delete / Backspace when selected"
            "Add vertex: ", "Hover near boundary edge and click / tap"
            "Move island: ", "Drag inside island body"
            "Relocate entrance: ", "Drag entrance marker"
            "Add island: ", "Double-click empty canvas area"
            "Delete island: ", "Double-click inside island body"
        ]

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
                    h4 { attr.style "margin: 0; font-size: 1.05rem; color: #111; font-weight: 600;"; text "Boundary Guide" }
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
                    renderGuideSection "Toolbar Modes" toolbarModes
                    div { attr.style "border-top: 1px solid #eee; margin: 2px 0;" }
                    renderGuideSection "Canvas Controls" canvasControls
                }
            }
        }

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
    let standardPad = State.standardPad w h

    let minX = x - standardPad
    let minY = y - standardPad
    let safeW = max 1.0 (w + 2.0 * standardPad)
    let safeH = max 1.0 (h + 2.0 * standardPad)

    let clientW =
        match model.SvgInfo with
        | Some info when info.ClientW > 0.0 -> info.ClientW
        | _ ->
            try
                match box js with
                | :? Microsoft.JSInterop.IJSInProcessRuntime as inProc ->
                    let cw = inProc.Invoke<float>("getSvgWidth", "polygon-editor-svg")
                    match cw > 0.0 with true -> cw | false -> 400.0
                | _ -> 400.0
            with _ -> 400.0

    let svgScale = safeW / max 200.0 clientW
    let boundRadius = max 4.0 (10.5 * svgScale)
    let boundLabel = max 6.0 (12.5 * svgScale)
    let boundLabelInt = max 1 (int (System.Math.Round boundLabel))
    let bndStWdO = max 1 (int (System.Math.Round (max 1.0 (3.5 * svgScale))))
    let bndStWdI = max 1 (int (System.Math.Round (max 1.0 (2.5 * svgScale))))
    let haloCr = sprintf "%.1f" (boundRadius + 5.0 * svgScale)
    let boundRadiusStr = sprintf "%.1f" boundRadius
    let textYOffset = boundRadius + 6.0 * svgScale

    let viewBoxString = sprintf "%f %f %f %f" minX minY safeW safeH

    let getPointerId ev =
        match box ev with
        | :? PointerEventArgs as pev -> pev.PointerId
        | _ -> 1L

    let invokePointerCapture action ev =
        js.InvokeVoidAsync(action, "polygon-editor-svg", getPointerId ev) |> ignore

    let polygonClass baseClass =
        classList [ baseClass, true; "mapModeOpacity", model.UseMapBase ]

    let renderPolygon baseClass pointsStr strokeWidth =
        bdrPgn()
            .cs(polygonClass baseClass)
            .pt(pointsStr)
            .sw(string strokeWidth)
            .Elt()

    let renderVertex polyIdx vtxIdx (rawPt: Point) (dispPt: Point) vtxClass labelClass =
        let isSelected =
            match model.SelectedVertex with
            | Some sel -> sel.PolyIndex = polyIdx && sel.VertexIndex = vtxIdx
            | None -> false

        let cartX = int (System.Math.Round dispPt.X)
        let cartY = int (System.Math.Round dispPt.Y)
        let xStr = sprintf "%.1f" rawPt.X
        let yStr = sprintf "%.1f" rawPt.Y
        let labelYStr = sprintf "%.1f" (rawPt.Y - textYOffset)
        let labelText = sprintf "(%d, %d)" cartX cartY
        let circleClass = classList [ vtxClass, true; "selected", isSelected ]
        let fillColor = match isSelected with true -> "#2563eb" | false -> "#333"

        concat {
            match isSelected with
            | true -> selHlo().cx(xStr).cy(yStr).cr(haloCr).Elt()
            | false -> ()

            bdrCrl()
                .cs(circleClass)
                .cx(xStr)
                .cy(yStr)
                .cr(boundRadiusStr)
                .cl(fillColor)
                .Elt()

            vtxTxt()
                .tc(labelClass)
                .x(xStr)
                .y(labelYStr)
                .tf(boundLabelInt)
                .nm(labelText)
                .Elt()
        }

    svg {
        attr.id "polygon-editor-svg"
        attr.``class`` (classList [ "polygon-editor-svg", true; "editor-locked", model.IsLocked ])
        attr.tabindex -1
        "data-padding-ratio" => (((2.0 * standardPad) / safeW).ToString(System.Globalization.CultureInfo.InvariantCulture))
        attr.style (match model.UseMapBase with true -> "background-color: transparent;" | false -> "")
        "viewBox" => viewBoxString

        // Pointer events with pointer capture for unbreakable dragging
        on.pointerdown (fun ev ->
            invokePointerCapture "capturePointer" ev
            dispatch (PointerDown ev)
        )
        on.pointerup (fun ev ->
            invokePointerCapture "releasePointer" ev
            dispatch PointerUp
        )
        on.pointermove (fun ev -> dispatch (PointerMove ev))
        on.dblclick (fun ev -> dispatch (DoubleClick ev))
        on.keydown (fun ev -> dispatch (KeyDown ev))

        // Outer polygon
        renderPolygon "outerPolygon" model.OuterPointsStr bndStWdO

        // Islands
        model.IslandPointsStrs
        |> Array.map (fun islandPtsStr ->
            renderPolygon "islandPolygon" islandPtsStr bndStWdI
        )
        |> concatNodes

        // Outer vertices
        model.Outer
        |> Array.mapi (fun i rawPt ->
            renderVertex 0 i rawPt model.DisplayOuter.[i] "outerVertex" "outerVertexLabel"
        )
        |> concatNodes

        // Island vertices
        model.Islands
        |> Array.mapi (fun i isl ->
            isl
            |> Array.mapi (fun j rawPt ->
                renderVertex (i + 1) j rawPt model.DisplayIslands.[i].[j] "islandVertex" "islandVertexLabel"
            )
            |> concatNodes
        )
        |> concatNodes

        // Ghost vertex preview on edge hover
        match model.IsLocked, model.GhostVertex with
        | false, Some ghost ->
            ghstVtx()
                .cx(sprintf "%.1f" ghost.Point.X)
                .cy(sprintf "%.1f" ghost.Point.Y)
                .ty(sprintf "%.1f" (ghost.Point.Y + boundLabel * 0.35))
                .cr(sprintf "%.1f" (boundRadius + 2.0 * svgScale))
                .tf(boundLabelInt)
                .Elt()
        | _ -> ()

        // --- Entry point (Architectural Plan Double Door Symbol) ---
        elt "g" {
            attr.``class`` "entryPoint"
            attr.style (sprintf "transform: translate(%.1fpx, %.1fpx) scale(%.3f);" model.EntryPoint.X model.EntryPoint.Y (1.0 * svgScale))

            // Invisible hit circle for effortless grabbing on touch and mouse
            elt "circle" {
                "cx" => "0"
                "cy" => "0"
                "r" => "18"
                attr.style "fill: transparent; stroke: none; pointer-events: all;"
            }

            // Wall jambs on both sides of the door opening
            elt "line" {
                attr.``class`` "entryJambLeft"
                "x1" => "-12"
                "y1" => "4"
                "x2" => "-9"
                "y2" => "4"
                attr.style "stroke: currentColor; stroke-width: 3; stroke-linecap: square; vector-effect: non-scaling-stroke;"
            }
            elt "line" {
                attr.``class`` "entryJambRight"
                "x1" => "9"
                "y1" => "4"
                "x2" => "12"
                "y2" => "4"
                attr.style "stroke: currentColor; stroke-width: 3; stroke-linecap: square; vector-effect: non-scaling-stroke;"
            }

            // Threshold line connecting the jambs
            elt "line" {
                attr.``class`` "entryThreshold"
                "x1" => "-9"
                "y1" => "4"
                "x2" => "9"
                "y2" => "4"
                attr.style "stroke: currentColor; stroke-width: 1.2; stroke-linecap: butt; opacity: 0.5; vector-effect: non-scaling-stroke;"
            }

            // Left door leaf (open at 90 degrees into space)
            elt "line" {
                attr.``class`` "entryDoorLeafLeft"
                "x1" => "-9"
                "y1" => "4"
                "x2" => "-9"
                "y2" => "-5"
                attr.style "stroke: currentColor; stroke-width: 2.2; stroke-linecap: round; vector-effect: non-scaling-stroke;"
            }

            // Right door leaf (open at 90 degrees into space)
            elt "line" {
                attr.``class`` "entryDoorLeafRight"
                "x1" => "9"
                "y1" => "4"
                "x2" => "9"
                "y2" => "-5"
                attr.style "stroke: currentColor; stroke-width: 2.2; stroke-linecap: round; vector-effect: non-scaling-stroke;"
            }

            // Left door swing arc (quarter circle)
            elt "path" {
                attr.``class`` "entryDoorArcLeft"
                "d" => "M 0,4 A 9 9 0 0 0 -9,-5"
                attr.style "fill: none; stroke: currentColor; stroke-width: 1.4; stroke-dasharray: 2.5,2; opacity: 0.85; vector-effect: non-scaling-stroke;"
            }

            // Right door swing arc (quarter circle)
            elt "path" {
                attr.``class`` "entryDoorArcRight"
                "d" => "M 0,4 A 9 9 0 0 1 9,-5"
                attr.style "fill: none; stroke: currentColor; stroke-width: 1.4; stroke-dasharray: 2.5,2; opacity: 0.85; vector-effect: non-scaling-stroke;"
            }
        }
    }

let view model dispatch (js: IJSRuntime) canUndo canRedo =
    div {
        controlAndInstructions model dispatch js canUndo canRedo

        // Hidden fields for JS interop callback
        let handleMapJsonTrigger elementId onParsed =
            async {
                let! dataStr = js.InvokeAsync<string>("eval", [| box $"document.getElementById('{elementId}').value" |]).AsTask() |> Async.AwaitTask
                match System.String.IsNullOrWhiteSpace dataStr with
                | true -> ()
                | false ->
                    try
                        use doc = System.Text.Json.JsonDocument.Parse dataStr
                        let root = doc.RootElement
                        let w = root.GetProperty("widthMeters").GetDouble()
                        let h = root.GetProperty("heightMeters").GetDouble()
                        onParsed (w, h, dataStr)
                    with ex ->
                        printfn "Error parsing map JSON from %s: %s" elementId ex.Message
            } |> Async.StartImmediate

        input { attr.id "hymap-data"; attr.``type`` "hidden" }
        button {
            attr.id "hymap-trigger"
            attr.style "display:none;"
            on.click (fun _ -> handleMapJsonTrigger "hymap-data" (fun (w, h, raw) -> dispatch (MapTopographyReceived (w, h, raw))))
        }

        // Hidden fields for live dimension updates
        input { attr.id "hymap-live-data"; attr.``type`` "hidden" }
        button {
            attr.id "hymap-live-trigger"
            attr.style "display:none;"
            on.click (fun _ -> handleMapJsonTrigger "hymap-live-data" (fun (w, h, _) -> dispatch (UpdateLogicalDimensions (w, h))))
        }

        // Map and SVG Container
        let containerAspectRatio =
            match model.UseBoundary, model.UseMapBase, model.LogicalHeight > 0.0 with
            | true, false, true ->
                let pad = State.standardPad model.LogicalWidth model.LogicalHeight
                sprintf "%.6f" ((model.LogicalWidth + 2.0 * pad) / (model.LogicalHeight + 2.0 * pad))
            | _ -> "1"

        let containerStyle =
            match model.UseBoundary, model.UseMapBase with
            | false, false -> "aspect-ratio: 1 / 1; border: none; background: transparent;"
            | _ -> sprintf "aspect-ratio: %s; border: 1px solid #e0e0e0; background: #f0f0f0;" containerAspectRatio

        let hymapPointerEvents =
            match model.UseMapBase, model.IsMapLocked with
            | false, _ -> "visibility: hidden;"
            | true, true -> "pointer-events: none;"
            | true, false -> "pointer-events: auto;"

        let svgPointerEvents =
            match model.UseMapBase, model.IsMapLocked with
            | true, false -> "none"
            | _ -> "auto"

        div {
            attr.key "map-and-svg-container"
            attr.id "map-and-svg-container"
            attr.``class`` "boundary-svg-container"
            attr.style containerStyle
            
            // Hymap Layer Wrapper (Handles dynamic state so hymap-container itself is strictly static and NEVER re-rendered by Blazor)
            div {
                attr.key "hymap-wrapper"
                attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; z-index: 0; %s" hymapPointerEvents)
                     
                // Hymap Layer (Native) - Absolutely no children or dynamic attributes to ensure Leaflet DOM is fully preserved
                div {
                    attr.key "hymap-container"
                    attr.id "hymap-container"
                    attr.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 0;"
                }
            }

            // SVG Editor Layer
            let editorContent = polygonEditorSvg model dispatch js
            div {
                attr.style (sprintf "position: absolute; top: 0; left: 0; width: 100%%; height: 100%%; z-index: 1; pointer-events: %s;" svgPointerEvents)
                match model.PolygonEnabled with
                | true -> editorContent
                | false ->
                    div {
                        attr.style "pointer-events:none; opacity:0.5; width: 100%; height: 100%;"
                        editorContent
                    }
            }

            // Lock Icon Overlay (Top Right)
            match model.UseMapBase with
            | false -> ()
            | true ->
                let (lockTitle, iconSvg) =
                    match model.IsMapLocked with
                    | true ->
                        "Map is locked. Click to unlock",
                        """<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="#e63946" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 10 0v4"></path></svg>"""
                    | false ->
                        "Map is active. Click to lock",
                        """<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="#333" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect><path d="M7 11V7a5 5 0 0 1 9.9-1"></path></svg>"""

                div {
                    attr.``class`` "hymap-lock-btn"
                    attr.title lockTitle
                    on.click (fun _ ->
                        let newState = not model.IsMapLocked
                        dispatch (ToggleMapLock newState)
                        let jsAction = match newState with true -> "Hymap.lockMap" | false -> "Hymap.unlockMap"
                        js.InvokeVoidAsync(jsAction).AsTask() |> ignore
                    )
                    rawHtml iconSvg
                }
        }

        // Bottom Action Bar
        match model.UseMapBase, model.IsMapLocked, model.TopographyData with
        | true, true, Some topoData ->
            div {
                attr.style "display: flex; justify-content: center; gap: 12px; margin-top: 10px; padding-bottom: 30px;"
                button {
                    attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                    on.click (fun _ -> FileManager.exportMapImage js)
                    text "Download Map Image"
                }
                button {
                    attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                    on.click (fun _ -> FileManager.exportMapData js topoData "terrain")
                    text "Download Terrain Grid"
                }
            }
        | _ -> empty()

        // Instructions Modal / Card (rendered cleanly at boundary view root level)
        instructionsModal model dispatch js
    }
