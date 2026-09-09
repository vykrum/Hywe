namespace Hywe.Site

open System
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components.Web
open System.Text.Json
open System.Text.Json.Nodes

module State =

    // ---------- Initial Constants ----------
    let initBound = 300.0, 300.0
    let initWidth = fst initBound
    let initHeight = snd initBound
    let initEntry = { X = 150.0; Y = 50.0 }
    let initRadius = 6
    let minBound = 100.0
    let maxBound = 1000.0
    let initOuter = [| { X = 0.0; Y = 0.0 }
                       { X = initWidth; Y = 0.0 }
                       { X = initWidth; Y = initHeight }
                       { X = 0.0; Y = initHeight } |]
    let initIslands = Array.empty<Point[]>

    // ---------- Import helpers ----------

    let parsePoint (multiplier: float) (s: string) : Result<Point, string> =
        match s.Split(',', StringSplitOptions.RemoveEmptyEntries) with
        | [| x; y |] ->
            match Double.TryParse x, Double.TryParse y with
            | (true, xv), (true, yv) ->
                Ok { X = xv * multiplier; Y = yv * multiplier }
            | _ -> Error $"Invalid point: {s}"
        | _ -> Error $"Invalid point format: {s}"

    let sequenceResults (arr: Result<'a,'e> array) : Result<'a array,'e> =
        let rec loop i acc =
            match i < arr.Length with
            | false -> Ok (List.toArray (List.rev acc))
            | true ->
                match arr.[i] with
                | Ok v -> loop (i + 1) (v :: acc)
                | Error e -> Error e
        loop 0 []

    let parsePoly (multiplier: float) (s: string) : Result<Point[], string> =
        s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.chunkBySize 2
        |> Array.map (fun a -> parsePoint multiplier (String.concat "," a))
        |> sequenceResults

    let parseIslands (multiplier: float) (s: string) : Result<Point[][], string> =
        match String.IsNullOrWhiteSpace s with
        | true -> Ok [||]
        | false ->
            s.Split('-', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun isl ->
                isl.Split(',', StringSplitOptions.RemoveEmptyEntries)
                |> Array.chunkBySize 2
                |> Array.map (fun a -> parsePoint multiplier (String.concat "," a))
                |> sequenceResults)
            |> sequenceResults

    // ---------- Utility functions ----------
    let clampPt (model: PolygonEditorModel) (pt: Point) =
        {
            X = max 0.0 (min model.LogicalWidth pt.X)
            Y = max 0.0 (min model.LogicalHeight pt.Y)
        }

    let snapshot (model: PolygonEditorModel) : PolygonEditorModel =
        { model with Dragging = None; DraggingIsland = None; DragOffset = None; GhostVertex = None }

    let polyToSvgPoints (poly: Point[]) =
        poly |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " "

    // Core logic for UI scaling (Unified 10x ratio for all modes)
    let formatBoundaryValue (w: float) (value: float) (isMapBase: bool) =
        value / 10.0

    let updateDisplayFields (model: PolygonEditorModel) =
        let scale v = formatBoundaryValue model.LogicalWidth v model.UseMapBase
        { model with
            DisplayWidth = scale model.LogicalWidth
            DisplayHeight = scale model.LogicalHeight
            DisplayOuter = model.Outer |> Array.map (fun pt -> { X = scale pt.X; Y = scale (model.LogicalHeight - pt.Y) })
            DisplayIslands = model.Islands |> Array.map (Array.map (fun pt -> { X = scale pt.X; Y = scale (model.LogicalHeight - pt.Y) }))
        }

    /// Refreshes cached strings and UI fields to avoid expensive re-formatting during every frame
    let refreshCachedStrings (model: PolygonEditorModel) =
        let displayUpdated = updateDisplayFields model
        { displayUpdated with
            OuterPointsStr = polyToSvgPoints displayUpdated.Outer
            IslandPointsStrs = displayUpdated.Islands |> Array.map polyToSvgPoints }

    // ---------- JS interop helpers ----------
    let getSvgInfo (js: IJSRuntime) =
        async {
            let! el = js.InvokeAsync<JsonElement>("getSvgInfo", [| box "polygon-editor-svg" |]).AsTask() |> Async.AwaitTask
            let left = el.GetProperty("left").GetDouble()
            let top = el.GetProperty("top").GetDouble()
            let width = el.GetProperty("width").GetDouble()
            let height = el.GetProperty("height").GetDouble()
            let vbx = el.GetProperty("viewBoxX").GetDouble()
            let vby = el.GetProperty("viewBoxY").GetDouble()
            let vbw = el.GetProperty("viewBoxW").GetDouble()
            let vbh = el.GetProperty("viewBoxH").GetDouble()
            return { ViewBoxX = vbx; ViewBoxY = vby; ViewBoxW = vbw; ViewBoxH = vbh; ClientLeft = left; ClientTop = top; ClientW = width; ClientH = height }
        }

    let toSvgCoordsFromInfo (info: SvgInfo) (clientX: float) (clientY: float) =
        let x = info.ViewBoxX + (clientX - info.ClientLeft) * info.ViewBoxW / info.ClientW
        let y = info.ViewBoxY + (clientY - info.ClientTop) * info.ViewBoxH / info.ClientH
        { X = x; Y = y }

    // Occasional precise mapping using getSvgCoords (for double click & contextmenu)
    let toSvgCoords (js: IJSRuntime) (ev: MouseEventArgs) : Async<Point> =
        async {
            let! result =
                js.InvokeAsync<JsonElement>("getSvgCoords", [| box "polygon-editor-svg"; box ev.ClientX; box ev.ClientY |]).AsTask()
                |> Async.AwaitTask
            return { X = result.GetProperty("x").GetDouble(); Y = result.GetProperty("y").GetDouble() }
        }

    let ensureEntryWithin
        (outer: Point[])
        (islands: Point[][])
        (entry: Point) =

        match Geometry.isEntryPointValid outer islands entry with
        | true -> entry
        | false -> Geometry.closestValidEntryPoint outer islands

    /// Return (outer, islands, absolute, entry, width, height, elevation, baseStr)
    let exportPolygonStrings (model: PolygonEditorModel) : string * string * string * string * int * int * int * string =
        let divisor = 10.0
        let fmtPoint (p: Point) = sprintf "%d,%d" (int (System.Math.Floor((p.X + 0.001) / divisor))) (int (System.Math.Floor((p.Y + 0.001) / divisor)))

        let outer =
            model.Outer
            |> Array.map fmtPoint
            |> String.concat ","

        let islands =
            model.Islands
            |> Array.map (fun isl -> isl |> Array.map fmtPoint |> String.concat ",")
            |> String.concat "-"

        let entry = fmtPoint (ensureEntryWithin model.Outer model.Islands model.EntryPoint)
        let absolute = 
            match model.UseMapBase, model.UseAbsolute with
            | true, _ -> "2"
            | false, true -> "1"
            | false, false -> "0"
        let w = max 1 (int (System.Math.Floor((model.LogicalWidth + 0.001) / 10.0)))
        let h = max 1 (int (System.Math.Floor((model.LogicalHeight + 0.001) / 10.0)))
        outer, islands, absolute, entry, w, h, model.Elevation, model.BaseStr

    // ---------- Import function ----------
    let importPolygonStrings
        (outerStr: string)
        (islandsStr: string)
        (absStr: string)
        (entryStr: string)
        (w: int)
        (h: int)
        (model: PolygonEditorModel)
        : Result<PolygonEditorModel, string> =

        let logicalWidth = match w <= 0 with | true -> initWidth | false -> float (max 10 w) * 10.0
        let logicalHeight = match h <= 0 with | true -> initHeight | false -> float (max 10 h) * 10.0

        let multiplier = match absStr with | "2" -> 2.0 | _ -> 10.0

        parsePoly multiplier outerStr
        |> Result.bind (fun outer ->
            parseIslands multiplier islandsStr
            |> Result.bind (fun islands ->
                parsePoint multiplier entryStr
                |> Result.map (fun entry ->
                    let width = logicalWidth
                    let height = logicalHeight
                    
                    let outer = match Array.isEmpty outer with | true -> initOuter | false -> outer
                    let fixedEntry = ensureEntryWithin outer islands entry

                    { model with
                        Outer = outer
                        Islands = islands
                        EntryPoint = fixedEntry
                        LogicalWidth = width
                        LogicalHeight = height
                        UseAbsolute = (absStr = "1")
                        UseBoundary = (absStr <> "1")
                        UseMapBase = (absStr = "2")
                        PolygonEnabled = (absStr <> "1")
                        Dragging = None
                        DragOffset = None
                        SvgInfo = None }
                    |> refreshCachedStrings
                )
            )
        )


    // ---------- Initial Model ----------

    let initModel =
        {
            UseBoundary = false
            UseAbsolute = true
            PolygonEnabled = false
            UseMapBase = false
            IsMapLocked = false
            TopographyData = None
            LogicalWidth = initWidth
            LogicalHeight = initHeight
            Elevation = 0
            BaseStr = ""
            Outer = initOuter
            Islands = initIslands
            Dragging = None
            DraggingIsland = None
            DragOffset = None
            SvgInfo = None
            LastMoveMs = None
            VertexRadius = initRadius
            EntryPoint = initEntry
            DraggingEntry = false
            GhostVertex = None
            OuterPointsStr = ""
            IslandPointsStrs = [||]
            DisplayWidth = 0.0
            DisplayHeight = 0.0
            DisplayOuter = [||]
            DisplayIslands = [||]
            MapScale = 1.0
        }
        |> refreshCachedStrings

    let handlePointerUp (model: PolygonEditorModel) : PolygonEditorModel =
        { model with Dragging = None; DraggingEntry = false; DraggingIsland = None; DragOffset = None; LastMoveMs = None }

    let commitGhost (ghost: GhostCandidate) (model: PolygonEditorModel) : PolygonEditorModel option =
        match ghost.PolyIndex = 0 with
        | true ->
            let insertIdx = ghost.EdgeIndex + 1
            let newOuter =
                Array.append
                    (Array.append model.Outer.[0 .. ghost.EdgeIndex] [| ghost.Point |])
                    model.Outer.[insertIdx ..]
            let ok =
                not (Geometry.polygonSelfIntersects newOuter) &&
                not (model.Islands |> Array.exists (fun island -> Geometry.polygonsIntersect newOuter island)) &&
                (model.Islands |> Array.forall (fun island -> Geometry.isPolygonInside newOuter island))
            match ok with
            | true ->
                let updated = { model with Outer = newOuter; GhostVertex = None }
                Some (updated |> refreshCachedStrings)
            | false -> None
        | false ->
            let islandIdx = ghost.PolyIndex - 1
            let island = model.Islands.[islandIdx]
            let insertIdx = ghost.EdgeIndex + 1
            let newIsland =
                Array.append
                    (Array.append island.[0 .. ghost.EdgeIndex] [| ghost.Point |])
                    island.[insertIdx ..]
            let ok =
                Geometry.isPolygonInside model.Outer newIsland &&
                not (Geometry.polygonSelfIntersects newIsland) &&
                not (model.Islands |> Array.mapi (fun i isl -> i, isl) |> Array.exists (fun (i, isl) -> i <> islandIdx && Geometry.polygonsIntersect newIsland isl))
            match ok with
            | true ->
                let newIslands =
                    model.Islands
                    |> Array.mapi (fun i isl -> match i = islandIdx with true -> newIsland | false -> isl)
                let updated = { model with Islands = newIslands; GhostVertex = None }
                Some (updated |> refreshCachedStrings)
            | false -> None

    let handlePointerMove (ev: MouseEventArgs) (model: PolygonEditorModel) : PolygonEditorModel =
        match model.PolygonEnabled with
        | false -> model
        | true ->
            let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
            match model.LastMoveMs with
            | Some last when nowMs - last < 16.0 -> model
            | _ -> 
                match model.Dragging, model.DraggingEntry, model.DraggingIsland, model.DragOffset, model.SvgInfo with
                // -------------------------------------------------------
                // 1) Dragging the entry point
                // -------------------------------------------------------
                | None, true, None, Some offset, Some info ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let newEntry = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

                    match Geometry.isEntryPointValid model.Outer model.Islands newEntry with
                    | true -> { model with EntryPoint = newEntry; LastMoveMs = Some nowMs; GhostVertex = None }
                    | false -> { model with LastMoveMs = Some nowMs; GhostVertex = None }

                // -------------------------------------------------------
                // 2) Dragging a polygon vertex (outer or island)
                // -------------------------------------------------------
                | Some drag, false, None, Some offset, Some info ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let newPt = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

                    let proposedModel = 
                        match drag.PolyIndex = 0 with
                        | true ->
                            let newOuter =
                                model.Outer
                                |> Array.mapi (fun i pt -> match i = drag.VertexIndex with true -> newPt | false -> pt)
                            { model with Outer = newOuter }
                        | false ->
                            let islandIdx = drag.PolyIndex - 1
                            let newIslands =
                                model.Islands
                                |> Array.mapi (fun i island ->
                                    match i = islandIdx with
                                    | true -> island |> Array.mapi (fun j pt -> match j = drag.VertexIndex with true -> newPt | false -> pt)
                                    | false -> island)
                            { model with Islands = newIslands }

                    match Geometry.isConfigurationValid proposedModel.Outer proposedModel.Islands with
                    | false -> { model with LastMoveMs = Some nowMs; GhostVertex = None }
                    | true ->
                        let isEntryValid = Geometry.isEntryPointValid proposedModel.Outer proposedModel.Islands proposedModel.EntryPoint
                        let finalModel = 
                            match isEntryValid with
                            | true -> { proposedModel with LastMoveMs = Some nowMs; GhostVertex = None }
                            | false -> 
                                 { proposedModel with 
                                     EntryPoint = Geometry.closestValidEntryPoint proposedModel.Outer proposedModel.Islands
                                     LastMoveMs = Some nowMs
                                     GhostVertex = None }
                        finalModel |> refreshCachedStrings

                // -------------------------------------------------------
                // 3) Dragging an entire island
                // -------------------------------------------------------
                | None, false, Some islIdx, Some startPt, Some info ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let dx = svgPt.X - startPt.X
                    let dy = svgPt.Y - startPt.Y

                    let targetIsland = model.Islands.[islIdx]
                    let translatedIsland = targetIsland |> Array.map (fun pt -> { X = pt.X + dx; Y = pt.Y + dy })

                    let insideOuter = Geometry.isPolygonInside model.Outer translatedIsland
                    let noIntersectionWithOthers =
                        model.Islands
                        |> Array.mapi (fun i isl -> i, isl)
                        |> Array.forall (fun (i, isl) -> i = islIdx || not (Geometry.polygonsIntersect translatedIsland isl))
                    let notEncloseEntry = not (Geometry.isInsidePolygon translatedIsland model.EntryPoint)

                    match insideOuter && noIntersectionWithOthers && notEncloseEntry with
                    | true ->
                        let newIslands =
                            model.Islands
                            |> Array.mapi (fun i isl -> match i = islIdx with true -> translatedIsland | false -> isl)
                        let updated = { model with Islands = newIslands; DragOffset = Some svgPt; LastMoveMs = Some nowMs; GhostVertex = None }
                        updated |> refreshCachedStrings
                    | false ->
                        { model with LastMoveMs = Some nowMs; GhostVertex = None }

                // -------------------------------------------------------
                // 4) Hovering (not dragging): Detect closest edge for ghost vertex preview
                // -------------------------------------------------------
                | None, false, None, _, Some info ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let boundScale = match model.LogicalWidth with | w when w <> fst initBound -> w / fst initBound | _ -> 1.0
                    let ghostThreshold = max 15.0 (20.0 * boundScale)
                    let ghostCandidate = Geometry.findClosestEdge svgPt ghostThreshold model.Outer model.Islands
                    { model with GhostVertex = ghostCandidate; LastMoveMs = Some nowMs }

                | _ -> model

    let updateSync (msg: PolygonEditorMessage) (model: PolygonEditorModel) : PolygonEditorModel option =
        match msg with
        | PointerMove ev -> Some (handlePointerMove ev model)
        | PointerUp -> Some (handlePointerUp model)
        | CommitGhostVertex ->
            match model.GhostVertex with
            | Some ghost -> commitGhost ghost model
            | None -> None
        | _ -> None

    // ---------- Update ----------
    let update (js: IJSRuntime) (msg: PolygonEditorMessage) (model: PolygonEditorModel) : Async<PolygonEditorModel> =
        match msg with
        | ToggleBoundary isChecked ->
            async { 
                let newOuter = match isChecked && model.Outer.Length < 3 with | true -> initOuter | false -> model.Outer
                return
                    { model with
                        UseBoundary = isChecked
                        PolygonEnabled = isChecked
                        Outer = newOuter
                        UseAbsolute = match isChecked with
                                      | true -> false
                                      | false -> true
                    } |> refreshCachedStrings
            }

        | ToggleAbsolute isChecked -> async{ 
                                                let updated = { model with UseAbsolute = isChecked }
                                                return updated
                                            }

        | ToggleMapBase isChecked -> 
            async {
                let updated = { model with UseMapBase = isChecked; IsMapLocked = false }
                match isChecked with
                | true -> return updated
                | false ->
                    // Switching back to No Base. The map scale divisor is lost, so multiplying by 10
                    // is inaccurate if the user zoomed heavily.
                    // Instead, we ensure "default settings and scaling" by normalizing 
                    // the polygon's logical width back to exactly `initWidth` (300.0).
                    let scale = initWidth / updated.LogicalWidth
                    let safeW = initWidth
                    let safeH = updated.LogicalHeight * scale
                    
                    let newOuter = updated.Outer |> Array.map (fun pt -> { pt with X = pt.X * scale; Y = pt.Y * scale })
                    let newIslands = updated.Islands |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scale; Y = pt.Y * scale }))
                    
                    return { updated with LogicalWidth = safeW; LogicalHeight = safeH; Outer = newOuter; Islands = newIslands } |> refreshCachedStrings
            }

        | ToggleMapLock isLocked -> async {
                                                let updated = { model with IsMapLocked = isLocked }
                                                return updated
                                            }

        | MapTopographyReceived (w, h, topoJson) -> async {
                                                // Hymap sends exact physical width/height in Meters.
                                                let rec findScaleFactor width height factor =
                                                    match width <= 100.0 && height <= 100.0 with
                                                    | true -> factor
                                                    | false -> findScaleFactor (width / 2.0) (height / 2.0) (factor * 2.0)
                                                let sf = findScaleFactor w h 1.0
                                                
                                                let floorW = System.Math.Floor((w / sf) + 0.001)
                                                let floorH = System.Math.Floor((h / sf) + 0.001)

                                                let hyweInternalScale = 10.0
                                                let scaledW = floorW * hyweInternalScale
                                                let scaledH = floorH * hyweInternalScale

                                                let (|ParsedJson|_|) (json: string) =
                                                    try Some (JsonNode.Parse(json))
                                                    with _ -> None

                                                // Scale topography data X and Y points to match internal decimeter scale
                                                let scaledTopoJson = 
                                                    match topoJson with
                                                    | ParsedJson (:? JsonArray as arr) ->
                                                        let newArr = JsonArray()
                                                        arr |> Seq.iter (fun item ->
                                                            match item with
                                                            | :? JsonObject as obj ->
                                                                let cloned = obj.DeepClone().AsObject()
                                                                let x = cloned.["X"].GetValue<float>()
                                                                let y = cloned.["Y"].GetValue<float>()
                                                                cloned.["X"] <- JsonValue.Create((x / sf) * hyweInternalScale)
                                                                cloned.["Y"] <- JsonValue.Create((y / sf) * hyweInternalScale)
                                                                newArr.Add(cloned)
                                                            | other -> newArr.Add(match other with null -> null | x -> x.DeepClone())
                                                        )
                                                        newArr.ToJsonString()
                                                    | _ -> topoJson

                                                let safeW = max 1.0 (scaledW)
                                                let safeH = max 1.0 (scaledH) 
                                                
                                                // Scale existing points to the new map bounds (similar to UpdateLogicalWidth)
                                                let scaleX = match model.LogicalWidth <= 0.0 with | true -> 1.0 | false -> safeW / model.LogicalWidth
                                                let scaleY = match model.LogicalHeight <= 0.0 with | true -> 1.0 | false -> safeH / model.LogicalHeight
                                                
                                                let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY })
                                                let newIslands = model.Islands |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY }))
                                                
                                                let updated = { model with LogicalWidth = safeW; LogicalHeight = safeH; Outer = newOuter; Islands = newIslands; TopographyData = Some scaledTopoJson; BaseStr = scaledTopoJson; MapScale = sf * 10.0 }
                                                return updated |> refreshCachedStrings
                                            }

        | UpdateLogicalWidth newW -> async {
            let oldW = model.LogicalWidth
            let oldH = model.LogicalHeight

            let safeW = match newW <= 0.0 with | true -> initWidth | false -> min maxBound (max minBound newW)
            
            let safeH = oldH

            let scaleX = match oldW <= 0.0 with | true -> 1.0 | false -> safeW / oldW
            let scaleY = match oldH <= 0.0 with | true -> 1.0 | false -> safeH / oldH

            let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY })
            let newIslands =
                model.Islands
                |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY }))
            let updated = { model with LogicalWidth = safeW; LogicalHeight = safeH; Outer = newOuter; Islands = newIslands }
            return updated |> refreshCachedStrings
            }

        | UpdateLogicalHeight newH -> async {
            let oldW = model.LogicalWidth
            let oldH = model.LogicalHeight

            let safeH = match newH <= 0.0 with | true -> initHeight | false -> min maxBound (max minBound newH)

            let safeW = oldW

            let scaleX = match oldW <= 0.0 with | true -> 1.0 | false -> safeW / oldW
            let scaleY = match oldH <= 0.0 with | true -> 1.0 | false -> safeH / oldH

            let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY })
            let newIslands =
                model.Islands
                |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY }))

            let updated = { model with LogicalWidth = safeW; LogicalHeight = safeH; Outer = newOuter; Islands = newIslands }
            return updated |> refreshCachedStrings
            }

        | UpdateLogicalDimensions (newW, newH) -> async {
            // If we are in Map Base mode, the geographic dimensions are managed by the map bounds.
            // We should ONLY update the LogicalWidth/Height to match the map viewport, 
            // but NEVER scale the polygon's SVG coordinates. If the map is locked, we don't 
            // even update the LogicalWidth/Height because the bounds are locked geographically.
            match model.UseMapBase && model.IsMapLocked with
            | true -> return model
            | false ->
                let rec findScaleFactor w h factor =
                    match w <= 100.0 && h <= 100.0 with
                    | true -> factor
                    | false -> findScaleFactor (w / 2.0) (h / 2.0) (factor * 2.0)
                let sf = findScaleFactor newW newH 1.0
                
                let scaledW = System.Math.Floor((newW / sf) + 0.001)
                let scaledH = System.Math.Floor((newH / sf) + 0.001)

                let hyweInternalScale = 10.0
                let safeW = match scaledW <= 0.0 with | true -> 1.0 | false -> scaledW * hyweInternalScale
                let safeH = match scaledH <= 0.0 with | true -> 1.0 | false -> scaledH * hyweInternalScale
                
                let oldW = model.LogicalWidth
                let oldH = model.LogicalHeight
                let scaleX = match oldW <= 0.0 with | true -> 1.0 | false -> safeW / oldW
                let scaleY = match oldH <= 0.0 with | true -> 1.0 | false -> safeH / oldH

                let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY })
                let newIslands =
                    model.Islands
                    |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scaleX; Y = pt.Y * scaleY }))

                let mapScale = match newW <= 100.0 && newH <= 100.0 with | true -> sf | false -> sf * 10.0
                let updated = { model with LogicalWidth = safeW; LogicalHeight = safeH; Outer = newOuter; Islands = newIslands; MapScale = mapScale }
                return updated |> refreshCachedStrings
            }

        | PointerDown ev ->
            async {
                match model.PolygonEnabled with
                | false -> return model
                | true ->
                    // Get svg transform info once per drag (cheap JS call)
                    let! info = getSvgInfo js
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)

                    let boundScale = match model.LogicalWidth with | w when w <> fst initBound -> w / fst initBound | _ -> 1.0
                    let rHit = max 12.0 (float (model.VertexRadius + 6) * boundScale)
                    let rEntryHit = max 18.0 (25.0 * boundScale) // generous hit radius for entry point

                    // Check vertices in outer polygon
                    let dragOuter =
                        [|0 .. model.Outer.Length - 1|]
                        |> Array.tryFind (fun i -> Geometry.withinRadiusSq model.Outer.[i] svgPt rHit)
                        |> Option.map (fun i -> { PolyIndex = 0; VertexIndex = i })

                    // Check vertices in islands
                    let dragIsland =
                        match dragOuter with
                        | Some _ -> None
                        | None ->
                            [|0 .. model.Islands.Length - 1|]
                            |> Array.tryPick (fun pi ->
                                let poly = model.Islands.[pi]
                                [|0 .. poly.Length - 1|]
                                |> Array.tryFind (fun vi -> Geometry.withinRadiusSq poly.[vi] svgPt rHit)
                                |> Option.map (fun vi -> { PolyIndex = pi + 1; VertexIndex = vi })
                            )

                    let drag = 
                        match dragOuter with
                        | Some d -> Some d
                        | None -> dragIsland

                    let entryDrag = 
                        match drag with
                        | Some _ -> false
                        | None -> Geometry.withinRadiusSq model.EntryPoint svgPt rEntryHit

                    let ghostHit =
                        match drag, entryDrag, model.GhostVertex with
                        | None, false, Some ghost ->
                            match Geometry.withinRadiusSq ghost.Point svgPt (rHit + 4.0) with
                            | true -> Some ghost
                            | false -> None
                        | _ -> None

                    match drag, entryDrag, ghostHit with
                    | Some d, _, _ ->
                        let v =
                            match d.PolyIndex = 0 with
                            | true -> model.Outer.[d.VertexIndex]
                            | false -> model.Islands.[d.PolyIndex - 1].[d.VertexIndex]
                        let offset = { X = svgPt.X - v.X; Y = svgPt.Y - v.Y }
                        let newModel = snapshot model
                        return { newModel with Dragging = Some d; DragOffset = Some offset; SvgInfo = Some info; GhostVertex = None }

                    | None, true, _ ->
                        let offset = { X = svgPt.X - model.EntryPoint.X; Y = svgPt.Y - model.EntryPoint.Y }
                        let newModel = snapshot model
                        return { newModel with DraggingEntry = true; DragOffset = Some offset; SvgInfo = Some info; GhostVertex = None }

                    | None, false, Some ghost ->
                        match commitGhost ghost model with
                        | Some committed ->
                            let insertIdx = ghost.EdgeIndex + 1
                            let dragInfo = { PolyIndex = ghost.PolyIndex; VertexIndex = insertIdx }
                            return { committed with Dragging = Some dragInfo; DragOffset = Some { X = 0.0; Y = 0.0 }; SvgInfo = Some info; GhostVertex = None }
                        | None ->
                            return { model with SvgInfo = Some info; GhostVertex = None }

                    | None, false, None ->
                        // Check if click was inside an island to drag whole island
                        let dragIslandBody =
                            model.Islands
                            |> Array.tryFindIndex (fun isl -> Geometry.isInsidePolygon isl svgPt)

                        match dragIslandBody with
                        | Some islIdx ->
                            let newModel = snapshot model
                            return { newModel with DraggingIsland = Some islIdx; DragOffset = Some svgPt; SvgInfo = Some info; GhostVertex = None }
                        | None ->
                            return { model with SvgInfo = Some info }
            }


        | PointerUp -> async { return handlePointerUp model }

        | PointerMove ev -> async { return handlePointerMove ev model }

        | DoubleClick ev ->
            async {
                match model.PolygonEnabled with
                | false -> return model
                | true ->
                    let! p = toSvgCoords js ev
                    let rThreshold = float model.VertexRadius + 6.0
                    
                    let tryDeleteVertex (poly: Point[]) =
                        poly
                        |> Array.mapi (fun i pt -> (i, pt))
                        |> Array.tryFind (fun (_, pt) -> Geometry.withinRadiusSq pt p rThreshold)
                        |> Option.bind (fun (vi, _) ->
                            let newPoly = Array.init (poly.Length - 1) (fun idx -> match idx < vi with | true -> poly.[idx] | false -> poly.[idx + 1])
                            match newPoly.Length >= 3 && not (Geometry.polygonSelfIntersects newPoly) with
                            | true -> Some newPoly
                            | false -> None
                        )

                    // Step 1: Try deleting vertex in outer polygon
                    let afterOuterDelete =
                        match tryDeleteVertex model.Outer with
                        | Some newOuter -> Some { snapshot model with Outer = newOuter }
                        | None -> None

                    // Step 2: Try deleting vertex in islands
                    let afterIslandDelete =
                        match afterOuterDelete with
                        | Some _ -> afterOuterDelete
                        | None ->
                            [|0 .. model.Islands.Length - 1|]
                            |> Array.tryPick (fun i ->
                                match tryDeleteVertex model.Islands.[i] with
                                | Some newIsland ->
                                    let newIslands =
                                        model.Islands
                                        |> Array.mapi (fun idx isl -> match idx = i with true -> newIsland | false -> isl)
                                    Some { snapshot model with Islands = newIslands }
                                | None -> None
                            )

                    // Step 3: Insert vertex on edges
                    let afterInsert =
                        match afterIslandDelete with
                        | Some _ -> afterIslandDelete
                        | None ->
                            let rThresholdInsert = 20.0
                            let edgesOuter =
                                [|0 .. model.Outer.Length - 1|]
                                |> Array.map (fun i -> 0, i, model.Outer.[i], model.Outer.[(i + 1) % model.Outer.Length])
                            
                            let edgesIslands =
                                model.Islands
                                |> Array.mapi (fun idx isl ->
                                    [|0 .. isl.Length - 1|]
                                    |> Array.map (fun i -> idx + 1, i, isl.[i], isl.[(i + 1) % isl.Length])
                                )
                                |> Array.concat

                            let closestEdge =
                                Array.append edgesOuter edgesIslands
                                |> Array.map (fun (pIdx, vIdx, a, b) ->
                                    let distSq = Geometry.distancePointToSegmentSq p a b
                                    (pIdx, vIdx, distSq)
                                )
                                |> Array.filter (fun (_, _, distSq) -> distSq < rThresholdInsert * rThresholdInsert)
                                |> Array.sortBy (fun (_, _, distSq) -> distSq)
                                |> Array.tryHead

                            match closestEdge with
                            | Some (polyIdx, vIdx, _) ->
                                let newModel = snapshot model
                                match polyIdx = 0 with
                                | true ->
                                    let newOuter = Array.append (Array.append (model.Outer.[0..vIdx]) [| p |]) (model.Outer.[vIdx+1..])
                                    let ok = not (Geometry.polygonSelfIntersects newOuter) &&
                                             not (model.Islands |> Array.exists (fun island -> Geometry.polygonsIntersect newOuter island)) &&
                                             (model.Islands |> Array.forall (fun island -> Geometry.isPolygonInside newOuter island))
                                    match ok with
                                    | true -> Some { newModel with Outer = newOuter }
                                    | false -> None
                                | false ->
                                    let islandIdx = polyIdx - 1
                                    let island = model.Islands.[islandIdx]
                                    let newIsland = Array.append (Array.append (island.[0..vIdx]) [| p |]) (island.[vIdx+1..])
                                    let ok = Geometry.isPolygonInside model.Outer newIsland &&
                                             not (Geometry.polygonSelfIntersects newIsland) &&
                                             not (model.Islands |> Array.mapi (fun i isl -> i, isl) |> Array.exists (fun (i, isl) -> i <> islandIdx && Geometry.polygonsIntersect newIsland isl))
                                    match ok with
                                    | true ->
                                        let newIslands =
                                            model.Islands
                                            |> Array.mapi (fun idx isl -> match idx = islandIdx with true -> newIsland | false -> isl)
                                        Some { newModel with Islands = newIslands }
                                    | false -> None
                            | None -> None

                    // Step 4: Delete entire island if inside
                    let afterIslandRemove =
                        match afterInsert with
                        | Some _ -> afterInsert
                        | None ->
                            let insideIslandIdx =
                                model.Islands
                                |> Array.tryFindIndex (fun island ->
                                    Geometry.isInsidePolygon island p &&
                                    (island |> Array.forall (fun v -> not (Geometry.withinRadiusSq v p rThreshold))) &&
                                    ([| 0 .. island.Length - 1 |] |> Array.forall (fun i ->
                                        let a = island.[i]
                                        let b = island.[(i + 1) % island.Length]
                                        Geometry.distancePointToSegmentSq p a b > rThreshold * rThreshold))
                                )
                            match insideIslandIdx with
                            | Some idx ->
                                let newIslands = model.Islands |> Array.mapi (fun i isl -> i, isl) |> Array.filter (fun (i,_) -> i <> idx) |> Array.map snd
                                Some { snapshot model with Islands = newIslands }
                            | None -> None

                    // Step 5: Add new island fallback
                    let finalModel =
                        match afterIslandRemove with
                        | Some m -> m
                        | None ->
                            match Geometry.isInsidePolygon model.Outer p with
                            | false -> model
                            | true ->
                                let size = 40.0 * (model.LogicalWidth/fst initBound)
                                let half = size / 2.0
                                let island = [| { X = p.X - half; Y = p.Y - half }
                                                { X = p.X + half; Y = p.Y - half }
                                                { X = p.X + half; Y = p.Y + half }
                                                { X = p.X - half; Y = p.Y + half } |]
                                let insideOuter = Geometry.isPolygonInside model.Outer island
                                let insideAnyIsland = model.Islands |> Array.exists (fun existingIsland -> Geometry.isPolygonInside existingIsland island)
                                let noIntersectsExisting = not (model.Islands |> Array.exists (fun existingIsland -> Geometry.polygonsIntersect island existingIsland))
                                let noSelfIntersect = not (Geometry.polygonSelfIntersects island)
                                let wouldEncloseEntry = Geometry.isInsidePolygon island model.EntryPoint
                                match insideOuter && not insideAnyIsland && noIntersectsExisting && noSelfIntersect && not wouldEncloseEntry with
                                | true -> { snapshot model with Islands = Array.append [| island |] model.Islands }
                                | false -> model

                    return finalModel |> refreshCachedStrings
            }
     
        | RemoveVertex _ -> async { return model }
     
        | StartDragEntry ev ->
            async {
                match model.SvgInfo with
                | Some info ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let offset = { X = svgPt.X - model.EntryPoint.X; Y = svgPt.Y - model.EntryPoint.Y }
                    return { model with DraggingEntry = true; DragOffset = Some offset }
                | None ->
                    return model
            }

        | MoveDragEntry ev ->
            async {
                match model.SvgInfo, model.DraggingEntry, model.DragOffset with
                | Some info, true, Some offset ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let newEntry = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

                    let insideOuter = Geometry.isInsidePolygon model.Outer newEntry
                    let outsideIslands =
                        not (model.Islands |> Array.exists (fun isl -> Geometry.isInsidePolygon isl newEntry))

                    match insideOuter && outsideIslands with
                    | true -> return { model with EntryPoint = newEntry }
                    | false -> return { model with EntryPoint = Geometry.closestValidEntryPoint model.Outer model.Islands }
                | _ ->
                    return model
            }

        | EndDragEntry ->
            async {
                return { model with DraggingEntry = false; DragOffset = None }
            }

        | CommitGhostVertex ->
            async {
                match updateSync CommitGhostVertex model with
                | Some m -> return m
                | None -> return model
            }

        | ImportFromSyntax (outer, islands, abs, entry, w, h) ->
            async {
                match importPolygonStrings outer islands abs entry w h model with
                | Ok m -> return snapshot m
                | Error _ -> return model
            }
