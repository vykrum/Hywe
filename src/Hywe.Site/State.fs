namespace Hywe.Site

open System
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components.Web
open System.Text.Json

module State =

    // ---------- Initial Constants ----------
    let initBound = 300.0, 300.0
    let initWidth = fst initBound
    let initHeight = snd initBound
    let initEntry = { X = 150.0; Y = 50.0 }
    let initRadius = 6
    let minBound = 100.0
    let maxBound = 4000.0
    let initOuter = [| { X = 0.0; Y = 0.0 }
                       { X = initWidth; Y = 0.0 }
                       { X = initWidth; Y = initHeight }
                       { X = 0.0; Y = initHeight } |]
    let initIslands = Array.empty<Point[]>

    // ---------- Import helpers ----------

    let parsePoint (s: string) : Result<Point, string> =
        match s.Split(',', StringSplitOptions.RemoveEmptyEntries) with
        | [| x; y |] ->
            match Double.TryParse x, Double.TryParse y with
            | (true, xv), (true, yv) ->
                Ok { X = xv * 10.0; Y = yv * 10.0 }
            | _ -> Error $"Invalid point: {s}"
        | _ -> Error $"Invalid point format: {s}"

    let sequenceResults (arr: Result<'a,'e> array) : Result<'a array,'e> =
        let buffer = ResizeArray<'a>()
        let mutable err : 'e option = None
        let mutable i = 0

        while i < arr.Length && err.IsNone do
            match arr.[i] with
            | Ok v -> buffer.Add v
            | Error e -> err <- Some e
            i <- i + 1

        match err with
        | Some e -> Error e
        | None -> Ok (buffer.ToArray())

    let parsePoly (s: string) : Result<Point[], string> =
        s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.chunkBySize 2
        |> Array.map (fun a -> parsePoint (String.concat "," a))
        |> sequenceResults

    let parseIslands (s: string) : Result<Point[][], string> =
        if String.IsNullOrWhiteSpace s then Ok [||]
        else
            s.Split('-', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun isl ->
                isl.Split(',', StringSplitOptions.RemoveEmptyEntries)
                |> Array.chunkBySize 2
                |> Array.map (fun a -> parsePoint (String.concat "," a))
                |> sequenceResults)
            |> sequenceResults

    // ---------- Utility functions ----------
    let clampPt (model: PolygonEditorModel) (pt: Point) =
        {
            X = max 0.0 (min model.LogicalWidth pt.X)
            Y = max 0.0 (min model.LogicalHeight pt.Y)
        }

    let snapshot (model: PolygonEditorModel) : PolygonEditorModel =
        { model with Dragging = None; DragOffset = None }

    let polyToSvgPoints (poly: Point[]) =
        poly |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " "

    /// Refreshes cached strings to avoid expensive re-formatting during every frame
    let refreshCachedStrings (model: PolygonEditorModel) =
        { model with
            OuterPointsStr = polyToSvgPoints model.Outer
            IslandPointsStrs = model.Islands |> Array.map polyToSvgPoints }

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

        if Geometry.isEntryPointValid outer islands entry then entry
        else Geometry.closestValidEntryPoint outer islands

    /// Return (outer, islands, absolute, entry, width, height, elevation, baseStr)
    let exportPolygonStrings (model: PolygonEditorModel) : string * string * string * string * int * int * int * string =
        let fmtPoint (p: Point) = sprintf "%d,%d" (int (System.Math.Round(p.X / 10.0))) (int (System.Math.Round(p.Y / 10.0)))

        let outer =
            model.Outer
            |> Array.map fmtPoint
            |> String.concat ","

        let islands =
            model.Islands
            |> Array.map (fun isl -> isl |> Array.map fmtPoint |> String.concat ",")
            |> String.concat "-"

        let entry = fmtPoint (ensureEntryWithin model.Outer model.Islands model.EntryPoint)
        let absolute = if model.UseAbsolute then "1" else "0"
        let w = int (System.Math.Round(model.LogicalWidth / 10.0))
        let h = int (System.Math.Round(model.LogicalHeight / 10.0))
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

        parsePoly outerStr
        |> Result.bind (fun outer ->
            parseIslands islandsStr
            |> Result.bind (fun islands ->
                parsePoint entryStr
                |> Result.map (fun entry ->
                    let width = if w <= 0 then initWidth else float (max 10 w) * 10.0
                    let height = if h <= 0 then initHeight else float (max 10 h) * 10.0
                    
                    let outer = if Array.isEmpty outer then initOuter else outer
                    let fixedEntry = ensureEntryWithin outer islands entry

                    { model with
                        Outer = outer
                        Islands = islands
                        EntryPoint = fixedEntry
                        LogicalWidth = width
                        LogicalHeight = height
                        UseAbsolute = (absStr = "1")
                        UseBoundary = not (absStr = "1")
                        PolygonEnabled = not (absStr = "1")
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
            LogicalWidth = initWidth
            LogicalHeight = initHeight
            Elevation = 0
            BaseStr = ""
            Outer = initOuter
            Islands = initIslands
            Dragging = None
            DragOffset = None
            SvgInfo = None
            LastMoveMs = None
            VertexRadius = initRadius
            EntryPoint = initEntry
            DraggingEntry = false
            OuterPointsStr = ""
            IslandPointsStrs = [||]
        }
        |> refreshCachedStrings

    // ---------- Update ----------
    let update (js: IJSRuntime) (msg: PolygonEditorMessage) (model: PolygonEditorModel) : Async<PolygonEditorModel> =
        match msg with
        | ToggleBoundary isChecked ->
            async { 
                let newOuter = if isChecked && model.Outer.Length < 3 then initOuter else model.Outer
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

        | UpdateLogicalWidth newW -> async {
            let safeW = if newW <= 0.0 then initWidth else max minBound ((max 10.0 newW) * 10.0)
            let oldW = model.LogicalWidth
            let scaleX = if oldW <= 0.0 then 1.0 else safeW / oldW

            let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX })
            let newIslands =
                model.Islands
                |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scaleX }))
            let updated = { model with LogicalWidth = safeW; Outer = newOuter; Islands = newIslands }
            return updated |> refreshCachedStrings
            }

        | UpdateLogicalHeight newH -> async {
            let safeH = if newH <= 0.0 then initHeight else max minBound ((max 10.0 newH) * 10.0)
            let oldH = model.LogicalHeight
            let scaleY = if oldH <= 0.0 then 1.0 else safeH / oldH

            let newOuter = model.Outer |> Array.map (fun pt -> { pt with Y = pt.Y * scaleY })
            let newIslands =
                model.Islands
                |> Array.map (Array.map (fun pt -> { pt with Y = pt.Y * scaleY }))

            let updated = { model with LogicalHeight = safeH; Outer = newOuter; Islands = newIslands }
            return updated |> refreshCachedStrings
            }

        | PointerDown ev ->
            async {
                if not model.PolygonEnabled then return model
                else
                    // Get svg transform info once per drag (cheap JS call)
                    let! info = getSvgInfo js
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)

                    let rHit = float model.VertexRadius + 4.0
                    let rEntryHit = float model.VertexRadius + 4.0 // hit radius for entry point
                    let mutable drag: DragInfo option = None
                    let mutable entryDrag = false

                    // Check vertices in outer polygon
                    for i = 0 to model.Outer.Length - 1 do
                        if drag.IsNone && Geometry.withinRadiusSq model.Outer.[i] svgPt rHit then
                            drag <- Some { PolyIndex = 0; VertexIndex = i }

                    // Check vertices in islands
                    if drag.IsNone then
                        for pi = 0 to model.Islands.Length - 1 do
                            let poly = model.Islands.[pi]
                            for vi = 0 to poly.Length - 1 do
                                if drag.IsNone && Geometry.withinRadiusSq poly.[vi] svgPt rHit then
                                    drag <- Some { PolyIndex = pi + 1; VertexIndex = vi }

                    // Check entry point
                    if drag.IsNone && Geometry.withinRadiusSq model.EntryPoint svgPt rEntryHit then
                        entryDrag <- true

                    match drag, entryDrag with
                    | Some d, _ ->
                        let v =
                            if d.PolyIndex = 0 then model.Outer.[d.VertexIndex]
                            else model.Islands.[d.PolyIndex - 1].[d.VertexIndex]
                        let offset = { X = svgPt.X - v.X; Y = svgPt.Y - v.Y }
                        let newModel = snapshot model
                        return { newModel with Dragging = Some d; DragOffset = Some offset; SvgInfo = Some info }
                    | None, true ->
                        let offset = { X = svgPt.X - model.EntryPoint.X; Y = svgPt.Y - model.EntryPoint.Y }
                        let newModel = snapshot model
                        return { newModel with DraggingEntry = true; DragOffset = Some offset; SvgInfo = Some info }
                    | None, false ->
                        return { model with SvgInfo = Some info }
            }


        | PointerUp -> async { return { model with Dragging = None; DragOffset = None; LastMoveMs = None;} }

        | PointerMove ev ->
            async {
                if not model.PolygonEnabled then return model
                else
                    let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
                
                    // Throttling check using pattern matching
                    return! match model.LastMoveMs with
                            | Some last when nowMs - last < 16.0 -> async { return model }
                            | _ -> 
                                match model.Dragging, model.DragOffset, model.SvgInfo with
                                // -------------------------------------------------------
                                // 1) Dragging the entry point
                                // -------------------------------------------------------
                                | None, Some offset, Some info when model.DraggingEntry ->
                                    async {
                                        let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                                        let newEntry = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

                                        return match Geometry.isEntryPointValid model.Outer model.Islands newEntry with
                                               | true -> { model with EntryPoint = newEntry; LastMoveMs = Some nowMs }
                                               | false -> model
                                    }

                                // -------------------------------------------------------
                                // 2) Dragging a polygon vertex (outer or island)
                                // -------------------------------------------------------
                                | Some drag, Some offset, Some info ->
                                    async {
                                        let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                                        let newPt = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

                                        // Generate the proposed state based on what is being dragged
                                        let proposedModel = 
                                            match drag.PolyIndex = 0 with
                                            | true ->
                                                let newOuter = Array.copy model.Outer
                                                newOuter.[drag.VertexIndex] <- newPt
                                                { model with Outer = newOuter }
                                            | false ->
                                                let islandIdx = drag.PolyIndex - 1
                                                let newIslands = Array.copy model.Islands
                                                let poly = Array.copy newIslands.[islandIdx]
                                                poly.[drag.VertexIndex] <- newPt
                                                newIslands.[islandIdx] <- poly
                                                { model with Islands = newIslands }

                                        // Validate the entire configuration using the pipeline helper
                                        return match Geometry.isConfigurationValid proposedModel.Outer proposedModel.Islands with
                                               | false -> model
                                               | true ->
                                                   let isEntryValid = Geometry.isEntryPointValid proposedModel.Outer proposedModel.Islands proposedModel.EntryPoint
                                               
                                                   let finalModel = 
                                                       match isEntryValid with
                                                       | true -> { proposedModel with LastMoveMs = Some nowMs }
                                                       | false -> 
                                                            { proposedModel with 
                                                                EntryPoint = Geometry.closestValidEntryPoint proposedModel.Outer proposedModel.Islands
                                                                LastMoveMs = Some nowMs }
                                                   finalModel |> refreshCachedStrings
                                    }
                                | _ -> async { return model }
            }

        | DoubleClick ev ->
            async {
                if not model.PolygonEnabled then return model
                else
                    let! p = toSvgCoords js ev
                    let rThreshold = float model.VertexRadius + 6.0
                    let mutable updatedModel = model
                    let mutable didDelete = false
                    let tryDeleteVertex (poly: Point[]) =
                        poly
                        |> Array.mapi (fun i pt -> (i, pt))
                        |> Array.tryFind (fun (_, pt) -> Geometry.withinRadiusSq pt p rThreshold)
                        |> Option.bind (fun (vi, _) ->
                            // Create new polygon with vertex at vi removed
                            let newPoly = Array.init (poly.Length - 1) (fun idx -> if idx < vi then poly.[idx] else poly.[idx + 1])
                            // Only accept if polygon has at least 3 vertices and no self intersections
                            if newPoly.Length >= 3 && not (Geometry.polygonSelfIntersects newPoly) then Some newPoly else None
                        )
                    // 1. Try deleting vertex in outer polygon
                    match tryDeleteVertex model.Outer with
                    | Some newOuter ->
                        updatedModel <- snapshot model
                        updatedModel <- { updatedModel with Outer = newOuter }
                        didDelete <- true
                    | None ->
                        // 2. Try deleting vertex in islands
                        let mutable islandDeleted = false
                        for i in 0 .. model.Islands.Length - 1 do
                            if not islandDeleted then
                                match tryDeleteVertex model.Islands.[i] with
                                | Some newIsland ->
                                    updatedModel <- snapshot updatedModel
                                    let newIslands = Array.copy updatedModel.Islands
                                    newIslands.[i] <- newIsland
                                    updatedModel <- { updatedModel with Islands = newIslands }
                                    islandDeleted <- true
                                    didDelete <- true
                                | None -> ()

                        if not islandDeleted then
                            // Check insert vertex on edges
                            let rThresholdInsert = 20.0

                            let mutable insertPolyIndex : int option = None
                            let mutable insertVertexIndex : int option = None
                            let mutable minDistSq : float = Double.MaxValue

                            let checkEdges (poly: Point[]) (polyIndex: int) =
                                for i in 0 .. poly.Length - 1 do
                                    let j = (i + 1) % poly.Length
                                    let a = poly.[i]
                                    let b = poly.[j]
                                    let distSq = Geometry.distancePointToSegmentSq p a b
                                    if distSq < minDistSq && distSq < rThresholdInsert * rThresholdInsert then
                                        minDistSq <- distSq
                                        insertPolyIndex <- Some polyIndex
                                        insertVertexIndex <- Some j

                            checkEdges model.Outer 0
                            for idx in 0 .. model.Islands.Length - 1 do
                                checkEdges model.Islands.[idx] (idx + 1)

                            match insertPolyIndex, insertVertexIndex with
                            | Some polyIdx, Some vIdx ->
                                let newModel = snapshot model
                                if polyIdx = 0 then
                                    let newOuter = Array.append (Array.append (model.Outer.[0..vIdx-1]) [| p |]) (model.Outer.[vIdx..])
                                    if not (Geometry.polygonSelfIntersects newOuter) &&
                                       not (model.Islands |> Array.exists (fun island -> Geometry.polygonsIntersect newOuter island)) &&
                                       (model.Islands |> Array.forall (fun island -> Geometry.isPolygonInside newOuter island)) then
                                        updatedModel <- { newModel with Outer = newOuter }
                                        didDelete <- true
                                    // else do nothing
                                else
                                    let islandIdx = polyIdx - 1
                                    let island = model.Islands.[islandIdx]
                                    let newIsland = Array.append (Array.append (island.[0..vIdx-1]) [| p |]) (island.[vIdx..])

                                    if Geometry.isPolygonInside model.Outer newIsland &&
                                       not (Geometry.polygonSelfIntersects newIsland) &&
                                       not (model.Islands
                                            |> Array.mapi (fun i isl -> i, isl)
                                            |> Array.exists (fun (i, isl) -> i <> islandIdx && Geometry.polygonsIntersect newIsland isl)) then
                                        let newIslands = Array.copy model.Islands
                                        newIslands.[islandIdx] <- newIsland
                                        updatedModel <- { newModel with Islands = newIslands }
                                        didDelete <- true
                                    // else do nothing
                            | _ -> ()

                            // 3. Delete entire island if inside polygon but NOT near vertex or edge
                            let insideIslandIdx =
                                model.Islands
                                |> Array.tryFindIndex (fun island ->
                                    Geometry.isInsidePolygon island p &&
                                    // NOT near any vertex
                                    (island |> Array.forall (fun v -> not (Geometry.withinRadiusSq v p rThreshold))) &&
                                    // NOT near any edge (distance > threshold)
                                    (let distToEdgesOk =
                                        [| 0 .. island.Length - 1 |]
                                        |> Array.forall (fun i ->
                                            let a = island.[i]
                                            let b = island.[(i + 1) % island.Length]
                                            Geometry.distancePointToSegmentSq p a b > rThreshold * rThreshold)
                                    distToEdgesOk)
                                )

                            match insideIslandIdx with
                            | Some idx ->
                                updatedModel <- snapshot updatedModel
                                let newIslands = updatedModel.Islands |> Array.mapi (fun i isl -> i, isl) |> Array.filter (fun (i,_) -> i <> idx) |> Array.map snd
                                updatedModel <- { updatedModel with Islands = newIslands }
                                didDelete <- true
                            | None -> ()

                    // 4. fallback: add new island rectangle if not deleted anything
                    if not didDelete then
                        if Geometry.isInsidePolygon model.Outer p then
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
                            // Prevent island that would enclose the current entry point
                            let wouldEncloseEntry = Geometry.isInsidePolygon island model.EntryPoint

                            if insideOuter && not insideAnyIsland && noIntersectsExisting && noSelfIntersect && not wouldEncloseEntry then
                                updatedModel <- { updatedModel with Islands = Array.append [| island |] model.Islands }
                            // else do nothing

                    return updatedModel |> refreshCachedStrings
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

                    if insideOuter && outsideIslands then
                        let updated =  { model with EntryPoint = newEntry }
                        return updated
                    else
                        let updated =  { model with EntryPoint = Geometry.closestValidEntryPoint model.Outer model.Islands }
                        return updated
                | _ ->
                    return model
            }

        | EndDragEntry ->
            async {
                return { model with DraggingEntry = false; DragOffset = None }
            }

        | ImportFromSyntax (outer, islands, abs, entry, w, h) ->
            async {
                match importPolygonStrings outer islands abs entry w h model with
                | Ok m -> return snapshot m
                | Error _ -> return model
            }
