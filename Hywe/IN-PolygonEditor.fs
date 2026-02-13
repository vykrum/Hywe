module PolygonEditor

open System.Text.Json
open Microsoft.JSInterop
open Bolero
open Bolero.Html
open System
open Microsoft.AspNetCore.Components.Web

// ---------- Types ----------
type Point = { X: float; Y: float }
type DragInfo = { PolyIndex: int; VertexIndex: int }

type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

// Use arrays for fast random access and cheap shallow copies
type PolygonEditorModel =
    {
        UseBoundary: bool
        UseAbsolute: bool
        PolygonEnabled: bool        
        LogicalWidth: float
        LogicalHeight: float
        Outer: Point[]
        Islands: Point[][]
        VertexRadius: int
        Dragging: DragInfo option
        DragOffset: Point option      // offset between pointer svg point and vertex so dragging doesn't jump
        SvgInfo: SvgInfo option       // cached transform info so we don't call JS on every mousemove
        LastMoveMs: float option      // for simple throttling
        EntryPoint: Point
        DraggingEntry: bool
    }

type EditorState =
    | Stable of PolygonEditorModel
    | FreshlyImported of PolygonEditorModel

type PolygonEditorMessage =
    | ToggleBoundary of bool
    | ToggleAbsolute of bool
    | UpdateLogicalWidth of float
    | UpdateLogicalHeight of float
    | PointerDown of MouseEventArgs
    | PointerUp
    | PointerMove of MouseEventArgs
    | DoubleClick of MouseEventArgs
    | RemoveVertex of int * int
    | StartDragEntry of MouseEventArgs
    | MoveDragEntry of MouseEventArgs
    | EndDragEntry
    | ImportFromSyntax of string * string * string * string * int * int

// ---------- Geometry helpers ----------
let inline sqr x = x * x
let distanceSq (a: Point) (b: Point) = sqr (a.X - b.X) + sqr (a.Y - b.Y)
let withinRadiusSq (a: Point) (b: Point) (r: float) = distanceSq a b <= r*r

let distancePointToSegmentSq p a b =
    let vx = b.X - a.X
    let vy = b.Y - a.Y
    let wx = p.X - a.X
    let wy = p.Y - a.Y
    let c1 = wx * vx + wy * vy
    if c1 <= 0.0 then distanceSq p a
    else
        let c2 = vx * vx + vy * vy
        if c2 <= c1 then distanceSq p b
        else
            let t = c1 / c2
            let proj = { X = a.X + t*vx; Y = a.Y + t*vy }
            distanceSq p proj

let isInsidePolygon (poly: Point[]) (pt: Point) =
    let n = poly.Length
    [| 0 .. n - 1 |]
    |> Array.fold (fun acc i ->
        let vi = poly.[i]
        let vj = poly.[(i + n - 1) % n]
        match (vi.Y > pt.Y) <> (vj.Y > pt.Y) with
        | true ->
            let xIntersect = (vj.X - vi.X) * (pt.Y - vi.Y) / (vj.Y - vi.Y) + vi.X
            match pt.X < xIntersect with
            | true -> not acc
            | false -> acc
        | false -> acc
    ) false

let isPolygonInside outer inner =
    inner |> Array.forall (isInsidePolygon outer)

let private eps = 1e-9

let private orient (p: Point) (q: Point) (r: Point) =
    let v = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
    if abs v < eps then 0 elif v > 0.0 then 1 else 2

let private onSegment (p: Point) (q: Point) (r: Point) =
    q.X <= max p.X r.X + eps && q.X >= min p.X r.X - eps &&
    q.Y <= max p.Y r.Y + eps && q.Y >= min p.Y r.Y - eps

let edgesIntersect (p1: Point) (q1: Point) (p2: Point) (q2: Point) =
    let o1 = orient p1 q1 p2
    let o2 = orient p1 q1 q2
    let o3 = orient p2 q2 p1
    let o4 = orient p2 q2 q1

    if o1 <> o2 && o3 <> o4 then true
    elif o1 = 0 && onSegment p1 p2 q1 then true
    elif o2 = 0 && onSegment p1 q2 q1 then true
    elif o3 = 0 && onSegment p2 p1 q2 then true
    elif o4 = 0 && onSegment p2 q1 q2 then true
    else false

let polygonSelfIntersects (points: Point[]) =
    let n = points.Length
    let rec loop i j =
        if i >= n then false
        elif j >= n then loop (i + 1) (i + 2)
        elif abs (i - j) = 1 || abs (i - j) = n - 1 then loop i (j + 1)
        elif edgesIntersect points.[i] points.[(i+1)%n] points.[j] points.[(j+1)%n] then true
        else loop i (j + 1)
    loop 0 2

let polygonsIntersect (polyA: Point[]) (polyB: Point[]) : bool =
    let edgesA = [| for i in 0 .. polyA.Length - 1 -> (polyA.[i], polyA.[(i + 1) % polyA.Length]) |]
    let edgesB = [| for i in 0 .. polyB.Length - 1 -> (polyB.[i], polyB.[(i + 1) % polyB.Length]) |]

    edgesA |> Array.exists (fun (a1,a2) ->
        edgesB |> Array.exists (fun (b1,b2) ->
            edgesIntersect a1 a2 b1 b2))

let closestValidEntryPoint (outer: Point[]) (islands: Point[][]) =
    let vertexClearance = 80.0   // min distance from any vertex
    let edgeClearance   = 50.0   // min distance from any edge

    let tooCloseToVertices (pt: Point) (poly: Point[]) =
        poly |> Array.exists (fun v -> withinRadiusSq pt v vertexClearance)

    let tooCloseToEdges (pt: Point) (poly: Point[]) =
        let mutable close = false
        let n = poly.Length
        let mutable i = 0
        while i < n && not close do
            let a = poly.[i]
            let b = poly.[(i+1) % n]
            if distancePointToSegmentSq pt a b <= edgeClearance * edgeClearance then
                close <- true
            i <- i + 1
        close

    let mutable candidate = { X = 0.0; Y = 0.0 }
    let step = 1.0
    let maxSearch = 200.0
    let mutable found = false
    let mutable distSq = Double.MaxValue

    for x in 0.0 .. step .. maxSearch do
        for y in 0.0 .. step .. maxSearch do
            let pt = { X = x; Y = y }
            if isInsidePolygon outer pt &&
               not (islands |> Array.exists (fun isl -> isInsidePolygon isl pt)) &&
               not (tooCloseToVertices pt outer) &&
               not (tooCloseToEdges pt outer) &&
               not (islands |> Array.exists (tooCloseToVertices pt)) &&
               not (islands |> Array.exists (tooCloseToEdges pt)) then
                let d = x*x + y*y
                if d < distSq then
                    candidate <- pt
                    distSq <- d
                    found <- true

    if found then candidate
    else
        { X = outer |> Array.averageBy (fun p -> p.X)
          Y = outer |> Array.averageBy (fun p -> p.Y) }

let isConfigurationValid (outer: Point[]) (islands: Point[][]) =
    let allIslandsValid = 
        islands 
        |> Array.forall (fun isl -> 
            not (polygonSelfIntersects isl) && 
            isPolygonInside outer isl)
            
    let noIslandIntersections =
        islands 
        |> Array.mapi (fun i a -> 
            islands 
            |> Array.mapi (fun j b -> i, j, b) 
            |> Array.forall (fun (idxA, idxB, bPoly) -> 
                match idxA = idxB with 
                | true -> true 
                | false -> not (polygonsIntersect a bPoly)))
        |> Array.forall id

    match not (polygonSelfIntersects outer) with
    | true -> allIslandsValid && noIslandIntersections
    | false -> false

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

    let minClear = 20.0
    let minClearSq = minClear * minClear

    let sq x = x * x
    let distSq a b = sq (a.X - b.X) + sq (a.Y - b.Y)

    let centroid (poly: Point[]) =
        let sx, sy =
            poly
            |> Array.fold (fun (ax, ay) p -> (ax + p.X, ay + p.Y)) (0.0, 0.0)
        { X = sx / float poly.Length; Y = sy / float poly.Length }

    let nearestEdgeSq poly pt =
        poly
        |> Array.mapi (fun i a ->
            let b = poly.[(i + 1) % poly.Length]
            distancePointToSegmentSq pt a b)
        |> Array.min

    let tooClose poly pt =
        let edgeTooClose = nearestEdgeSq poly pt < minClearSq
        let vertexTooClose =
            poly |> Array.exists (fun v -> distSq v pt < minClearSq)
        edgeTooClose || vertexTooClose

    // Move toward centroid until valid
    let rec moveInward pt step =
        match isInsidePolygon outer pt,
              islands |> Array.exists (fun isl -> isInsidePolygon isl pt),
              tooClose outer pt
        with
        | true, false, false -> pt
        | _ ->
            let c = centroid outer
            let dx, dy = c.X - pt.X, c.Y - pt.Y
            let len = sqrt (dx * dx + dy * dy)
            let ux, uy =
                match len with
                | l when l > 0.0 -> dx / l, dy / l
                | _ -> 0.0, 0.0
            let next =
                { X = pt.X + ux * step
                  Y = pt.Y + uy * step }
            moveInward next step

    moveInward entry 5.0

/// Return (outer, islands, absolute, entry, width, height)
let exportPolygonStrings (model: PolygonEditorModel) : string * string * string * string * int * int =
    let fmtPoint (p: Point) = sprintf "%d,%d" (int (System.Math.Round(p.X/ 10.0))) (int (System.Math.Round(p.Y/ 10.0)))

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
    let w = int (System.Math.Round(model.LogicalWidth/ 10.0))
    let h = int (System.Math.Round(model.LogicalHeight/ 10.0))
    outer, islands, absolute, entry, w, h

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
                let width = float w * 10.0
                let height = float h * 10.0
                let fixedEntry = ensureEntryWithin outer islands entry

                { model with
                    Outer = outer
                    Islands = islands
                    EntryPoint = fixedEntry
                    LogicalWidth = width
                    LogicalHeight = height
                    UseAbsolute = (absStr = "1")
                    UseBoundary = true
                    PolygonEnabled = true
                    Dragging = None
                    DragOffset = None
                    SvgInfo = None }
            )
        )
    )


// ---------- Initial Model ----------

let initBound = 300.0, 300.0
let initWidth = fst initBound
let initHeight = snd initBound
let initEntry = { X = 150.0; Y = 50.0 }
let initRadius = 6
let minBound = 40.0
let maxBound = 4000.0
let initOuter = [| { X = 0.0; Y = 0.0 }
                   { X = initWidth; Y = 0.0 }
                   { X = initWidth; Y = initHeight }
                   { X = 0.0; Y = initHeight } |]
let initIslands = Array.empty

let initModel =
    {
        UseBoundary = false
        UseAbsolute = true
        PolygonEnabled = false        
        LogicalWidth = initWidth
        LogicalHeight = initHeight
        Outer = initOuter
        Islands = initIslands
        Dragging = None
        DragOffset = None
        SvgInfo = None
        LastMoveMs = None
        VertexRadius = initRadius
        EntryPoint = initEntry
        DraggingEntry = false
    }

// ---------- Update ----------
let update (js: IJSRuntime) (msg: PolygonEditorMessage) (model: PolygonEditorModel) : Async<PolygonEditorModel> =
    match msg with
    | ToggleBoundary isChecked ->
        async { 
            return
                { model with
                    UseBoundary = isChecked
                    PolygonEnabled = isChecked
                    UseAbsolute = match isChecked with
                                  | true -> false
                                  | false -> true
                }
        }

    | ToggleAbsolute isChecked -> async{ 
                                            let updated = { model with UseAbsolute = isChecked }
                                            return updated
                                        }

    | UpdateLogicalWidth newW -> async {
        let safeW = max minBound newW
        let oldW = model.LogicalWidth
        let scaleX = safeW / oldW

        let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX })
        let newIslands =
            model.Islands
            |> Array.map (Array.map (fun pt -> { pt with X = pt.X * scaleX }))
        let updated = { model with LogicalWidth = safeW; Outer = newOuter; Islands = newIslands }
        return updated
        }

    | UpdateLogicalHeight newH -> async {
        let safeH = max minBound newH
        let oldH = model.LogicalHeight
        let scaleY = safeH / oldH

        let newOuter = model.Outer |> Array.map (fun pt -> { pt with Y = pt.Y * scaleY })
        let newIslands =
            model.Islands
            |> Array.map (Array.map (fun pt -> { pt with Y = pt.Y * scaleY }))

        let updated = { model with LogicalHeight = safeH; Outer = newOuter; Islands = newIslands }
        return updated
        }

    | PointerDown ev ->
        async {
            // Get svg transform info once per drag (cheap JS call)
            let! info = getSvgInfo js
            let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)

            let rHit = float model.VertexRadius + 4.0
            let rEntryHit = float model.VertexRadius + 4.0 // hit radius for entry point
            let mutable drag: DragInfo option = None
            let mutable entryDrag = false

            // Check vertices in outer polygon
            for i = 0 to model.Outer.Length - 1 do
                if drag.IsNone && withinRadiusSq model.Outer.[i] svgPt rHit then
                    drag <- Some { PolyIndex = 0; VertexIndex = i }

            // Check vertices in islands
            if drag.IsNone then
                for pi = 0 to model.Islands.Length - 1 do
                    let poly = model.Islands.[pi]
                    for vi = 0 to poly.Length - 1 do
                        if drag.IsNone && withinRadiusSq poly.[vi] svgPt rHit then
                            drag <- Some { PolyIndex = pi + 1; VertexIndex = vi }

            // Check entry point
            if drag.IsNone && withinRadiusSq model.EntryPoint svgPt rEntryHit then
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

                                let insideOuter = isInsidePolygon model.Outer newEntry
                                let outsideIslands = not (model.Islands |> Array.exists (fun isl -> isInsidePolygon isl newEntry))

                                return match insideOuter && outsideIslands with
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
                                return match isConfigurationValid proposedModel.Outer proposedModel.Islands with
                                       | false -> model
                                       | true ->
                                           // Check if existing entry point is still valid in new geometry
                                           let isEntryValid = 
                                                isInsidePolygon proposedModel.Outer proposedModel.EntryPoint &&
                                                not (proposedModel.Islands |> Array.exists (fun isl -> isInsidePolygon isl proposedModel.EntryPoint))
                                       
                                           match isEntryValid with
                                           | true -> { proposedModel with LastMoveMs = Some nowMs }
                                           | false -> 
                                                { proposedModel with 
                                                    EntryPoint = closestValidEntryPoint proposedModel.Outer proposedModel.Islands
                                                    LastMoveMs = Some nowMs }
                            }
                        | _ -> async { return model }
        }

    | DoubleClick ev ->
        async {
            let! p = toSvgCoords js ev
            let rThreshold = float model.VertexRadius + 6.0
            let mutable updatedModel = model
            let mutable didDelete = false
            let tryDeleteVertex (poly: Point[]) =
                poly
                |> Array.mapi (fun i pt -> (i, pt))
                |> Array.tryFind (fun (_, pt) -> withinRadiusSq pt p rThreshold)
                |> Option.bind (fun (vi, _) ->
                    // Create new polygon with vertex at vi removed
                    let newPoly = Array.init (poly.Length - 1) (fun idx -> if idx < vi then poly.[idx] else poly.[idx + 1])
                    // Only accept if polygon has at least 3 vertices and no self intersections
                    if newPoly.Length >= 3 && not (polygonSelfIntersects newPoly) then Some newPoly else None
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
                            let distSq = distancePointToSegmentSq p a b
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
                            if not (polygonSelfIntersects newOuter) &&
                               not (model.Islands |> Array.exists (fun island -> polygonsIntersect newOuter island)) &&
                               (model.Islands |> Array.forall (fun island -> isPolygonInside newOuter island)) then
                                updatedModel <- { newModel with Outer = newOuter }
                                didDelete <- true
                            // else do nothing
                        else
                            let islandIdx = polyIdx - 1
                            let island = model.Islands.[islandIdx]
                            let newIsland = Array.append (Array.append (island.[0..vIdx-1]) [| p |]) (island.[vIdx..])

                            if isPolygonInside model.Outer newIsland &&
                               not (polygonSelfIntersects newIsland) &&
                               not (model.Islands
                                    |> Array.mapi (fun i isl -> i, isl)
                                    |> Array.exists (fun (i, isl) -> i <> islandIdx && polygonsIntersect newIsland isl)) then
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
                            isInsidePolygon island p &&
                            // NOT near any vertex
                            (island |> Array.forall (fun v -> not (withinRadiusSq v p rThreshold))) &&
                            // NOT near any edge (distance > threshold)
                            (let distToEdgesOk =
                                [| 0 .. island.Length - 1 |]
                                |> Array.forall (fun i ->
                                    let a = island.[i]
                                    let b = island.[(i + 1) % island.Length]
                                    distancePointToSegmentSq p a b > rThreshold * rThreshold)
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
                if isInsidePolygon model.Outer p then
                    let size = 40.0 * (model.LogicalWidth/fst initBound)
                    let half = size / 2.0
                    let island = [| { X = p.X - half; Y = p.Y - half }
                                    { X = p.X + half; Y = p.Y - half }
                                    { X = p.X + half; Y = p.Y + half }
                                    { X = p.X - half; Y = p.Y + half } |]

                    let insideOuter = isPolygonInside model.Outer island
                    let insideAnyIsland = model.Islands |> Array.exists (fun existingIsland -> isPolygonInside existingIsland island)
                    let noIntersectsExisting = not (model.Islands |> Array.exists (fun existingIsland -> polygonsIntersect island existingIsland))
                    let noSelfIntersect = not (polygonSelfIntersects island)

                    if insideOuter && not insideAnyIsland && noIntersectsExisting && noSelfIntersect then
                        let newModel = snapshot model
                        updatedModel <- { newModel with Islands = Array.append [| island |] model.Islands }
                    // else do nothing

            return updatedModel
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

                let insideOuter = isInsidePolygon model.Outer newEntry
                let outsideIslands =
                    not (model.Islands |> Array.exists (fun isl -> isInsidePolygon isl newEntry))

                if insideOuter && outsideIslands then
                    let updated =  { model with EntryPoint = newEntry }
                    return updated
                else
                    let updated =  { model with EntryPoint = closestValidEntryPoint model.Outer model.Islands }
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
    let renderNumericInput labelText value msg =
        div {
            attr.style "display: flex; align-items: center; gap: 4px;"
            label { attr.style "font-size: 0.85em; color: #444;"; text labelText }
            input {
                attr.``class`` "boundaryInput"
                attr.``type`` "number"
                attr.step "1"
                attr.style "width: 50px; padding: 2px; font-size: 0.9em;"
                attr.value (string (value / 10.0))
                attr.disabled (not model.UseBoundary)
                on.change (fun ev ->
                    match System.Double.TryParse (string ev.Value) with
                    | (true, v) -> dispatch (msg (v * 10.0))
                    | _ -> ()
                )
            }
        }

    div {
        attr.``class`` "control-and-instructions"
        // Key changes: justify-content center and flex-nowrap
        attr.style "display: flex; flex-flow: row nowrap; gap: 15px; align-items: center; justify-content: center; width: 100%; padding: 5px 0;"

        // --- Column 1: Toggles ---
        div {
            attr.style "display: flex; flex-direction: column; gap: 8px;"
            
            // Toggle 1: Boundary
            div {
                attr.``class`` "toggle-group"
                button {
                    attr.``type`` "button"
                    attr.``class`` (match model.UseBoundary with | false -> "toggle-btn active" | _ -> "toggle-btn")
                    on.click (fun _ -> dispatch (ToggleBoundary false))
                    text "Unbound"
                }
                button {
                    attr.``type`` "button"
                    attr.``class`` (match model.UseBoundary with | true -> "toggle-btn active" | _ -> "toggle-btn")
                    on.click (fun _ -> dispatch (ToggleBoundary true))
                    text "Boundary"
                }
            }

            // Toggle 2: Absolute/Relative
            div {
                attr.``class`` "toggle-group"
                attr.style (match model.UseBoundary with | true -> "display: flex;" | _ -> "display: flex; opacity: 0.3; pointer-events: none;")
                button {
                    attr.``type`` "button"
                    attr.``class`` (match model.UseAbsolute with | false -> "toggle-btn active" | _ -> "toggle-btn")
                    on.click (fun _ -> dispatch (ToggleAbsolute false))
                    text "Relative"
                }
                button {
                    attr.``type`` "button"
                    attr.``class`` (match model.UseAbsolute with | true -> "toggle-btn active" | _ -> "toggle-btn")
                    on.click (fun _ -> dispatch (ToggleAbsolute true))
                    text "Absolute"
                }
            }
        }

        // --- Column 2: Dimensions ---
        div {
            attr.style "display: flex; flex-direction: column; gap: 12px; border-left: 1px solid #eee; border-right: 1px solid #eee; padding: 0 15px;"
            renderNumericInput "W:" model.LogicalWidth UpdateLogicalWidth
            renderNumericInput "H:" model.LogicalHeight UpdateLogicalHeight
        }

        // --- Column 3: Instructions ---
        div {
            attr.style "font-size: 0.75em; color: #777; line-height: 1.3; min-width: 160px;"
            p { attr.style "margin: 0;"; text "• Click edge: add point" }
            p { attr.style "margin: 0;"; text "• Dbl-click point: delete" }
            p { attr.style "margin: 0;"; text "• Dbl-click inside: Island" }
        }
    }

// Polygon Editor SVG with polygons, vertices, and event handlers
let polygonEditorSvg model dispatch =
            let boundScale = match model.LogicalWidth with
                                | w when w <> fst initBound -> w / fst initBound
                                | _ -> 1.0            
            let boundRadius = max 1 (int (float initRadius * boundScale))
            let boundLabel = max 1(int (float (initRadius + 4) * boundScale))  
            let bndTxtRr = max 1(int(float (initRadius + 6) * boundScale))
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
                .pt(model.Outer |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " ")
                .sw(string bndStWdO)
                .Elt()

            // Islands
            for island in model.Islands do
                bdrPgn()
                    .cs("islandPolygon")
                    .pt(island |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " ")
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
            let entryIcon = [|-15.0,25.0;15.0,25.0;15.0,15.0;-5.0,15.0;-5.0,5.0;15.0,5.0;15.0,-5.0;-5.0,-5.0;-5.0,-15.0;15.0,-15.0;15.0,-25.0;-15.0,-25.0|]
                            |> Array.map (fun (x, y) -> model.EntryPoint.X + (x * boundScale * 0.3),model.EntryPoint.Y + (y * boundScale * 0.3))
                            |> Array.map (fun (x, y) -> sprintf "%f,%f" x y)
                            |> String.concat " "
            bdrPgn()
                .cs("entryPoint")
                .pt(entryIcon)
                .sw("0")
                .Elt()
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

