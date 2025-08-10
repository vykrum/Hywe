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
        LogicalWidth: float
        LogicalHeight: float
        Outer: Point[]
        Islands: Point[][]
        VertexRadius: int
        Dragging: DragInfo option
        DragOffset: Point option      // offset between pointer svg point and vertex so dragging doesn't jump
        SvgInfo: SvgInfo option       // cached transform info so we don't call JS on every mousemove
        LastMoveMs: float option      // for simple throttling
    }

// Only minimal messages needed for editing; kept structure similar to original
type PolygonEditorMessage =
    | UpdateLogicalWidth of float
    | UpdateLogicalHeight of float
    | PointerDown of MouseEventArgs
    | PointerUp
    | PointerMove of MouseEventArgs
    | DoubleClick of MouseEventArgs
    | RemoveVertex of int * int

// ---------- Geometry helpers (efficient) ----------
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
    let mutable c = false
    let n = poly.Length
    for i in 0..n-1 do
        let j = (i + n - 1) % n
        let vi = poly.[i]
        let vj = poly.[j]
        if (vi.Y > pt.Y) <> (vj.Y > pt.Y) then
            let xIntersect = (vj.X - vi.X)*(pt.Y - vi.Y)/(vj.Y - vi.Y) + vi.X
            if pt.X < xIntersect then c <- not c
    c

let isPolygonInside outer inner =
    inner |> Array.forall (isInsidePolygon outer)

let edgesIntersect (p1: Point) (p2: Point) (p3: Point) (p4: Point) : bool =
    let cross (a: Point) (b: Point) = a.X * b.Y - a.Y * b.X

    let vec a b = { X = b.X - a.X; Y = b.Y - a.Y }

    let d1 = vec p3 p4
    let d2 = vec p1 p2

    let denominator = cross d2 d1

    if denominator = 0.0 then
        // Parallel or collinear lines
        false
    else
        let s = cross (vec p3 p1) d1 / denominator
        let t = cross (vec p3 p1) d2 / denominator
        // Check if s and t lie between 0 and 1 -> segments intersect
        s >= 0.0 && s <= 1.0 && t >= 0.0 && t <= 1.0

let polygonSelfIntersects (poly: Point[]) : bool =
    let n = poly.Length
    // Check every edge against every other edge
    // Skip adjacent edges and edges sharing vertices
    let edges = [| for i in 0 .. n-1 -> (poly.[i], poly.[(i + 1) % n]) |]

    let intersects i j =
        // Ignore if edges are the same or adjacent
        if abs(i - j) <= 1 || abs(i - j) = n - 1 then false
        else
            let (p1i, p2i) = edges.[i]
            let (p1j, p2j) = edges.[j]
            edgesIntersect p1i p2i p1j p2j


    // Check all pairs
    seq {
        for i in 0 .. n-1 do
            for j in i+1 .. n-1 do
                if intersects i j then yield true
    } |> Seq.contains true

let polygonsIntersect (polyA: Point[]) (polyB: Point[]) : bool =
    let edgesA = [| for i in 0 .. polyA.Length - 1 -> (polyA.[i], polyA.[(i + 1) % polyA.Length]) |]
    let edgesB = [| for i in 0 .. polyB.Length - 1 -> (polyB.[i], polyB.[(i + 1) % polyB.Length]) |]

    edgesA |> Array.exists (fun (a1,a2) ->
        edgesB |> Array.exists (fun (b1,b2) ->
            edgesIntersect a1 a2 b1 b2))

let clampPt (model: PolygonEditorModel) (pt: Point) =
    {
        X = max 0.0 (min model.LogicalWidth pt.X)
        Y = max 0.0 (min model.LogicalHeight pt.Y)
    }

let snapshot (model: PolygonEditorModel) : PolygonEditorModel =
    { model with Dragging = None; DragOffset = None }

// ---------- Initial Model ----------
let initBound = 400.0, 400.0
let initRadius = 6
let initModel =
    let logicalWidth, logicalHeight = initBound
    {
        LogicalWidth = logicalWidth
        LogicalHeight = logicalHeight
        Outer = [| { X = 0.0; Y = 0.0 }
                   { X = logicalWidth; Y = 0.0 }
                   { X = logicalWidth; Y = logicalHeight }
                   { X = 0.0; Y = logicalHeight } |]
        Islands = Array.empty
        Dragging = None
        DragOffset = None
        SvgInfo = None
        LastMoveMs = None
        VertexRadius = initRadius
    }
// ---------- JS interop helpers ----------
// JS function expected to return the SVG client rect and viewBox as a JSON object:
// { left, top, width, height, viewBoxX, viewBoxY, viewBoxW, viewBoxH }
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
    // Map client coords inside svg element to svg viewBox coords
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


// ---------- Update ----------
let update (js: IJSRuntime) (msg: PolygonEditorMessage) (model: PolygonEditorModel) : Async<PolygonEditorModel> =
    match msg with
    | UpdateLogicalWidth newW -> async {
        let oldW = model.LogicalWidth
        let scaleX = newW / oldW

        // Scale outer polygon points in X direction
        let newOuter = model.Outer |> Array.map (fun pt -> { pt with X = pt.X * scaleX })

        // Scale islands points in X direction
        let newIslands = 
            model.Islands
            |> Array.map (fun island -> island |> Array.map (fun pt -> { pt with X = pt.X * scaleX }))

        return { model with LogicalWidth = newW; Outer = newOuter; Islands = newIslands }
        }

    | UpdateLogicalHeight newH -> async {
        let oldH = model.LogicalHeight
        let scaleY = newH / oldH

        // Scale outer polygon points in Y direction
        let newOuter = model.Outer |> Array.map (fun pt -> { pt with Y = pt.Y * scaleY })

        // Scale islands points in Y direction
        let newIslands =
            model.Islands
            |> Array.map (fun island -> island |> Array.map (fun pt -> { pt with Y = pt.Y * scaleY }))

        return { model with LogicalHeight = newH; Outer = newOuter; Islands = newIslands }
        }

    | PointerDown ev ->
        async {
            // Get svg transform info once per drag (cheap JS call)
            let! info = getSvgInfo js
            let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)

            // find vertex hit using squared radius test
            let rHit = float model.VertexRadius + 4.0
            let mutable drag: DragInfo option = None
            for i = 0 to model.Outer.Length - 1 do
                if drag.IsNone && withinRadiusSq model.Outer.[i] svgPt rHit then
                    drag <- Some { PolyIndex = 0; VertexIndex = i }

            if drag.IsNone then
                for pi = 0 to model.Islands.Length - 1 do
                    let poly = model.Islands.[pi]
                    for vi = 0 to poly.Length - 1 do
                        if drag.IsNone && withinRadiusSq poly.[vi] svgPt rHit then
                            drag <- Some { PolyIndex = pi + 1; VertexIndex = vi }

            match drag with
            | Some d ->
                // compute offset so vertex doesn't jump to pointer
                let v = if d.PolyIndex = 0 then model.Outer.[d.VertexIndex] else model.Islands.[d.PolyIndex - 1].[d.VertexIndex]
                let offset = { X = svgPt.X - v.X; Y = svgPt.Y - v.Y }
                let newModel = snapshot model
                return { newModel with Dragging = Some d; DragOffset = Some offset; SvgInfo = Some info;}
            | None ->
                // no drag — keep svg info cache for quick mapping later
                return { model with SvgInfo = Some info }
        }

    | PointerUp -> async { return { model with Dragging = None; DragOffset = None; LastMoveMs = None;} }

    | PointerMove ev ->
        async {
            // Throttle etc. unchanged
            let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
            match model.LastMoveMs with
            | Some last when nowMs - last < 16.0 -> return model
            | _ ->
                match model.Dragging, model.DragOffset, model.SvgInfo with
                | Some drag, Some offset, Some info ->
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let newPt = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

                    // Check if allowed to update vertex
                    let allowed =
                        if drag.PolyIndex = 0 then
                            // Outer polygon update: 
                            // Build new outer with moved vertex
                            let newOuter = 
                                let arr = Array.copy model.Outer
                                arr.[drag.VertexIndex] <- newPt
                                arr
        
                            // Check no self intersection in outer polygon
                            let noSelfIntersect = not (polygonSelfIntersects newOuter)

                            // Check no intersections with any island
                            let noIntersectsIslands = not (model.Islands |> Array.exists (fun island -> polygonsIntersect newOuter island))

                            // Also, all islands must remain inside outer polygon after move (optional, for safety)
                            let islandsInside = model.Islands |> Array.forall (fun island -> isPolygonInside newOuter island)

                            noSelfIntersect && noIntersectsIslands && islandsInside

                        else
                            // Island update:
                            let islandIdx = drag.PolyIndex - 1
                            let newIslands = Array.copy model.Islands
                            let poly = Array.copy newIslands.[islandIdx]
                            poly.[drag.VertexIndex] <- newPt

                            // Check island stays inside outer polygon
                            let insideOuter = isPolygonInside model.Outer poly

                            // Check island has no self intersections
                            let noSelfIntersect = not (polygonSelfIntersects poly)

                            // Check island does not intersect other islands (excluding self)
                            let noIntersectsOthers =
                                model.Islands
                                |> Array.mapi (fun i isl -> i, isl)
                                |> Array.forall (fun (i, isl) -> i = islandIdx || not (polygonsIntersect poly isl))

                            insideOuter && noSelfIntersect && noIntersectsOthers

                    if allowed then
                        let updatedModel =
                            if drag.PolyIndex = 0 then
                                let arr = Array.copy model.Outer
                                arr.[drag.VertexIndex] <- newPt
                                { model with Outer = arr; LastMoveMs = Some nowMs }
                            else
                                let updatedIslands = Array.copy model.Islands
                                let poly = Array.copy updatedIslands.[drag.PolyIndex - 1]
                                poly.[drag.VertexIndex] <- newPt
                                updatedIslands.[drag.PolyIndex - 1] <- poly
                                { model with Islands = updatedIslands; LastMoveMs = Some nowMs }

                        return updatedModel
                    else
                        // reject move if outside outer polygon boundary
                        return model
                | _ -> return model
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
                    let size = 40.0
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

// ---------- View (lighter-weight) ----------
type bdrPgn = Template<"""<polygon class="${cs}" points="${pt}"/>""">
type bdrCrl = Template<"""<circle class="${cs}" cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
type bdcrPh = Template<"""<path id = "${pathid}" fill = "none" d="M ${sx},${sy} A ${r},${r} 0 1,1 ${ex},${ey} A ${r},${r} 0 1,1 ${sx},${sy}">""">
type bdcrTx = Template<"""
    <text id="${pth}" class="${tc}" font-size="${tf}" fill="#808080" text-anchor="middle">
      <textPath href="#${pth}" letter-spacing="0.2px" startOffset="50%">${nm}</textPath>
    </text>
    """>

let view model dispatch (js: IJSRuntime) =
    let minBound = 48.0
    let maxBound = 4800.0
    let boundScale = match model.LogicalWidth with
                     | w when w <> fst initBound -> w / fst initBound
                     | _ -> 1.0
    let boundRadius = int (float initRadius * boundScale)
    let boundLabel = int (float (initRadius + 4) * boundScale)  
    let bndTxtRr = float (initRadius + 4) * boundScale

    div {
        attr.``class`` "polygon-editor-container"

        // Controls
        div {
            attr.``class`` "boundaryInput"
            label { text "Width:" }
            input {
                attr.``type`` "number"
                attr.step "1"
                attr.min (string minBound)
                attr.max (string maxBound)
                attr.value (string model.LogicalWidth)
                on.input (fun ev ->
                    let s = string ev.Value
                    match System.Double.TryParse(s) with
                    | true, v -> dispatch (UpdateLogicalWidth v)
                    | false, _ -> () )
            }
            label { text "Height:" }
            input {
                attr.``type`` "number"
                attr.step "1"
                attr.min (string minBound)
                attr.max (string maxBound)
                attr.value (string model.LogicalHeight)
                on.input (fun ev ->
                    let s = string ev.Value
                    match System.Double.TryParse(s) with
                    | true, v -> dispatch (UpdateLogicalHeight v)
                    | false, _ -> () )
            }
        }
        // Bounding Box
        let boundingBoxWithLogical model =
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
            let (x, y, w, h) = boundingBoxWithLogical model
            let padding = 50.0
            sprintf "%f %f %f %f" (x - padding) (y - padding) (w + 2.0*padding) (h + 2.0*padding)

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
                .Elt()

            // Islands
            for island in model.Islands do
                bdrPgn()
                    .cs("islandPolygon")
                    .pt(island |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " ")
                    .Elt()

            // Outer vertices (circles). Draw texts only if ShowLabels = true
            for i = 0 to model.Outer.Length - 1 do
                let pt = model.Outer.[i]
                let id = sprintf "outerVertex-%d" i
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
                    .sy($"{pt.Y + bndTxtRr}")
                    .r($"{bndTxtRr}")
                    .ex($"{pt.X}")
                    .ey($"{pt.Y - bndTxtRr}")
                    .Elt()

                bdcrTx()
                    .pth(id)
                    .tc("outerVertexLabel")
                    .tf(boundLabel)
                    .nm(sprintf "(%0.0f, %0.0f)" pt.X pt.Y)
                    .Elt()

            // Island vertices
            for islandIdx in 0 .. model.Islands.Length - 1 do
                let island = model.Islands.[islandIdx]
                for vertexIdx in 0 .. island.Length - 1 do
                    let pt = island.[vertexIdx]
                    let id = sprintf "islandVertex-%d-%d" islandIdx vertexIdx

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
                        .sy($"{pt.Y + bndTxtRr}")
                        .r($"{bndTxtRr}")
                        .ex($"{pt.X}")
                        .ey($"{pt.Y - bndTxtRr}")
                        .Elt()

                    bdcrTx()
                        .pth(id) 
                        .tc("islandVertexLabel")
                        .tf(boundLabel)
                        .nm(sprintf "(%0.0f, %0.0f)" pt.X pt.Y)
                        .Elt()
        }
    }