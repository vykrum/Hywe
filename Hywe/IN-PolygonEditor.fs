module PolygonEditor

open System.Text.Json
open Microsoft.AspNetCore.Components
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
        ShowLabels: bool              // avoid rendering labels during drag
        History: PolygonEditorModel list
        Future: PolygonEditorModel list
    }

// Only minimal messages needed for editing; kept structure similar to original
type PolygonEditorMessage =
    | UpdateLogicalWidth of float
    | UpdateLogicalHeight of float
    | PointerDown of MouseEventArgs
    | PointerUp
    | PointerMove of MouseEventArgs
    | DoubleClick of MouseEventArgs
    | ContextMenu of MouseEventArgs
    | RemoveVertex of int * int
    | Undo
    | Redo
    | ToggleLabels

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

let clampPt (model: PolygonEditorModel) (pt: Point) =
    {
        X = max 0.0 (min model.LogicalWidth pt.X)
        Y = max 0.0 (min model.LogicalHeight pt.Y)
    }

let snapshot (model: PolygonEditorModel) : PolygonEditorModel =
    let cleanModel = { model with History = []; Future = [] }
    { model with History = cleanModel :: model.History; Future = []; Dragging = None; DragOffset = None }

// ---------- Initial Model ----------
let initModel =
    let logicalWidth, logicalHeight = 800.0, 600.0
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
        ShowLabels = false
        VertexRadius = 6
        History = []
        Future = []
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
    | UpdateLogicalWidth w -> async { return { model with LogicalWidth = w }}
    | UpdateLogicalHeight h -> async { return { model with LogicalHeight = h }}

    | Undo ->
        async {
            match model.History with
            | prev::rest ->
                let currentClean = { model with History = []; Future = [] }
                return { prev with History = rest; Future = currentClean :: model.Future }
            | [] -> return model
        }

    | Redo ->
        async {
            match model.Future with
            | next::rest ->
                let currentClean = { model with History = []; Future = [] }
                return { next with History = currentClean :: model.History; Future = rest }
            | [] -> return model
        }

    | ToggleLabels -> async { return { model with ShowLabels = not model.ShowLabels } }

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
                return { newModel with Dragging = Some d; DragOffset = Some offset; SvgInfo = Some info; ShowLabels = false }
            | None ->
                // no drag — keep svg info cache for quick mapping later
                return { model with SvgInfo = Some info }
        }

    | PointerUp -> async { return { model with Dragging = None; DragOffset = None; LastMoveMs = None; ShowLabels = true } }

    | PointerMove ev ->
        async {
            // Throttle: simple time-based gate (~60fps)
            let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
            match model.LastMoveMs with
            | Some last when nowMs - last < 16.0 -> return model
            | _ ->
                match model.Dragging, model.DragOffset, model.SvgInfo with
                | Some drag, Some offset, Some info ->
                    // compute svg point from client coords without calling JS
                    let svgPt = toSvgCoordsFromInfo info (float ev.ClientX) (float ev.ClientY)
                    let newPt = clampPt model { X = svgPt.X - offset.X; Y = svgPt.Y - offset.Y }

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
                | _ -> return model
        }

    | DoubleClick ev ->
        async {
            let! p = toSvgCoords js ev
            if isInsidePolygon model.Outer p then
                let size = 40.0
                let half = size / 2.0
                let island = [| { X = p.X-half; Y = p.Y-half }
                                { X = p.X+half; Y = p.Y-half }
                                { X = p.X+half; Y = p.Y+half }
                                { X = p.X-half; Y = p.Y+half } |]
                if isPolygonInside model.Outer island then
                    let model = snapshot model
                    return { model with Islands = Array.append [| island |] model.Islands }
                else return model
            else return model
        }

    | ContextMenu ev ->
        async {
            let! p = toSvgCoords js ev
            let allPolys = Array.append [| model.Outer |] model.Islands
            let mutable updated = model
            let mutable found = false
            for pi = 0 to allPolys.Length - 1 do
                let poly = allPolys.[pi]
                for vi = 0 to poly.Length - 1 do
                    if not found && withinRadiusSq poly.[vi] p (float model.VertexRadius + 2.0) then
                        let newPoly = Array.init (poly.Length - 1) (fun idx ->
                            // copy all but vi
                            if idx < vi then poly.[idx] else poly.[idx+1])
                        updated <- snapshot updated
                        if pi = 0 then
                            if newPoly.Length >= 3 then updated <- { updated with Outer = newPoly }
                        else
                            if newPoly.Length >= 3 then
                                let arr = Array.copy updated.Islands
                                arr.[pi-1] <- newPoly
                                updated <- { updated with Islands = arr }
                            else
                                // remove entire island
                                let arr = updated.Islands |> Array.mapi (fun i v -> i,v) |> Array.filter (fun (i,_) -> i<>pi-1) |> Array.map snd
                                updated <- { updated with Islands = arr }
                        found <- true
            return updated
        }

    | RemoveVertex _ -> async { return model }

// ---------- View (lighter-weight) ----------
type bdrPgn = Template<"""<polygon points="${pt}" fill="${cl}" stroke="${st}" />""">
type bdrCrl = Template<"""<circle cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
type bdrTxt = Template<"""<text x="${tx}" y="${ty}" font-size="${tf}" font-family="Verdana" text-anchor="middle" dominant-baseline="middle" fill="#808080">${nm}</text>""">

let view model dispatch (js: IJSRuntime) =
    div {
        attr.``class`` "polygon-editor-container"

        // Controls
        div {
            attr.style "margin-bottom: 6px; display: flex; gap: 10px; align-items: center;"
            label { text "Width:" }
            input {
                attr.value (string model.LogicalWidth)
                attr.style "width: 70px;"
                on.input (fun ev ->
                    let s = string ev.Value
                    match System.Double.TryParse(s) with
                    | true, v -> dispatch (UpdateLogicalWidth v)
                    | false, _ -> () )
            }
            label { text "Height:" }
            input {
                attr.value (string model.LogicalHeight)
                attr.style "width: 70px;"
                on.input (fun ev ->
                    let s = string ev.Value
                    match System.Double.TryParse(s) with
                    | true, v -> dispatch (UpdateLogicalHeight v)
                    | false, _ -> () )
            }
            button { on.click (fun _ -> dispatch ToggleLabels); text (if model.ShowLabels then "Hide labels" else "Show labels") }
        }

        svg {
            attr.id "polygon-editor-svg"
            attr.``class`` "polygon-editor-svg"
            "viewBox" => (sprintf "%f %f %f %f" -50.0 -50.0 (model.LogicalWidth + 100.0) (model.LogicalHeight + 100.0))

            // Mouse events
            on.mousedown (fun ev -> dispatch (PointerDown ev))
            on.mouseup (fun _ -> dispatch PointerUp)
            on.mousemove (fun ev -> dispatch (PointerMove ev))
            on.dblclick (fun ev -> dispatch (DoubleClick ev))
            on.contextmenu (fun ev -> dispatch (ContextMenu ev))

            // Touch events
            on.touchstart (fun tev -> 
                let firstTouch = tev.Touches |> Seq.tryHead
                match firstTouch with
                | Some t -> 
                    let mev = MouseEventArgs(ClientX = t.ClientX, ClientY = t.ClientY)
                    dispatch (PointerDown mev)
                | None -> ())
            on.touchmove (fun tev -> 
                let firstTouch = tev.Touches |> Seq.tryHead
                match firstTouch with
                | Some t -> 
                    let mev = MouseEventArgs(ClientX = t.ClientX, ClientY = t.ClientY)
                    dispatch (PointerMove mev)
                | None -> ())
            on.touchend (fun _ -> dispatch PointerUp)

            // Outer polygon
            bdrPgn()
                .pt(model.Outer |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " ")
                .cl("#cccccc")
                .st("#333")
                .Elt()

            // Islands
            for island in model.Islands do
                bdrPgn()
                    .pt(island |> Array.map (fun p -> sprintf "%.1f,%.1f" p.X p.Y) |> String.concat " ")
                    .cl("#ffffff")
                    .st("#333")
                    .Elt()

            // Outer vertices (circles). Draw texts only if ShowLabels = true
            for i = 0 to model.Outer.Length - 1 do
                let pt = model.Outer.[i]
                bdrCrl()
                    .cx(sprintf "%.1f" pt.X)
                    .cy(sprintf "%.1f" pt.Y)
                    .cr(string model.VertexRadius)
                    .cl("#333")
                    .Elt()
                if model.ShowLabels then
                    bdrTxt()
                        .tx(sprintf "%.1f" (pt.X + 10.0))
                        .ty(sprintf "%.1f" (pt.Y - 10.0))
                        .tf("11")
                        .nm(sprintf "(%0.0f, %0.0f)" pt.X pt.Y)
                        .Elt()

            // Island vertices
            for island in model.Islands do
                for pt in island do
                    bdrCrl()
                        .cx(sprintf "%.1f" pt.X)
                        .cy(sprintf "%.1f" pt.Y)
                        .cr(string model.VertexRadius)
                        .cl("#444")
                        .Elt()
                    if model.ShowLabels then
                        bdrTxt()
                            .tx(sprintf "%.1f" (pt.X + 10.0))
                            .ty(sprintf "%.1f" (pt.Y - 10.0))
                            .tf("10")
                            .nm(sprintf "(%0.0f, %0.0f)" pt.X pt.Y)
                            .Elt()
        }
    }
