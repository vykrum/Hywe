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

type PolygonEditorModel =
    {
        LogicalWidth: float
        LogicalHeight: float
        Outer: Point list
        Islands: Point list list
        VertexRadius: int
        Dragging: DragInfo option
        DragStart: Point option
        MouseStart: Point option
        History: PolygonEditorModel list
        Future: PolygonEditorModel list
    }

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

// ---------- Geometry helpers ----------
let distance a b =
    let dx = a.X - b.X
    let dy = a.Y - b.Y
    Math.Sqrt(dx * dx + dy * dy)

let distancePointToSegment p a b =
    let vx = b.X - a.X
    let vy = b.Y - a.Y
    let wx = p.X - a.X
    let wy = p.Y - a.Y
    let c1 = wx * vx + wy * vy
    if c1 <= 0.0 then distance p a
    else
        let c2 = vx * vx + vy * vy
        if c2 <= c1 then distance p b
        else
            let t = c1 / c2
            distance p { X = a.X + t*vx; Y = a.Y + t*vy }

let isInsidePolygon (poly: Point list) (pt: Point) =
    let mutable c = false
    let n = List.length poly
    for i in 0..n-1 do
        let j = (i + n - 1) % n
        let vi = poly.[i]
        let vj = poly.[j]
        if (vi.Y > pt.Y) <> (vj.Y > pt.Y) then
            let xIntersect = (vj.X - vi.X)*(pt.Y - vi.Y)/(vj.Y - vi.Y) + vi.X
            if pt.X < xIntersect then
                c <- not c
    c

let isPolygonInside outer inner =
    inner |> List.forall (isInsidePolygon outer)

let insertAt i x lst =
    let before, after = List.splitAt i lst
    before @ (x::after)

let clampPt (model: PolygonEditorModel) (pt: Point) =
    {
        X = max 0.0 (min model.LogicalWidth pt.X)
        Y = max 0.0 (min model.LogicalHeight pt.Y)
    }

// Create a snapshot for undo history
let snapshot (model: PolygonEditorModel) : PolygonEditorModel =
    let cleanModel = { model with History = []; Future = [] }
    { model with
        History = cleanModel :: model.History
        Future = []
        Dragging = None
        DragStart = None
        MouseStart = None }

// ---------- Initial Model ----------
let initModel =
    let logicalWidth, logicalHeight = 800.0, 600.0
    {
        LogicalWidth = logicalWidth
        LogicalHeight = logicalHeight
        Outer =
            [ { X = 0.0; Y = 0.0 }
              { X = logicalWidth; Y = 0.0 }
              { X = logicalWidth; Y = logicalHeight }
              { X = 0.0; Y = logicalHeight } ]
        Islands = []
        Dragging = None
        DragStart = None
        MouseStart = None
        VertexRadius = 6
        History = []
        Future = []
    }

// Convert mouse event to SVG coordinates via JS
let toSvgCoords (js: IJSRuntime) (ev: MouseEventArgs) : Async<Point> =
    async {
        let! result =
            js.InvokeAsync<JsonElement>("getSvgCoords", [| box "polygon-editor-svg"; box ev.ClientX; box ev.ClientY |]).AsTask()
            |> Async.AwaitTask

        return {
            X = result.GetProperty("x").GetDouble()
            Y = result.GetProperty("y").GetDouble()
        }
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

    | PointerDown ev ->
        async {
            let! svgPt = toSvgCoords js ev
            let drag =
                model.Outer
                |> List.tryFindIndex (fun v -> distance v svgPt < float model.VertexRadius + 2.0)
                |> Option.map (fun vi -> { PolyIndex = 0; VertexIndex = vi })

            let newModel =
                { (snapshot model) with
                    Dragging = drag
                    DragStart = Some svgPt
                    MouseStart = Some { X = ev.ClientX; Y = ev.ClientY } }

            return newModel
        }
    | PointerUp -> async { return { model with Dragging = None; DragStart = None; MouseStart = None } }

    | PointerMove ev ->
        async {
            match model.Dragging, model.DragStart, model.MouseStart with
            | Some drag, Some dragStart, Some mouseStart ->
                let dx = ev.ClientX - mouseStart.X
                let dy = ev.ClientY - mouseStart.Y
                let newPt = clampPt model { X = dragStart.X + dx; Y = dragStart.Y + dy }

                let newModel =
                    if drag.PolyIndex = 0 then
                        let updatedOuter =
                            model.Outer
                            |> List.mapi (fun i pt -> if i = drag.VertexIndex then newPt else pt)
                        { model with Outer = updatedOuter }
                    else
                        let updatedIslands =
                            model.Islands
                            |> List.mapi (fun pi poly ->
                                if pi = drag.PolyIndex - 1 then
                                    poly |> List.mapi (fun vi pt -> if vi = drag.VertexIndex then newPt else pt)
                                else poly)
                        { model with Islands = updatedIslands }

                return newModel
            | _ -> return model
        }


    | DoubleClick ev ->
        async {
            let! p = toSvgCoords js ev
            if isInsidePolygon model.Outer p then
                let size = 40.0
                let half = size / 2.0
                let island =
                    [ { X = p.X-half; Y = p.Y-half }
                      { X = p.X+half; Y = p.Y-half }
                      { X = p.X+half; Y = p.Y+half }
                      { X = p.X-half; Y = p.Y+half } ]
                if isPolygonInside model.Outer island then
                    let model = snapshot model
                    return { model with Islands = island :: model.Islands }
                else return model
            else return model
        }

    | ContextMenu ev ->
        async {
            let! p = toSvgCoords js ev
            let allPolys = model.Outer :: model.Islands

            let mutable updated = model
            let mutable found = false

            for pi, poly in List.indexed allPolys do
                for vi, vtx in List.indexed poly do
                    if not found && distance vtx p < float model.VertexRadius + 2.0 then
                        let newPoly = poly |> List.mapi (fun i v -> i, v) |> List.filter (fun (i,_) -> i<>vi) |> List.map snd
                        updated <- snapshot updated
                        if pi = 0 then
                            if List.length newPoly >= 3 then updated <- { updated with Outer = newPoly }
                        else
                            if List.length newPoly >= 3 then
                                updated <- { updated with Islands = updated.Islands |> List.mapi (fun i isl -> if i=pi-1 then newPoly else isl) }
                            else
                                updated <- { updated with Islands = updated.Islands |> List.mapi (fun i isl -> i,isl) |> List.filter (fun (i,_) -> i<>pi-1) |> List.map snd }
                        found <- true
            return updated
        }

    | RemoveVertex _ -> async { return model }

// ---------- View ----------
type bdrPgn = Template<"""<polygon points="${pt}" fill="${cl}" stroke="${st}" />""">
type bdrCrl = Template<"""<circle cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
type bdrTxt = Template<"""<text x="${tx}" y="${ty}" font-size="${tf}" font-family="Verdana" text-anchor="middle" dominant-baseline="middle" fill="#808080">${nm}</text>""">

let view model dispatch (js: IJSRuntime) =
    div {
        attr.``class`` "polygon-editor-container"

        // Width & Height Controls
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
                    | false, _ -> ()
                )
            }

            label { text "Height:" }
            input {
                attr.value (string model.LogicalHeight)
                attr.style "width: 70px;"
                on.input (fun ev ->
                    let s = string ev.Value
                    match System.Double.TryParse(s) with
                    | true, v -> dispatch (UpdateLogicalHeight v)
                    | false, _ -> ()
                )
            }
        }

        // SVG Editor
        svg {
            attr.id "polygon-editor-svg"
            attr.``class`` "polygon-editor-svg"
            "viewBox" => sprintf
                "%f %f %f %f"
                -50.0
                -50.0
                (model.LogicalWidth + 100.0)
                (model.LogicalHeight + 100.0)

            on.mousedown (fun ev -> dispatch (PointerDown ev))
            on.mouseup (fun _ -> dispatch PointerUp)
            on.mousemove (fun ev -> dispatch (PointerMove ev))
            on.dblclick (fun ev -> dispatch (DoubleClick ev))
            on.contextmenu (fun ev -> dispatch (ContextMenu ev))

            // Outer polygon
            bdrPgn()
                .pt(model.Outer |> List.map (fun p -> $"{p.X},{p.Y}") |> String.concat " ")
                .cl("#cccccc")
                .st("#333")
                .Elt()

            // Islands
            for island in model.Islands do
                bdrPgn()
                    .pt(island |> List.map (fun p -> $"{p.X},{p.Y}") |> String.concat " ")
                    .cl("#ffffff")
                    .st("#333")
                    .Elt()

            // Outer vertices with labels
            for pt in model.Outer do
                bdrCrl()
                    .cx(string pt.X)
                    .cy(string pt.Y)
                    .cr(string model.VertexRadius)
                    .cl("#333")
                    .Elt()

                bdrTxt()
                    .tx(string (pt.X + 10.0))
                    .ty(string (pt.Y - 10.0))
                    .tf("11")
                    .nm(sprintf "(%.0f, %.0f)" pt.X pt.Y)
                    .Elt()

            // Island vertices with labels
            for island in model.Islands do
                for pt in island do
                    bdrCrl()
                        .cx(string pt.X)
                        .cy(string pt.Y)
                        .cr(string model.VertexRadius)
                        .cl("#444")
                        .Elt()

                    bdrTxt()
                        .tx(string (pt.X + 10.0))
                        .ty(string (pt.Y - 10.0))
                        .tf("10")
                        .nm(sprintf "(%.0f, %.0f)" pt.X pt.Y)
                        .Elt()
        }
    }

