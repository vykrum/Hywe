module Boundary

open System.Text.Json
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Bolero
open Bolero.Html
open System
open Microsoft.AspNetCore.Components.Web

type Point = { X: float; Y: float }

type DragInfo = { PolyIndex: int; VertexIndex: int }

type PolygonEditorModel =
    {
        LogicalWidth: float
        LogicalHeight: float
        Outer: Point list
        Islands: Point list list
        Dragging: DragInfo option
        VertexRadius: int
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

let distance a b =
    let dx = a.X - b.X
    let dy = a.Y - b.Y
    Math.Sqrt(dx * dx + dy * dy)

let isInsidePolygon (poly: Point list) (pt: Point) =
    let rec loop i j c =
        if i < List.length poly then
            let vi = List.item i poly
            let vj = List.item j poly
            let intersect =
                ((vi.Y > pt.Y) <> (vj.Y > pt.Y)) &&
                (pt.X < (vj.X - vi.X) * (pt.Y - vi.Y) / (vj.Y - vi.Y + 0.000001) + vi.X)
            loop (i + 1) i (if intersect then not c else c)
        else c
    loop 0 (List.length poly - 1) false

let isPolygonInside (outer: Point list) (inner: Point list) =
    inner |> List.forall (fun p -> isInsidePolygon outer p)

let insertAt i x lst =
    let before, after = List.splitAt i lst
    before @ [x] @ after

let initModel =
    let logicalWidth = 800.0
    let logicalHeight = 600.0
    {
        LogicalWidth = logicalWidth
        LogicalHeight = logicalHeight
        Outer =
            [   // Start at (0,0) and go clockwise
                { X = 0.0; Y = 0.0 }
                { X = logicalWidth; Y = 0.0 }
                { X = logicalWidth; Y = logicalHeight }
                { X = 0.0; Y = logicalHeight }
            ]
        Islands = []
        Dragging = None
        VertexRadius = 6
    }

let toSvgCoords (js: IJSRuntime) (ev: MouseEventArgs) : Async<Point> =
    async {
        let! result =
            js.InvokeAsync<JsonElement>("getSvgCoords", [| box "polygon-editor-svg"; box ev.ClientX; box ev.ClientY |]).AsTask()
            |> Async.AwaitTask

        let x = result.GetProperty("x").GetDouble()
        let y = result.GetProperty("y").GetDouble()
        return { X = x; Y = y }
    }

let update (js: IJSRuntime) (msg: PolygonEditorMessage) (model: PolygonEditorModel) : Async<PolygonEditorModel> =
    match msg with
    | UpdateLogicalWidth w -> async { return { model with LogicalWidth = w }}
    | UpdateLogicalHeight h -> async { return { model with LogicalHeight = h }}
    | PointerDown ev ->
        async {
            let! p = toSvgCoords js ev
            let allPolys = model.Outer :: model.Islands
            let found =
                allPolys
                |> List.mapi (fun pi poly ->
                    poly
                    |> List.mapi (fun vi v ->
                        if distance v p < float model.VertexRadius + 2.0 then Some (pi, vi) else None
                    )
                    |> List.choose id
                    |> List.tryHead
                )
                |> List.choose id
                |> List.tryHead

            match found with
            | Some (pi, vi) ->
                return { model with Dragging = Some { PolyIndex = pi; VertexIndex = vi } }
            | None ->
                let poly = model.Outer
                let (insertIndex, minDist, _) =
                    poly
                    |> List.mapi (fun i ptA ->
                        let ptB = List.item ((i + 1) % List.length poly) poly
                        let mid = { X = (ptA.X + ptB.X) / 2.0; Y = (ptA.Y + ptB.Y) / 2.0 }
                        (i, distance p mid, mid)
                    )
                    |> List.minBy (fun (_, d, _) -> d)

                if minDist < 20.0 then
                    let updated = insertAt (insertIndex + 1) p poly
                    return { model with Outer = updated }
                else
                    return model
        }

    | PointerUp ->
        async { return { model with Dragging = None } }

    | PointerMove ev ->
        async {
            match model.Dragging with
            | Some drag ->
                let! newPt = toSvgCoords js ev

                let clampPt (pt: Point) =
                    {
                        X = max 0.0 (min model.LogicalWidth pt.X)
                        Y = max 0.0 (min model.LogicalHeight pt.Y)
                    }

                if drag.PolyIndex = 0 then
                    let oldPt = model.Outer.[drag.VertexIndex]
                    let dx = newPt.X - oldPt.X
                    let dy = newPt.Y - oldPt.Y

                    let tryMoveVertex (t: float) =
                        model.Outer
                        |> List.mapi (fun i pt ->
                            if i = drag.VertexIndex then
                                clampPt { X = oldPt.X + t * dx; Y = oldPt.Y + t * dy }
                            else pt)

                    // Stepwise reduction to find max allowed drag (could be binary search too)
                    let rec findMaxT t =
                        if t < 0.01 then 0.0
                        else
                            let testPoly = tryMoveVertex t
                            let ok = model.Islands |> List.forall (fun isl -> isPolygonInside testPoly isl)
                            if ok then t
                            else findMaxT (t * 0.5)

                    let maxT = findMaxT 1.0
                    let limitedOuter = tryMoveVertex maxT

                    return { model with Outer = limitedOuter }

                else
                    let newPt = clampPt newPt
                    let updatedIslands =
                        model.Islands
                        |> List.mapi (fun pi poly ->
                            if pi = drag.PolyIndex - 1 then
                                poly |> List.mapi (fun vi pt -> if vi = drag.VertexIndex then newPt else pt)
                            else poly
                        )

                    let filtered = updatedIslands |> List.filter (fun isl -> isPolygonInside model.Outer isl)
                    return { model with Islands = filtered }
            | None -> return model
        }



    | DoubleClick ev ->
        async {
            let! p = toSvgCoords js ev
            if isInsidePolygon model.Outer p then
                let size = 40.0
                let half = size / 2.0
                let island =
                    [
                        { X = p.X - half; Y = p.Y - half }
                        { X = p.X + half; Y = p.Y - half }
                        { X = p.X + half; Y = p.Y + half }
                        { X = p.X - half; Y = p.Y + half }
                    ]
                if isPolygonInside model.Outer island then
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
                        let newPoly =
                            poly
                            |> List.mapi (fun i v -> i, v)
                            |> List.filter (fun (i, _) -> i <> vi)
                            |> List.map snd

                        if List.length newPoly >= 3 then
                            if pi = 0 then
                                updated <- { updated with Outer = newPoly }
                            else
                                let newIslands = updated.Islands |> List.mapi (fun i isl -> if i = pi - 1 then newPoly else isl)
                                updated <- { updated with Islands = newIslands }
                        elif pi > 0 then
                            let newIslands =
                                updated.Islands
                                |> List.mapi (fun i isl -> i, isl)
                                |> List.filter (fun (i, _) -> i <> pi - 1)
                                |> List.map snd
                            updated <- { updated with Islands = newIslands }
                        found <- true
            return updated
        }

    | RemoveVertex _ ->
        async { return model }

// ---------------------- View ----------------------

type bdrPgn = Template<"""<polygon points="${pt}" fill="${cl}" stroke="${st}" />""">
type bdrCrl = Template<"""<circle cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
type bdrTxt = Template<"""<text x="${tx}" y="${ty}" font-size="${tf}" font-family="Verdana" text-anchor="middle" dominant-baseline="middle" fill="#808080" opacity="1">${nm}</text>""">

let view model dispatch (js: IJSRuntime) =
    div{    
        let padding = 50.0

        let viewBox (model: PolygonEditorModel) =
            sprintf
                "%f %f %f %f"
                -padding
                -padding
                (model.LogicalWidth + 2.0 * padding)
                (model.LogicalHeight + 2.0 * padding)
        
        label{text "Width:" }
        input {
            attr.value (string model.LogicalWidth)
            on.input (fun ev ->
                let s = string ev.Value
                match System.Double.TryParse(s) with
                | true, v -> dispatch (UpdateLogicalWidth v)
                | false, _ -> ()
            )
        }

        label {text "Height:"}
        input{
            attr.value (string model.LogicalHeight)
            on.input (fun ev ->
                let s = string ev.Value
                match System.Double.TryParse(s) with
                | true, v -> dispatch (UpdateLogicalHeight v)
                | false, _ -> ()
            )
        }
        svg {
                attr.id "polygon-editor-svg"
                attr.width "100%"
                attr.height "auto"
                attr.style "display:block; border: 1px solid #ccc;background:#f0f0f0;"
                "viewBox" => (viewBox model)
                on.mousedown (fun ev -> dispatch (PointerDown ev))
                on.mouseup (fun ev -> dispatch PointerUp)
                on.mousemove (fun ev -> dispatch (PointerMove ev))
                on.dblclick (fun ev -> dispatch (DoubleClick ev))
                on.contextmenu (fun ev ->
                    dispatch (ContextMenu ev)
                )

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

                // Vertices
                for pt in model.Outer do
                    bdrCrl()
                        .cx(string pt.X)
                        .cy(string pt.Y)
                        .cr("6")
                        .cl("#333")
                        .Elt()

                    bdrTxt()
                      .tx(string (pt.X + 10.0))
                      .ty(string (pt.Y - 10.0))
                      .nm(sprintf "(%.0f, %.0f)" pt.X pt.Y)
                      .Elt()

                for island in model.Islands do
                    for pt in island do
                        bdrCrl()
                            .cx(string pt.X)
                            .cy(string pt.Y)
                            .cr("6")
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