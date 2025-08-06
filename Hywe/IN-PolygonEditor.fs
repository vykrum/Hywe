module PolygonEditor

open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Web
open Bolero.Html
open Bolero
open Elmish
open System

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
    }

type PolygonEditorMessage =
    | UpdateLogicalWidth of float
    | UpdateLogicalHeight of float
    | PointerDown of MouseEventArgs
    | PointerUp
    | PointerMove of MouseEventArgs
    | DoubleClick of MouseEventArgs
    | ContextMenu of MouseEventArgs

// ---------- Geometry helpers ----------
let distance a b =
    let dx = a.X - b.X
    let dy = a.Y - b.Y
    Math.Sqrt(dx*dx + dy*dy)

let clampPt (model: PolygonEditorModel) (pt: Point) =
    { X = max 0.0 (min model.LogicalWidth pt.X)
      Y = max 0.0 (min model.LogicalHeight pt.Y) }

// ---------- Initial Model ----------
let initModel =
    let w, h = 800.0, 600.0
    {
        LogicalWidth = w
        LogicalHeight = h
        Outer =
            [ { X = 0.0; Y = 0.0 }
              { X = w; Y = 0.0 }
              { X = w; Y = h }
              { X = 0.0; Y = h } ]
        Islands = []
        VertexRadius = 6
        Dragging = None
        DragStart = None
        MouseStart = None
    }

// ---------- Update ----------
let update (msg: PolygonEditorMessage) (model: PolygonEditorModel) : PolygonEditorModel =
    match msg with
    | UpdateLogicalWidth w -> { model with LogicalWidth = w }
    | UpdateLogicalHeight h -> { model with LogicalHeight = h }

    | PointerDown ev ->
        // Start dragging using client coordinates
        let mousePt = { X = ev.ClientX; Y = ev.ClientY }
        let drag =
            model.Outer
            |> List.tryFindIndex (fun v -> distance v mousePt < float model.VertexRadius + 2.0)
            |> Option.map (fun vi -> { PolyIndex = 0; VertexIndex = vi })
        { model with Dragging = drag; DragStart = Some mousePt; MouseStart = Some mousePt }

    | PointerMove ev ->
        match model.Dragging, model.DragStart, model.MouseStart with
        | Some drag, Some dragStart, Some mouseStart ->
            let dx = ev.ClientX - mouseStart.X
            let dy = ev.ClientY - mouseStart.Y
            let newPt = clampPt model { X = dragStart.X + dx; Y = dragStart.Y + dy }

            if drag.PolyIndex = 0 then
                { model with
                    Outer = model.Outer |> List.mapi (fun i pt -> if i = drag.VertexIndex then newPt else pt) }
            else
                { model with
                    Islands =
                        model.Islands
                        |> List.mapi (fun pi poly ->
                            if pi = drag.PolyIndex - 1 then
                                poly |> List.mapi (fun vi pt -> if vi = drag.VertexIndex then newPt else pt)
                            else poly) }
        | _ -> model

    | PointerUp ->
        { model with Dragging = None; DragStart = None; MouseStart = None }

    | DoubleClick _ -> model
    | ContextMenu _ -> model

// ---------- View ----------
type bdrPgn = Template<"""<polygon points="${pt}" fill="${cl}" stroke="${st}" />""">
type bdrCrl = Template<"""<circle cx="${cx}" cy="${cy}" r="${cr}" fill="${cl}" />""">
type bdrTxt = Template<"""<text x="${tx}" y="${ty}" font-size="${tf}" font-family="Verdana" text-anchor="middle" dominant-baseline="middle" fill="#808080">${nm}</text>""">

let view model dispatch =
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
                    | _ -> ())
            }

            label { text "Height:" }
            input {
                attr.value (string model.LogicalHeight)
                attr.style "width: 70px;"
                on.input (fun ev ->
                    let s = string ev.Value
                    match System.Double.TryParse(s) with
                    | true, v -> dispatch (UpdateLogicalHeight v)
                    | _ -> ())
            }
        }

        // SVG Editor
        svg {
            attr.id "polygon-editor-svg"
            attr.``class`` "polygon-editor-svg"
            "viewBox" => sprintf "%f %f %f %f" -50.0 -50.0 (model.LogicalWidth+100.0) (model.LogicalHeight+100.0)

            on.mousedown (fun ev -> dispatch (PointerDown ev))
            on.mouseup (fun _ -> dispatch PointerUp)
            on.mousemove (fun ev -> dispatch (PointerMove ev))

            // Outer polygon
            bdrPgn()
                .pt(model.Outer |> List.map (fun p -> $"{p.X},{p.Y}") |> String.concat " ")
                .cl("#cccccc").st("#333")
                .Elt()

            // Outer vertices with labels
            for pt in model.Outer do
                bdrCrl().cx(string pt.X).cy(string pt.Y).cr(string model.VertexRadius).cl("#333").Elt()
                bdrTxt().tx(string (pt.X+10.0)).ty(string (pt.Y-10.0)).tf("11").nm(sprintf "(%.0f, %.0f)" pt.X pt.Y).Elt()
        }
    }
