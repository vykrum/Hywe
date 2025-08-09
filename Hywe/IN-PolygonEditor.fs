module PolygonEditor

open Bolero.Html
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Web
open Microsoft.JSInterop
open Bridge

type Point = { X: float; Y: float }

type DragInfo = { VertexIndex: int }

type PolygonEditorModel = {
    Outer: Point list
    Dragging: DragInfo option
    SvgRef: ElementReference
}

type PolygonEditorMessage =
    | SetSvgRef of ElementReference
    | PointerDownAt of Point
    | PointerMoveAt of Point
    | PointerUp

let hitRadius = 15.0
let mutable svgRef = Unchecked.defaultof<ElementReference>

let convertClientToSvgCoords (jsRuntime: IJSRuntime) (svgRef: ElementReference) (clientX: float) (clientY: float) : Async<Point> =
    jsRuntime.InvokeAsync<Point>("clientToSvgPoint", svgRef, clientX, clientY).AsTask() |> Async.AwaitTask

let initModel: PolygonEditorModel =
    {
        Outer = [ { X = 100.0; Y = 100.0 }; { X = 700.0; Y = 100.0 }; { X = 700.0; Y = 500.0 }; { X = 100.0; Y = 500.0 } ]
        Dragging = None
        SvgRef = Unchecked.defaultof<ElementReference>
    }

let update (msg: PolygonEditorMessage) (model: PolygonEditorModel) : PolygonEditorModel =
    match msg with
    | SetSvgRef svgRef -> { model with SvgRef = svgRef }

    | PointerDownAt pt ->
        let hitVertex =
            model.Outer
            |> List.mapi (fun i v -> (i, v))
            |> List.tryFind (fun (_, v) ->
                let dx = v.X - pt.X
                let dy = v.Y - pt.Y
                (dx * dx + dy * dy) <= (hitRadius * hitRadius)
            )
        match hitVertex with
        | Some (idx, _) -> { model with Dragging = Some { VertexIndex = idx } }
        | None -> model

    | PointerMoveAt pt ->
        match model.Dragging with
        | Some dragInfo ->
            let updatedOuter =
                model.Outer
                |> List.mapi (fun i v ->
                    if i = dragInfo.VertexIndex then { X = pt.X; Y = pt.Y } else v
                )
            { model with Outer = updatedOuter }
        | None -> model

    | PointerUp -> { model with Dragging = None }

let view (model: PolygonEditorModel) (dispatch: PolygonEditorMessage -> unit) (js : IJSRuntime)=
    let padding = 20.0

    let xs = model.Outer |> List.map (fun p -> p.X)
    let ys = model.Outer |> List.map (fun p -> p.Y)
    let minX = xs |> List.min
    let maxX = xs |> List.max
    let minY = ys |> List.min
    let maxY = ys |> List.max

    let viewBoxX = minX - padding
    let viewBoxY = minY - padding
    let viewBoxWidth = (maxX - minX) + 2.0 * padding
    let viewBoxHeight = (maxY - minY) + 2.0 * padding

    let outerPointsStr = model.Outer |> List.map (fun p -> $"{p.X},{p.Y}") |> String.concat " "

    let vertexElements =
        model.Outer
        |> List.mapi (fun i p ->
            let x = p.X
            let y = p.Y
            let r = 10.0
            let pathId = $"arcPath{i}"
            let label = $"V{i}"
            let color = if i = 0 then "#ff5555" else "#5555ff"

            [
                // Arc path for label positioning
                crPh()
                    .pathid(pathId)
                    .sx($"{x}")
                    .sy($"{y + r}")
                    .r($"{r}")
                    .ex($"{x}")
                    .ey($"{y - r}")
                    .Elt()

                // Label text
                crTx()
                    .nm(label)
                    .pth(pathId)
                    .fw(if i = 0 then "700" else "400")
                    .fl(if i = 0 then "#333333" else "#666666")
                    .td("none")
                    .Elt()

                // Vertex circle
                crCl()
                    .cx($"{x}")
                    .cy($"{y}")
                    .cl(color)
                    .Elt()
            ]
        )
        |> List.concat

    let onPointerDown (ev: PointerEventArgs) : unit =
        async {
            if model.SvgRef.Id <> null then
                let! pt = convertClientToSvgCoords js model.SvgRef ev.ClientX ev.ClientY
                printfn $"PointerDown at SVG coords: {pt.X}, {pt.Y}"
                dispatch (PointerDownAt pt)
        } |> Async.StartImmediate

    let onPointerMove (ev: PointerEventArgs) : unit =
        async {
            if model.SvgRef.Id <> null then
                let! pt = convertClientToSvgCoords js model.SvgRef ev.ClientX ev.ClientY
                printfn $"PointerMove at SVG coords: {pt.X}, {pt.Y}"
                dispatch (PointerMoveAt pt)
        } |> Async.StartImmediate

    let onPointerUp (_: PointerEventArgs) : unit =
        dispatch PointerUp

    svg {
        yield attr.width "800"
        yield attr.height "600"
        yield "viewBox" => $"{viewBoxX} {viewBoxY} {viewBoxWidth} {viewBoxHeight}"
        yield attr.style "border: 1px solid #999; touch-action: none;"

        yield on.pointerdown onPointerDown
        yield on.pointermove onPointerMove
        yield on.pointerup onPointerUp

        //yield attr.ref &svgRef

        yield
            plgn()
                .pt(outerPointsStr)
                .cl("#cccccc")
                .Elt()

        for el in vertexElements do
            yield el
    }
