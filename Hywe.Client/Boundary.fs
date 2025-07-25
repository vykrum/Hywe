module Boundary

open Bolero
open Bolero.Html

type Vertex = { X: float; Y: float }

type BoundaryModel = {
    Vertices: Vertex list
    DragIndex: int option
    Width: float
    Height: float
}

type BoundaryMsg =
    | StartDrag of int
    | DragTo of float * float
    | EndDrag
    | SetWidth of float
    | SetHeight of float

let ccw (A: Vertex) (B: Vertex) (C: Vertex) =
    (C.Y - A.Y) * (B.X - A.X) > (B.Y - A.Y) * (C.X - A.X)

let segmentsIntersect (A: Vertex) (B: Vertex) (C: Vertex) (D: Vertex) =
    ccw A C D <> ccw B C D && ccw A B C <> ccw A B D

let isSelfIntersecting (verts: Vertex list) =
    let count = List.length verts
    let edges =
        [ for i in 0 .. count - 1 ->
            let a = List.item i verts
            let b = List.item ((i + 1) % count) verts
            (i, a, b) ]
    edges
    |> List.exists (fun (i, a1, a2) ->
        edges
        |> List.exists (fun (j, b1, b2) ->
            let adjacent =
                abs (i - j) <= 1 || abs (i - j) = count - 1
            not adjacent && segmentsIntersect a1 a2 b1 b2
        )
    )

let clamp lo hi v = min hi (max lo v)
let round2 (v: float) = System.Math.Round(v, 2)

let initModel = {
    Vertices = [
        { X = 0.0; Y = 0.0 }
        { X = 200.0; Y = 0.0 }
        { X = 200.0; Y = 100.0 }
        { X = 0.0; Y = 100.0 }
    ]
    DragIndex = None
    Width = 200.0
    Height = 100.0
}

let update msg model =
    match msg with
    | StartDrag index ->
        { model with DragIndex = Some index }

    | DragTo (x, y) ->
        match model.DragIndex with
        | Some i ->
            let clampedX = clamp 0.0 model.Width (round2 x)
            let clampedY = clamp 0.0 model.Height (round2 y)
            let updated =
                model.Vertices
                |> List.mapi (fun j v -> if j = i then { X = clampedX; Y = clampedY } else v)
            if isSelfIntersecting updated then model
            else { model with Vertices = updated }
        | None -> model

    | EndDrag ->
        { model with DragIndex = None }

    | SetWidth w ->
        let w = max 10.0 w
        let newVerts =
            [0.0, 0.0; w, 0.0; w, model.Height; 0.0, model.Height]
            |> List.map (fun (x, y) -> { X = x; Y = y })
        { model with Width = w; Vertices = newVerts }

    | SetHeight h ->
        let h = max 10.0 h
        let newVerts =
            [0.0, 0.0; model.Width, 0.0; model.Width, h; 0.0, h]
            |> List.map (fun (x, y) -> { X = x; Y = y })
        { model with Height = h; Vertices = newVerts }

type bdPgn = Template<"""
    <polygon 
        points="${pt}" 
        fill="${cl}"
        stroke="${st}" />
""">

type bdCrl = Template<"""
    <circle 
        cx="${cx}" 
        cy="${cy}" 
        r="5" 
        fill="${cl}"
        style="cursor:${cr}"
        onmousedown="${ev}" />
""">

type bdTxt = Template<"""
    <text 
        x="${tx}" 
        y="${ty}"
        font-size="10px"
        font-family="Verdana"
        text-anchor="middle"
        dominant-baseline="middle"
        fill="#808080"
        opacity="1">${nm}</text>
""">

let view (model: BoundaryModel) dispatch =
    let padding = 20.0
    let vbX = -padding
    let vbY = -padding
    let vbW = model.Width + 2.0 * padding
    let vbH = model.Height + 2.0 * padding
    let viewBox = sprintf "%f %f %f %f" vbX vbY vbW vbH

    let pointString =
        model.Vertices
        |> List.map (fun v -> $"{v.X},{v.Y}")
        |> String.concat " "

    div {
        attr.style "width: 100%; padding: 0 2rem; box-sizing: border-box;"

        div {
            attr.style "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 1rem;"

            label {
                attr.``for`` "width-input"
                text "Width:"
            }
            input {
                attr.id "width-input"
                attr.``type`` "number"
                attr.min "10"
                attr.value (string model.Width)
                on.input (fun e ->
                    match System.Double.TryParse(string e.Value) with
                    | true, v -> dispatch (SetWidth v)
                    | _ -> ()
                )
                attr.style "width: 80px"
            }

            label {
                attr.``for`` "height-input"
                text "Height:"
            }
            input {
                attr.id "height-input"
                attr.``type`` "number"
                attr.min "10"
                attr.value (string model.Height)
                on.input (fun e ->
                    match System.Double.TryParse(string e.Value) with
                    | true, v -> dispatch (SetHeight v)
                    | _ -> ()
                )
                attr.style "width: 80px"
            }
        }

        svg {
            attr.style "width: 100%; height: auto; display: block; border: 1px solid #ccc;"
            "viewBox" => viewBox
            "preserveAspectRatio" => "xMidYMid meet"

            on.mousemove (fun e ->
                let dx = float e.MovementX / 1.5
                let dy = float e.MovementY / 1.5
                dispatch (DragTo (dx, dy))
            )
            on.mouseup (fun _ -> dispatch EndDrag)

            bdPgn()
                .pt(pointString)
                .cl("steelblue")
                .st("black")
                .Elt()

            for i, v in List.indexed model.Vertices do
                bdCrl()
                    .cx(string v.X)
                    .cy(string v.Y)
                    .cl("red")
                    .cr("move")
                    .ev((fun _ -> dispatch (StartDrag i)))
                    .Elt()

                bdTxt()
                    .tx(string (v.X + 8.0))
                    .ty(string (v.Y - 8.0))
                    .nm($"({round2 v.X},{round2 v.Y})")
                    .Elt()
        }
    }


