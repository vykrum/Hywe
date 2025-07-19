module TreeSvg 

open System
open Bolero
open Bolero.Html


// Templates
type svLn = Template<"""
            <line
                x1="${x1}"
                y1="${y1}"
                x2="${x2}"
                y2="${y2}"
                stroke="${st}"
                />
            """>


type svCl = Template<
        """<circle
            cx="${cx}" 
            cy="${cy}" 
            r="${rd}" 
            fill="${fl}"
            stroke="${st}"
            />
        """>


type svTx = Template<
        """<text 
        x="${x}" 
        y="${y}"
        font-size = "${sz}"
        text-anchor="${ta}"
        >${nm}</text> """>

               

// -------------------------------
// 1. TreeNode type
// -------------------------------
type TreeNode = {
    Id: string
    Label: string
    Weight: int
    Children: TreeNode list
}

// -------------------------------
// 2. Safe input parser (Result-based)
// -------------------------------
let parseTreeInput (input: string) : Result<(string list * int * string) list, string> =
    try
        input.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun item ->
            let trimmed = item.Trim('(', ')')
            if trimmed.StartsWith("0/") then
                None  // Skip metadata node
            else
                let parts = trimmed.Split('/')
                if parts.Length = 3 then
                    let path = parts.[0].Split('.') |> Array.toList
                    match Int32.TryParse(parts.[1]) with
                    | true, weight ->
                        let label = parts.[2]
                        Some (Ok (path, weight, label))
                    | false, _ -> Some (Error $"Invalid weight: {parts.[1]}")
                else
                    Some (Error $"Malformed entry: {item}")
        )
        |> Array.toList
        |> List.fold (fun acc item ->
            match acc, item with
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Ok lst, Ok x -> Ok (x :: lst)
        ) (Ok [])
        |> Result.map List.rev
    with e ->
        Error e.Message

// -------------------------------
// 3. Tree construction from flat paths
// -------------------------------
let rec insertNode (path: string list) (weight: int) (label: string) (tree: TreeNode list) : TreeNode list =
    match path with
    | [] -> tree
    | [id] ->
        if tree |> List.exists (fun n -> n.Id = id) then tree
        else { Id = id; Label = label; Weight = weight; Children = [] } :: tree
    | id :: rest ->
        let updated =
            match tree |> List.tryFind (fun n -> n.Id = id) with
            | Some node ->
                let newChildren = insertNode rest weight label node.Children
                { node with Children = newChildren }
            | None ->
                let childTree = insertNode rest weight label []
                { Id = id; Label = ""; Weight = 0; Children = childTree }
        tree
        |> List.filter (fun n -> n.Id <> id)
        |> List.append [updated]

let buildTree (tuples: (string list * int * string) list) : TreeNode list =
    tuples |> List.fold (fun acc (path, weight, label) ->
        insertNode path weight label acc
    ) []

// -------------------------------
// 4. Coordinate layout
// -------------------------------
type PositionedNode = {
    Node: TreeNode
    X: float
    Y: float
    Children: PositionedNode list
}

let rec layoutTree (x: float) (y: float) (hGap: float) (vGap: float) (node: TreeNode) : PositionedNode * float =
    match node.Children with
    | [] ->
        ({ Node = node; X = x; Y = y; Children = [] }, x + hGap)
    | children ->
        let rec layoutChildren accX acc list =
            match list with
            | [] -> List.rev acc, accX
            | hd::tl ->
                let (child, nextX) = layoutTree accX (y + vGap) hGap vGap hd
                layoutChildren nextX (child :: acc) tl

        let childrenLayout, newX = layoutChildren x [] node.Children
        let minX = childrenLayout |> List.minBy (fun n -> n.X) |> fun n -> n.X
        let maxX = childrenLayout |> List.maxBy (fun n -> n.X) |> fun n -> n.X
        let center = (minX + maxX) / 2.0

        ({ Node = node; X = center; Y = y; Children = childrenLayout }, newX)

let layoutForest (forest: TreeNode list) : PositionedNode list =
    let _, result =
        forest |> List.fold (fun (x, acc) node ->
            let (layouted, nextX) = layoutTree x 0.0 100.0 100.0 node
            (nextX, layouted :: acc)
        ) (0.0, [])
    result |> List.rev

// -------------------------------
// 5. SVG rendering
// -------------------------------
let rec renderSvgTreeNode (node: PositionedNode) : Node list =
    let lines =
        node.Children
        |> List.map (fun child ->
            svLn()
                .x1(string node.X)
                .y1(string node.Y)
                .x2(string child.X)
                .y2(string child.Y)
                .st("black")
                .Elt()
        )

    let children = node.Children |> List.map renderSvgTreeNode |> List.concat

    let circleAndText =
        [
            svCl()
                .cx(string node.X)
                .cy(string node.Y)
                .rd("20")
                .fl("lightblue")
                .st("black")
                .Elt()

            svTx()
                .x(string node.X)
                .y(string (node.Y + 5.0))
                .ta("middle")
                .sz("12")
                .nm(node.Node.Label)
                .Elt()
        ]

    lines @ children @ circleAndText

let getBoundingBox (nodes: PositionedNode list) =
    let rec collectAll (node: PositionedNode) =
        (node.X, node.Y) :: (node.Children |> List.collect collectAll)
    let allPoints = nodes |> List.collect collectAll
    let minX = allPoints |> List.map fst |> List.min
    let maxX = allPoints |> List.map fst |> List.max
    let minY = allPoints |> List.map snd |> List.min
    let maxY = allPoints |> List.map snd |> List.max
    (minX, minY, maxX, maxY)

// -------------------------------
// 6. Main entry point
// -------------------------------
let viewTreeSvgFromString (input: string) : Node =
    match parseTreeInput input with
    | Error errMsg ->
        svg {
            yield attr.width "100%"
            yield attr.height "100"
            yield
                svTx()
                    .x("20")
                    .y("40")
                    .ta("middle")
                    .sz("16")
                    .nm($"Error: {errMsg}")
                    .Elt()
        }

    | Ok parsed ->
        let forest = buildTree parsed
        let positioned = layoutForest forest

        let (minX, minY, maxX, maxY) = getBoundingBox positioned
        let padding = 50.0
        let vbX = minX - padding
        let vbY = minY - padding
        let vbW = maxX - minX + 2.0 * padding
        let vbH = maxY - minY + 2.0 * padding
        let viewBoxStr = sprintf "%f %f %f %f" vbX vbY vbW vbH

        svg {
            yield attr.width "100%"
            yield attr.height "100%"
            yield "viewBox" => viewBoxStr
            yield attr.style "max-width: 100%; height: auto; display: block; margin: auto;"

            for node in positioned do
                for el in renderSvgTreeNode node do
                    yield el
        }






