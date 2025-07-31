module NodeCode

open System
open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components

// Tree data structure
type TreeNode =
    { Id: Guid
      Name: string
      Weight: string
      X: float
      Y: float
      Children: TreeNode list }

type SubModel =
    { Root: TreeNode }

type SubMsg =
    | AddChild of Guid
    | UpdateName of Guid * string
    | UpdateWeight of Guid * string
    | DeleteNode of Guid

// Templates
type svLn = Template<"""
            <line
                x1="${x1}"
                y1="${y1}"
                x2="${x2}"
                y2="${y2}"
                stroke="#888"
                strokeWidth="2"
                />
            """>

type svCl = Template<
        """<circle
            cx="${cx}" 
            cy="${cy}" 
            r="20" 
            fill="#fff8dc"
            stroke "#333"
            strokeWidth "1.5"
            />
        """>

type svTx = Template<
        """<text 
        x="${x}" 
        y="${y}"
        width = "50px"
        font-size = "10px"
        font-family="Verdana"
        text-anchor="middle"
        dominant-baseline="middle"
        fill = "#808080"
        opacity = "1"
        >${nm}</text> """>

// Labels for nodes
let randomNames = [
    "<Hive>"; "<Cell>"; "<Comb>"; "<Hex>"; "<Core>"; "<Dock>"; "<Ring>";
    "<Link>"; "<Arc>"; "<Mod>"; "<Buzz>"; "<Wax>"; "<Sting>"; "<Veil>";
    "<Arch>"; "<Glow>"; "<Path>"; "<Air>"; "<Clad>"; "<Echo>"; "<Dawn>";
    "<Brood>"; "<Guard>"; "<Swarm>"; "<Nect>"; "<Pupa>"; "<Drone>"; "<Queen>";
    "<Field>"; "<Trail>"
]

let rng = System.Random()

let getRandomName () =
    let index = rng.Next(0, randomNames.Length)
    randomNames.[index]

// Layout the tree
let rec layoutTree (node: TreeNode) (depth: int) (xOffset: float ref) : TreeNode =
    let y = float depth * 60.0 + 30.0
    match node.Children with
    | [] ->
        let x = !xOffset
        xOffset := x + 80.0
        { node with X = x; Y = y }
    | children ->
        let laidOutChildren = children |> List.map (fun c -> layoutTree c (depth + 1) xOffset)
        let firstX = laidOutChildren.Head.X
        let lastX = (List.last laidOutChildren).X
        let x = (firstX + lastX) / 2.0
        { node with X = x; Y = y; Children = laidOutChildren }

// Recursive function to update a node
let rec mapTree (f: TreeNode -> TreeNode) (node: TreeNode) : TreeNode =
    let updated = f node
    { updated with Children = updated.Children |> List.map (mapTree f) }

let updateNodeById id updateFn node =
    mapTree (fun n -> if n.Id = id then updateFn n else n) node

// Remove a node by ID
let rec removeNodeById id (node: TreeNode) : TreeNode option =
    if node.Id = id then
        None
    else
        let newChildren =
            node.Children
            |> List.choose (removeNodeById id)
        Some { node with Children = newChildren }

// Update logic
let updateSub msg model =
    match msg with
    | AddChild id ->
        let addChild n =
            let newChild =
                { Id = Guid.NewGuid()
                  Name = getRandomName ()
                  Weight = "25"
                  X = 0.0
                  Y = 0.0
                  Children = [] }
            { n with Children = n.Children @ [newChild] }
        let newRoot = updateNodeById id addChild model.Root
        let laidOut = layoutTree newRoot 0 (ref 100.0)
        { model with Root = laidOut }
    | UpdateName (id, newName) ->
        let newRoot = updateNodeById id (fun n -> { n with Name = newName }) model.Root
        { model with Root = newRoot }
    | UpdateWeight (id, newWeight) ->
        let newRoot = updateNodeById id (fun n -> { n with Weight = newWeight }) model.Root
        { model with Root = newRoot }
    | DeleteNode id ->
        match removeNodeById id model.Root with
        | Some newRoot ->
            let laidOut = layoutTree newRoot 0 (ref 100.0)
            { model with Root = laidOut }
        | None -> model  // If root is deleted, keep model unchanged for now

// Output string generation
let generateOutput (root: TreeNode) =
    let rec traverse (prefix: string) (node: TreeNode) =
        seq {
            yield $"({prefix}/{node.Weight}/{node.Name})"
            for i, child in node.Children |> List.indexed do
                yield! traverse $"{prefix}.{i + 1}" child
        }
    traverse "1" root |> String.concat ", "

// Flatten tree to list
let rec flattenTree (node: TreeNode) : TreeNode list =
    node :: (node.Children |> List.collect flattenTree)

let computeCanvasBounds (nodes: TreeNode list) =
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    let maxY = nodes |> List.map (fun n -> n.Y) |> List.max
    let width = maxX + 20.0
    let height = maxY + 20.0
    width, height

// Tree editor view
let viewTreeEditor (model: SubModel) (dispatch: SubMsg -> unit) : Node =
    let renderNode (node: TreeNode) : Node =
        let containerStyle =
            $"position:absolute; left:{node.X - 35.0}px; top:{node.Y - 25.0}px; " +
            "width:60px; height:30px; border-radius:6px; background-color:white; " +
            "border:1px solid #d3d3d1; font-size:12px; cursor:default; z-index:1; " +
            "display:flex; flex-direction:column; align-items:center; justify-content:center; padding:2px;"

        div {
            attr.style containerStyle

            input {
                attr.``type`` "text"
                attr.value node.Name
                on.input (fun (e: ChangeEventArgs) -> dispatch (UpdateName (node.Id, string e.Value)))
                attr.style "width:50px; font-size:10px; text-align:center; border:none; outline:none; background:white;color:#6e6e6e;"
            }

            div {
                attr.style "display:flex; justify-content:space-between; width:50px; font-size:12px; align-items:center;"

                match node.Id = model.Root.Id with
                | false ->
                    span {
                        attr.style "color:red; font-weight:bold; opacity:0.5; cursor:pointer;"
                        on.dblclick (fun _ -> dispatch (DeleteNode node.Id))
                        text "-"
                    }
                | true ->
                    span {
                        attr.style "width:10px;" // layout spacer to keep alignment
                        text " "
                    }

                input {
                    attr.``type`` "text"
                    attr.value node.Weight
                    on.input (fun (e: ChangeEventArgs) -> dispatch (UpdateWeight (node.Id, string e.Value)))
                    attr.style "width:20px; font-size:12px; text-align:center; border:none; outline:none; background:white;color:#595959"
                }

                span {
                    attr.style "color:green; font-weight:bold; opacity:0.5; cursor:pointer;"
                    on.click (fun _ -> dispatch (AddChild node.Id))
                    text "+"
                }
            }

        }

    let renderConnection (parent: TreeNode) (child: TreeNode) : Node =
        svLn()
            .x1($"{parent.X}")
            .y1($"{parent.Y + 15.0}")
            .x2($"{child.X}")
            .y2($"{child.Y - 15.0}")
            .Elt()

    let rec collectConnections (node: TreeNode) : Node list =
        let direct = node.Children |> List.map (fun child -> renderConnection node child)
        let nested = node.Children |> List.collect collectConnections
        direct @ nested

    let nodes = flattenTree model.Root
    let lines = collectConnections model.Root

    let canvasWidth, canvasHeight = computeCanvasBounds nodes
    
    // Main container with graph editor
    div {
        // Instructions
        div {
            attr.style "text-align: center; font-size: 12px; color: #2a2a2a; opacity: 0.6; padding-top: 2px;"
            span {
                attr.style "font-weight: bold;"
                text "Edit in place "
            }
            span {
                text "node labels and values. "
            }
            span {
                attr.style "font-weight: bold;"
                text "Click  "
            }
            span {
                attr.style "color: green; font-size: 12px; font-weight: bold;"
                text " + "
            }
            span {
                text " to add a child node, "
            }
            span {
                attr.style "font-weight: bold;"
                text "Double Click  "
            }
            span {
                attr.style "color: red; font-size: 14px; font-weight: bold;"
                text " - "
            }
            span {
                text " to delete a node and all of its descendants"
            }
        }
        // Outer Scrollable Container
        div {
            attr.style "width:100%; overflow-x:auto; padding:0.25rem 0; display:flex; justify-content:center;"

            // Inner absolute layout container with size based on nodes
            div {
                attr.style $"position:relative; width:{canvasWidth}px; height:{canvasHeight}px;"

                // Connection Lines
                svg {
                    attr.style $"position:absolute; top:0; left:0; width:{canvasWidth}px; height:{canvasHeight}px; z-index:0;"
                    for line in lines do
                        line
                }

                // Nodes
                for node in nodes do
                    renderNode node
            }
        }

    }

let getOutput (model: SubModel) (Q: string) =
    ($"(0/Q={Q}),")+(generateOutput model.Root)

// Initial Tree structure
let initModel () : SubModel =
    let node name weight children =
        { Id = Guid.NewGuid()
          Name = name
          Weight = weight
          X = 0.0
          Y = 0.0
          Children = children }

    // Create the structure manually
    let initTree =
        node "Foyer" "16" [
            node "Living" "40" [
                node "Dining" "40" [
                    node "Kitchen" "20" [
                        node "Utility" "10" []
                    ]
                    node "Bed" "26" [
                        node "Bath" "12" []
                    ]
                ]
                node "Study" "25" [
                    node "Powder" "10" []
                ]
            ]
        ]

    let laidOut = layoutTree initTree 0 (ref 100.0)
    { Root = laidOut }

