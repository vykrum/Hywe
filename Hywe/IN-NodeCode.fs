module NodeCode

open System
open Elmish
open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components

// --------------------
// Data Structures
// --------------------
type TreeNode =
    { Id: Guid
      Name: string
      Weight: string
      X: float
      Y: float
      Children: TreeNode list }

type SubModel = 
    { Root: TreeNode
      ConfirmingId: System.Guid option }

type SubMsg =
    | ConfirmDelete of System.Guid
    | CancelDelete
    | DeleteNode of System.Guid
    | AddChild of System.Guid
    | UpdateName of System.Guid * string
    | UpdateWeight of System.Guid * string

type svLn = Template<"""<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="#888" stroke-width="1"/>""">

// --------------------
// Helpers
// --------------------
let initWeight = 96
let randomNames = [
    "<Hive>"; "<Cell>"; "<Comb>"; "<Hex>"; "<Core>"; "<Dock>"; "<Ring>";
    "<Link>"; "<Arc>"; "<Mod>"; "<Buzz>"; "<Wax>"; "<Sting>"; "<Veil>";
    "<Arch>"; "<Glow>"; "<Path>"; "<Air>"; "<Clad>"; "<Echo>"; "<Dawn>";
    "<Brood>"; "<Guard>"; "<Swarm>"; "<Nect>"; "<Pupa>"; "<Drone>"; "<Queen>";
    "<Field>"; "<Trail>"
]
let rng = System.Random()
let getRandomName () = randomNames.[rng.Next(randomNames.Length)]

let injectSqn (input: string) (newSqn: string) =
    let pattern = "Q=[A-Z]+"
    let replacement = "Q=" + newSqn
    let regex = System.Text.RegularExpressions.Regex(pattern)
    match regex.IsMatch(input) with
    | true  -> regex.Replace(input, replacement)
    | false -> input + $"(0/Q={newSqn})"

// --------------------
// Tree Manipulation
// --------------------
let rec addChildToNodeById (node: TreeNode) parentId =
    match node.Id = parentId with
    | true ->
        let newChild = { Id = Guid.NewGuid(); Name = getRandomName(); Weight = "96"; X = 0.0; Y = 0.0; Children = [] }
        { node with Children = node.Children @ [newChild] }
    | false ->
        { node with Children = node.Children |> List.map (fun c -> addChildToNodeById c parentId) }

let rec removeNodeById id (node: TreeNode) : TreeNode option =
    match node.Id = id with
    | true -> None
    | false ->
        let newChildren = node.Children |> List.choose (removeNodeById id)
        Some { node with Children = newChildren }

let rec updateNodeById id updateFn node =
    match node.Id = id with
    | true -> updateFn node
    | false -> { node with Children = node.Children |> List.map (updateNodeById id updateFn) }

// --------------------
// Layout
// --------------------
let rec layoutTree (node: TreeNode) (depth: int) (xOffset: float ref) : TreeNode =
    let y = float depth * 65.0 + 30.0
    match node.Children with
    | [] ->
        let x = xOffset.Value
        xOffset.Value <- x + 60.0
        { node with X = x; Y = y }
    | children ->
        let laidOutChildren = children |> List.map (fun c -> layoutTree c (depth + 1) xOffset)
        let firstX = laidOutChildren.Head.X
        let lastX = (List.last laidOutChildren).X
        let x = (firstX + lastX) / 2.0
        { node with X = x; Y = y; Children = laidOutChildren }

// --------------------
// Update
// --------------------
let updateSub msg model =
    match msg with
    | ConfirmDelete id -> { model with ConfirmingId = Some id }, Cmd.none
    | CancelDelete -> { model with ConfirmingId = None }, Cmd.none
    
    | DeleteNode id -> 
        match removeNodeById id model.Root with
        | Some newRoot -> { model with Root = layoutTree newRoot 0 (ref 50.0); ConfirmingId = None }, Cmd.none
        | None -> model, Cmd.none

    | AddChild parentId ->
        let newRoot = addChildToNodeById model.Root parentId 
        { model with Root = layoutTree newRoot 0 (ref 50.0) }, Cmd.none

    | UpdateName (id, name) ->
        // Correct order: id, then function, then root
        let newRoot = updateNodeById id (fun n -> { n with Name = name }) model.Root
        { model with Root = newRoot }, Cmd.none

    | UpdateWeight (id, weight) ->
        // Correct order: id, then function, then root
        let newRoot = updateNodeById id (fun n -> { n with Weight = weight }) model.Root
        { model with Root = newRoot }, Cmd.none

// --------------------
// View: Hexagon nodes
// --------------------
let renderNode (node: TreeNode) (model: SubModel) (dispatch: SubMsg -> unit) =
    let outerStyle = 
        $"position:absolute; left:{node.X - 30.0}px; top:{node.Y - 35.0}px; width:60px; height:60px; background-color:#d3d3d1; " +
        "clip-path: polygon(50% 0%, 95% 25%, 95% 75%, 50% 100%, 5% 75%, 5% 25%); display:flex; align-items:center; justify-content:center;"
    
    let innerStyle = "position:relative; width:54px; height:54px; background-color:white; clip-path: inherit; display:flex; flex-direction:column; align-items:center; justify-content:center; font-size:12px; cursor:default; z-index:1; gap:2px; padding:2px;"

    div {
        attr.style outerStyle
        div {
            attr.style innerStyle
            match model.ConfirmingId = Some node.Id with
            | true -> 
                // --- CONFIRMATION VIEW ---
                button {
                    attr.``class`` "node-confirm-del"
                    on.click (fun _ -> dispatch (DeleteNode node.Id))
                    text "DELETE"
                }
                div { attr.style "width:60%; height:1px; background:#eee" }
                button {
                    attr.``class`` "node-confirm-cancel"
                    on.click (fun _ -> dispatch CancelDelete)
                    text "CANCEL"
                }
            | false ->
                // --- NORMAL EDIT VIEW ---
                concat {
                    match node.Id <> model.Root.Id with
                    | true -> 
                        button { 
                            attr.``class`` "nodebutton1"
                            on.click (fun _ -> dispatch (ConfirmDelete node.Id))
                            text "×" 
                        }
                    | false -> ()

                    input {
                        attr.``class`` "nodename"
                        attr.value node.Name
                        attr.placeholder "Name"
                        on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
                    }
                    input {
                        attr.``type`` "text"
                        attr.``class`` "nodeweight"
                        attr.value node.Weight
                        attr.placeholder "Size"
                        on.input (fun e -> dispatch (UpdateWeight (node.Id, string e.Value)))
                    }
                    
                    button { 
                        attr.``class`` "nodebutton2"
                        on.click (fun _ -> dispatch (AddChild node.Id))
                        text "+" 
                    }
                }
        }
    }

let viewTreeEditor (model: SubModel) (dispatch: SubMsg -> unit) : Node =      
    let renderConnection (parent: TreeNode) (child: TreeNode) : Node =
        svLn().x1($"{parent.X}").y1($"{parent.Y + 5.0}").x2($"{child.X}").y2($"{child.Y - 35.0}").Elt()

    let rec flattenTree node = node :: (node.Children |> List.collect flattenTree)
    
    let rec collectConnections (node: TreeNode) : Node list =
        node.Children |> List.collect (fun child -> renderConnection node child :: collectConnections child)

    let nodes = flattenTree model.Root
    let lines = collectConnections model.Root
    
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    let maxY = nodes |> List.map (fun n -> n.Y + 35.0) |> List.max
    let canvasWidth = maxX + 60.0
    let canvasHeight = maxY + 30.0

    div {
        attr.style "width:100%; overflow-x:auto; padding-top:16px; display:flex; justify-content:center;"
        div {
            attr.style $"position:relative; width:{canvasWidth}px; height:{max 150.0 canvasHeight}px;"
            svg {
                attr.style $"position:absolute; top:0; left:0; width:{canvasWidth}px; height:{canvasHeight}px; z-index:0;"
                for line in lines do line
            }
            for node in nodes do
                renderNode node model dispatch
        }
    }

// --------------------
// Serialization & Initialization
// --------------------
let generateOutput (root: TreeNode) =
    let rec traverse prefix (node: TreeNode) =
        seq {
            yield $"({prefix}/{node.Weight}/{node.Name})"
            for i, child in node.Children |> List.indexed do
                yield! traverse $"{prefix}.{i + 1}" child
        }
    traverse "1" root |> String.concat ", "

let buildTree (input: string) : TreeNode list =
    let lines = input.Trim([| ' '; ','; ')' |]).Split("),", StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim([| '('; ')' |])) |> Array.toList
    let rec build (items: (int list * string * string) list) (prefix: int list) : TreeNode list =
        items 
        |> List.choose (fun (path, weight, name) ->
            let isChild = (prefix = [] && path.Length = 1) || (path.Length = prefix.Length + 1 && List.take prefix.Length path = prefix)
            match isChild with | true -> Some(path, weight, name) | false -> None)
        |> List.map (fun (path, weight, name) -> { Id = Guid.NewGuid(); Name = name; Weight = weight; X = 0.0; Y = 0.0; Children = build items path })

    lines |> List.choose (fun line ->
        let parts = line.Split('/')
        match parts.[0] = "0" || parts.[0].StartsWith "0." with
        | true -> None
        | false -> Some(parts.[0].Split('.') |> Array.map int |> Array.toList, parts.[1], parts.[2])
    ) |> fun items -> build items []

let getOutput (model: SubModel) q w h x e o i =
    $"(0/Q={q}/W={w}/H={h}/X={x}/E={e}/O={o}/I={i})," + generateOutput model.Root

let initModel (inputString: string) : SubModel =
    match buildTree inputString with
    | [] -> failwith "No valid nodes found."
    | rootNode::_ -> { Root = layoutTree rootNode 0 (ref 50.0); ConfirmingId = None }