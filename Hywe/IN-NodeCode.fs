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

type svLn = Template<"""<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${color}" stroke-width="${width}"/>""">

// --------------------
// Helpers
// --------------------
let initWeight = 96
let randomNames = ["<Hive>"; "<Cell>"; "<Comb>"; "<Hex>"; "<Core>"; "<Dock>"; "<Ring>"; "<Link>"; "<Arc>"; "<Mod>"; "<Buzz>"; "<Wax>"; "<Sting>"; "<Veil>"; "<Arch>"; "<Glow>"; "<Path>"; "<Air>"; "<Clad>"; "<Echo>"; "<Dawn>"; "<Brood>"; "<Guard>"; "<Swarm>"; "<Nect>"; "<Pupa>"; "<Drone>"; "<Queen>"; "<Field>"; "<Trail>"]
let rng = System.Random()
let getRandomName () = randomNames.[rng.Next(randomNames.Length)]

let injectSqn (input: string) (newSqn: string) =
    let pattern = "Q=[A-Z]+"
    let replacement = "Q=" + newSqn
    let regex = System.Text.RegularExpressions.Regex(pattern)
    let isMatch = regex.IsMatch(input)
    match isMatch with
    | true  -> regex.Replace(input, replacement)
    | false -> input + $"(0/Q={newSqn})"

let rec isDescendant (targetId: Guid) (potentialParent: TreeNode) : bool =
    potentialParent.Children |> List.exists (fun c -> c.Id = targetId || isDescendant targetId c)

let rec findNodeById (id: Guid) (node: TreeNode) : TreeNode option =
    let found = node.Id = id
    match found with
    | true -> Some node
    | false -> node.Children |> List.tryPick (findNodeById id)

// --------------------
// Tree Manipulation
// --------------------
let rec addChildToNodeById (node: TreeNode) parentId =
    let found = node.Id = parentId
    match found with
    | true ->
        let newChild = { Id = Guid.NewGuid(); Name = getRandomName(); Weight = "96"; X = 0.0; Y = 0.0; Children = [] }
        { node with Children = node.Children @ [newChild] }
    | false -> { node with Children = node.Children |> List.map (fun c -> addChildToNodeById c parentId) }

let rec removeNodeById id (node: TreeNode) : TreeNode option =
    let found = node.Id = id
    match found with
    | true -> None
    | false ->
        let newChildren = node.Children |> List.choose (removeNodeById id)
        Some { node with Children = newChildren }

let rec updateNodeById id updateFn node =
    let found = node.Id = id
    match found with
    | true -> updateFn node
    | false -> { node with Children = node.Children |> List.map (updateNodeById id updateFn) }

// --------------------
// Layout
// --------------------
let rec layoutTree (node: TreeNode) (depth: int) (xOffset: float ref) : TreeNode =
    let y = float depth * 65.0 + 30.0
    let hasNoChildren = List.isEmpty node.Children
    match hasNoChildren with
    | true ->
        let x = xOffset.Value
        xOffset.Value <- x + 60.0
        { node with X = x; Y = y }
    | false ->
        let laidOutChildren = node.Children |> List.map (fun c -> layoutTree c (depth + 1) xOffset)
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
        let rootResult = removeNodeById id model.Root
        match rootResult with
        | Some newRoot -> { model with Root = layoutTree newRoot 0 (ref 50.0); ConfirmingId = None }, Cmd.none
        | None -> model, Cmd.none
    | AddChild parentId ->
        let newRoot = addChildToNodeById model.Root parentId 
        { model with Root = layoutTree newRoot 0 (ref 50.0) }, Cmd.none
    | UpdateName (id, name) ->
        let newRoot = updateNodeById id (fun n -> { n with Name = name }) model.Root
        { model with Root = newRoot }, Cmd.none
    | UpdateWeight (id, weight) ->
        let newRoot = updateNodeById id (fun n -> { n with Weight = weight }) model.Root
        { model with Root = newRoot }, Cmd.none

// --------------------
// View
// --------------------
let renderNode (node: TreeNode) (model: SubModel) (isAffected: bool) (dispatch: SubMsg -> unit) =
    let isConfirmingThis = model.ConfirmingId = Some node.Id
    
    // Combine CSS classes based on state
    let outerClasses = 
        String.concat " " [
            "node-outer"
            if isAffected then "is-affected"
            if isConfirmingThis then "is-confirming"
        ]

    div {
        attr.``class`` outerClasses
        attr.style $"left:{node.X - 30.0}px; top:{node.Y - 35.0}px;" // Position still dynamic
        div {
            attr.``class`` "node-inner"
            match isConfirmingThis with
            | true -> 
                button {
                    attr.``class`` "node-confirm-del"
                    on.click (fun _ -> dispatch (DeleteNode node.Id))
                    text "DELETE"
                }
                button {
                    attr.``class`` "node-confirm-cancel"
                    on.click (fun _ -> dispatch CancelDelete)
                    text "CANCEL"
                }
            | false ->
                concat {
                    let isNotRoot = node.Id <> model.Root.Id
                    match isNotRoot with
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
                        on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
                    }
                    input {
                        attr.``class`` "nodeweight"
                        attr.value node.Weight
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
    let confirmingSubtree = model.ConfirmingId |> Option.bind (fun id -> findNodeById id model.Root)
    let isAffected nodeId =
        match confirmingSubtree with
        | Some root -> root.Id = nodeId || isDescendant nodeId root
        | None -> false

    let renderConnection (parent: TreeNode) (child: TreeNode) : Node =
        let affected = isAffected child.Id
        let color = if affected then "#E67E22" else "#888"
        let width = if affected then "1.5" else "1"
        svLn().x1($"{parent.X}").y1($"{parent.Y + 5.0}").x2($"{child.X}").y2($"{child.Y - 35.0}")
             .color(color).width(width).Elt()

    let rec flattenTree node = node :: (node.Children |> List.collect flattenTree)
    let nodes = flattenTree model.Root
    let lines = model.Root.Children |> List.collect (fun c -> 
        let rec collect n = n.Children |> List.collect (fun child -> renderConnection n child :: collect child)
        renderConnection model.Root c :: collect c)
    
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    let maxY = nodes |> List.map (fun n -> n.Y + 35.0) |> List.max
    let canvasWidth = maxX + 60.0
    let canvasHeight = maxY + 30.0

    div {
        attr.``class`` "tree-container"
        div {
            attr.``class`` "tree-canvas"
            attr.style $"width:{canvasWidth}px; height:{max 150.0 canvasHeight}px;"
            svg {
                attr.``class`` "tree-svg"
                attr.style $"width:{canvasWidth}px; height:{canvasHeight}px;"
                for line in lines do line
            }
            for node in nodes do
                renderNode node model (isAffected node.Id) dispatch
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
        let isSpecialNode = parts.[0] = "0" || parts.[0].StartsWith "0."
        match isSpecialNode with
        | true -> None
        | false -> Some(parts.[0].Split('.') |> Array.map int |> Array.toList, parts.[1], parts.[2])
    ) |> fun items -> build items []

let getOutput (model: SubModel) q w h x e o i =
    $"(0/Q={q}/W={w}/H={h}/X={x}/E={e}/O={o}/I={i})," + generateOutput model.Root

let initModel (inputString: string) : SubModel =
    let treeResult = buildTree inputString
    let hasNodes = List.isEmpty treeResult
    match hasNodes with
    | true -> failwith "No valid nodes found."
    | false -> { Root = layoutTree treeResult.Head 0 (ref 50.0); ConfirmingId = None }