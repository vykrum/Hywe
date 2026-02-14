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
      PrimedNode: Guid option  }

type SubMsg =
    | AddChild of Guid
    | UpdateName of Guid * string
    | UpdateWeight of Guid * string
    | DeleteNode of Guid
    | PrimeDeletion of Guid
    | ClearPrime

type svLn = Template<"""
    <line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}"
          stroke="#888" stroke-width="1"/>
""">

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
        let laidOutChildren =
            children |> List.map (fun c -> layoutTree c (depth + 1) xOffset)

        let firstX = laidOutChildren.Head.X
        let lastX = (List.last laidOutChildren).X
        let x = (firstX + lastX) / 2.0

        { node with X = x; Y = y; Children = laidOutChildren }

// --------------------
// Tree Manipulation
// --------------------

let rec mapTree (f: TreeNode -> TreeNode) (node: TreeNode) : TreeNode =
    let updated = f node
    { updated with Children = updated.Children |> List.map (mapTree f) }

let updateNodeById id updateFn node =
    mapTree (fun n -> if n.Id = id then updateFn n else n) node

let rec removeNodeById id (node: TreeNode) : TreeNode option =
    if node.Id = id then None
    else
        let newChildren = node.Children |> List.choose (removeNodeById id)
        Some { node with Children = newChildren }

let updateSub msg model : SubModel * Cmd<SubMsg> =
    match msg with
    | PrimeDeletion id -> 
        { model with PrimedNode = Some id }, 
        Cmd.OfAsync.perform (fun () -> async {
            do! Async.Sleep 2000
            return ()
        }) () (fun _ -> ClearPrime)

    | ClearPrime -> 
        { model with PrimedNode = None }, Cmd.none

    | AddChild id ->
        let addChild n =
            let newChild = { Id = Guid.NewGuid(); Name = getRandomName (); Weight = $"{initWeight}"; X = 0.0; Y = 0.0; Children = [] }
            { n with Children = n.Children @ [newChild] }
        let newRoot = updateNodeById id addChild model.Root
        { model with Root = layoutTree newRoot 0 (ref 0.0) }, Cmd.none

    | UpdateName (id, newName) ->
        let newRoot = updateNodeById id (fun n -> { n with Name = newName }) model.Root
        { model with Root = newRoot }, Cmd.none

    | UpdateWeight (id, newWeight) ->
        let newRoot = updateNodeById id (fun n -> { n with Weight = newWeight }) model.Root
        { model with Root = newRoot }, Cmd.none 

    | DeleteNode id ->
        match removeNodeById id model.Root with
        | Some newRoot -> { model with Root = layoutTree newRoot 0 (ref 0.0); PrimedNode = None }, Cmd.none
        | None -> model, Cmd.none

// --------------------
// Output / Flatten Helpers
// --------------------

let generateOutput (root: TreeNode) =
    let rec traverse prefix (node: TreeNode) =
        seq {
            yield $"({prefix}/{node.Weight}/{node.Name})"
            for i, child in node.Children |> List.indexed do
                yield! traverse $"{prefix}.{i + 1}" child
        }
    traverse "1" root |> String.concat ", "

let rec flattenTree node =
    node :: (node.Children |> List.collect flattenTree)

let computeCanvasBounds (nodes: TreeNode list) =
    let minY = nodes |> List.map (fun n -> n.Y - 35.0) |> List.min
    let maxY = nodes |> List.map (fun n -> n.Y + 35.0) |> List.max
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    maxX, maxY - minY

// ----------------------------------------
// View: Hexagon nodes
// ----------------------------------------

let viewTreeEditor (model: SubModel) (dispatch: SubMsg -> unit) : Node =      
    let renderNode (node: TreeNode) : Node =
        let isPrimed = model.PrimedNode = Some node.Id
        
        let outerStyle =
            $"position:absolute; left:{node.X - 30.0}px; top:{node.Y - 35.0}px; " +
            "width:60px; height:60px; background-color:#d3d3d1; " +
            "clip-path: polygon(50% 0%, 95% 25%, 95% 75%, 50% 100%, 5% 75%, 5% 25%); " +
            "display:flex; align-items:center; justify-content:center;"

        let innerStyle =
            "position:relative; width:54px; height:54px; background-color:white; clip-path: inherit; " +
            "display:flex; flex-direction:column; align-items:center; justify-content:center; " +
            "font-size:12px; cursor:default; z-index:1; gap:3px; padding:2px;"

        div {
            attr.style outerStyle
            div {
                attr.style innerStyle

                // Minimalist hint (Styling moved to CSS class)
                match isPrimed with
                | true ->
                    span {
                        attr.``class`` "delete-hint"
                        text "double-tap to delete"
                    }
                | false -> text ""

                match node.Id = model.Root.Id with
                | false ->
                    button {
                        attr.``class`` "nodebutton1"
                        // Removed title to let CSS hover handle desktop tooltip
                        attr.style "touch-action: manipulation;"
                        on.click (fun _ -> dispatch (PrimeDeletion node.Id))
                        on.dblclick (fun _ -> dispatch (DeleteNode node.Id))
                        text "×"
                    }
                | true ->
                    button {
                        attr.``class`` "nodebutton0"
                        text "_"
                    }

                input {
                    attr.``type`` "text"
                    attr.``class`` "nodename"
                    attr.title "Edit in place"
                    "maxlength" => "10"
                    attr.value node.Name
                    on.input (fun (e: ChangeEventArgs) ->
                        dispatch (UpdateName (node.Id, string e.Value))
                    )
                }

                input {
                    attr.``type`` "number"
                    attr.``class`` "nodeweight"
                    attr.title "Edit in place"
                    attr.value node.Weight
                    on.input (fun (e: ChangeEventArgs) ->
                        let v = string e.Value
                        let rounded =
                            match System.Int32.TryParse(v) with
                            | true, i -> string i
                            | _ ->
                                match System.Double.TryParse(v) with
                                | true, d -> string (int (System.Math.Ceiling(d)))
                                | _ -> "3"
                        dispatch (UpdateWeight(node.Id, rounded))
                    )
                }

                button {
                    attr.``class`` "nodebutton2"
                    attr.title "Clk: Add Node"
                    on.click (fun _ -> dispatch (AddChild node.Id))
                    text "+"
                }
            }
        }
    let renderConnection (parent: TreeNode) (child: TreeNode) : Node =
        svLn()
            .x1($"{parent.X}")
            .y1($"{parent.Y + 5.0}")
            .x2($"{child.X}")
            .y2($"{child.Y - 35.0}")
            .Elt()

    let rec collectConnections (node: TreeNode) : Node list =
        node.Children
        |> List.collect (fun child -> renderConnection node child :: collectConnections child)

    let nodes = flattenTree model.Root
    let lines = collectConnections model.Root
    let canvasWidth, canvasHeight = computeCanvasBounds nodes

    div {
        attr.style "width:100%; overflow-x:auto; padding-top:16px; display:flex; justify-content:center;"
        div {
            attr.style $"position:relative; width:{canvasWidth}px; height:{max 150.0 canvasHeight}px;"
            svg {
                attr.style $"position:absolute; top:0; left:0; width:{canvasWidth}px; height:{canvasHeight}px; z-index:0;"
                for line in lines do line
            }

            for node in nodes do
                renderNode node
        }
    }

// -------------------- 
// String to Node Tree
// --------------------
let buildTree (input: string) : TreeNode list =
    let lines =
        input.Trim([| ' '; ','; ')' |])
             .Split("),", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim([| '('; ')' |]))
        |> Array.toList

    let rec build (items: (int list * string * string) list) (prefix: int list) : TreeNode list =
        items
        |> List.choose (fun (path, weight, name) ->
            let isChild =
                (prefix = [] && path.Length = 1) ||
                (path.Length = prefix.Length + 1 && List.take prefix.Length path = prefix)
            if isChild then Some(path, weight, name) else None
        )
        |> List.map (fun (path, weight, name) ->
            { Id = Guid.NewGuid()
              Name = name
              Weight = weight
              X = 0.0
              Y = 0.0
              Children = build items path }
        )

    lines
    |> List.choose (fun line ->
        let parts = line.Split('/')
        let path = parts.[0]
        if path = "0" || path.StartsWith "0." then None
        else
            let pathList = path.Split('.') |> Array.map int |> Array.toList
            Some(pathList, parts.[1], parts.[2])
    )
    |> fun items -> build items []

// --------------------
// Initialization
// --------------------
let getOutput (model: SubModel) (q: string) (w: int) (h: int) (x: string) (e: string) (o: string) (i: string)=
    $"(0/Q={q}/W={w}/H={h}/X={x}/E={e}/O={o}/I={i})," + generateOutput model.Root

let initModel (inputString: string) : SubModel =
    match buildTree inputString with
    | [] -> failwith "No valid nodes found in input string."
    | rootNode::_ -> 
        { Root = layoutTree rootNode 0 (ref 0.0)
          PrimedNode = None}