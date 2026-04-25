module NodeCode

open System
open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components.Web

// --------------------
// Data Structures
// --------------------
type Point = { X: float; Y: float }

type TreeNode =
    { Id: Guid
      Name: string
      Weight: string
      X: float
      Y: float
      Children: TreeNode list }

type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

type SubModel = 
    { Root: TreeNode
      ConfirmingId: System.Guid option
      DraggingId: System.Guid option
      PendingDragId: System.Guid option
      DropTargetId: System.Guid option
      SvgInfo: SvgInfo option
      PointerDownPos: Point option
      LastMoveMs: float option }

type SubMsg =
    | ConfirmDelete of System.Guid
    | CancelDelete
    | DeleteNode of System.Guid
    | AddChild of System.Guid
    | UpdateName of System.Guid * string
    | UpdateWeight of System.Guid * string
    | PointerDown of MouseEventArgs
    | PointerMove of MouseEventArgs
    | PointerUp
    | DragStartInternal of System.Guid * SvgInfo * Point
    | PointerUpInternal

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

let rec extractNode (id: Guid) (node: TreeNode) : TreeNode option * TreeNode option =
    if node.Id = id then (None, Some node)
    else
        let mutable extracted = None
        let newChildren = 
            node.Children |> List.choose (fun c ->
                let (newNode, found) = extractNode id c
                if found.IsSome then extracted <- found
                newNode)
        (Some { node with Children = newChildren }, extracted)

let rec insertBefore (targetId: Guid) (nodeToInsert: TreeNode) (node: TreeNode) : TreeNode =
    let rec insertInList list =
        match list with
        | [] -> []
        | h :: t ->
            if h.Id = targetId then nodeToInsert :: h :: t
            else h :: insertInList t
    
    let hasTarget = node.Children |> List.exists (fun c -> c.Id = targetId)
    if hasTarget then
        { node with Children = insertInList node.Children }
    else
        { node with Children = node.Children |> List.map (insertBefore targetId nodeToInsert) }

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

// ---------- JS interop helpers ----------
let getSvgInfo (js: IJSRuntime) =
    async {
        let! el = js.InvokeAsync<System.Text.Json.JsonElement>("getSvgInfo", [| box "tree-canvas-svg" |]).AsTask() |> Async.AwaitTask
        return { 
            ViewBoxX = el.GetProperty("viewBoxX").GetDouble()
            ViewBoxY = el.GetProperty("viewBoxY").GetDouble()
            ViewBoxW = el.GetProperty("viewBoxW").GetDouble()
            ViewBoxH = el.GetProperty("viewBoxH").GetDouble()
            ClientLeft = el.GetProperty("left").GetDouble()
            ClientTop = el.GetProperty("top").GetDouble()
            ClientW = el.GetProperty("width").GetDouble()
            ClientH = el.GetProperty("height").GetDouble()
        }
    }

let toSvgCoords (info: SvgInfo) (clientX: float) (clientY: float) =
    { X = info.ViewBoxX + (clientX - info.ClientLeft) * info.ViewBoxW / info.ClientW
      Y = info.ViewBoxY + (clientY - info.ClientTop) * info.ViewBoxH / info.ClientH }

// --------------------
// Update
// --------------------
let updateSub (js: IJSRuntime) msg model =
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
    | PointerDown ev ->
        { model with PointerDownPos = None; PendingDragId = None }, Cmd.OfAsync.perform (fun _ -> getSvgInfo js) () (fun info -> 
            let pt = toSvgCoords info (float ev.ClientX) (float ev.ClientY)
            let rec findNode node =
                if pt.X >= node.X - 30.0 && pt.X <= node.X + 30.0 &&
                   pt.Y >= node.Y - 35.0 && pt.Y <= node.Y + 25.0 then Some node.Id
                else node.Children |> List.tryPick findNode
            let hitId = findNode model.Root
            match hitId with
            | Some id when id <> model.Root.Id -> DragStartInternal (id, info, pt)
            | _ -> PointerUpInternal
        )
    | PointerMove ev ->
        let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
        match model.LastMoveMs with
        | Some last when nowMs - last < 16.0 -> model, Cmd.none
        | _ ->
            match model.DraggingId, model.SvgInfo with
            | Some _, Some info ->
                let pt = toSvgCoords info (float ev.ClientX) (float ev.ClientY)
                let rec findNode node =
                    if pt.X >= node.X - 30.0 && pt.X <= node.X + 30.0 &&
                       pt.Y >= node.Y - 35.0 && pt.Y <= node.Y + 25.0 then Some node.Id
                    else node.Children |> List.tryPick findNode
                
                let targetId = findNode model.Root
                { model with DropTargetId = targetId; LastMoveMs = Some nowMs }, Cmd.none

            | None, Some info -> 
                match model.PendingDragId, model.PointerDownPos with
                | Some pendingId, Some startPt ->
                    let pt = toSvgCoords info (float ev.ClientX) (float ev.ClientY)
                    let dx = pt.X - startPt.X
                    let dy = pt.Y - startPt.Y
                    let dist = sqrt (dx*dx + dy*dy)
                    if dist > 5.0 then
                        { model with DraggingId = Some pendingId; PendingDragId = None; LastMoveMs = Some nowMs }, Cmd.none
                    else model, Cmd.none
                | _ -> model, Cmd.none
            | _ -> model, Cmd.none

    | PointerUp ->
        let model = { model with PendingDragId = None; PointerDownPos = None }
        match model.DraggingId, model.DropTargetId with
        | Some sourceId, Some targetId when sourceId <> targetId ->
            let sourceNode = findNodeById sourceId model.Root
            match sourceNode with
            | None -> { model with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
            | Some sn ->
                if isDescendant targetId sn then { model with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
                else
                    let (rootWithoutSource, extracted) = extractNode sourceId model.Root
                    match rootWithoutSource, extracted with
                    | Some rs, Some ex ->
                        let newRoot = insertBefore targetId ex rs
                        { model with Root = layoutTree newRoot 0 (ref 50.0); DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
                    | _ -> { model with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
        | _ -> { model with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
    | DragStartInternal (id, info, pt) -> 
        { model with PendingDragId = Some id; SvgInfo = Some info; PointerDownPos = Some pt; DropTargetId = None }, Cmd.none
    | PointerUpInternal ->
        { model with DraggingId = None; PendingDragId = None; DropTargetId = None; SvgInfo = None; PointerDownPos = None }, Cmd.none

// --------------------
// View
// --------------------
let renderNode (node: TreeNode) (prefix: string) (model: SubModel) (isAffected: bool) (dispatch: SubMsg -> unit) =
    let isRoot = node.Id = model.Root.Id
    let isConfirmingThis = model.ConfirmingId = Some node.Id
    let isDropTarget = model.DropTargetId = Some node.Id
    
    let outerClasses = 
        String.concat " " [
            "node-outer"
            if isAffected && not isRoot then "is-affected"
            if isConfirmingThis && not isRoot then "is-confirming"
            if model.DraggingId = Some node.Id && not isRoot then "is-dragging"
            if isDropTarget && not isRoot then "is-drop-target"
        ]

    div {
        attr.``class`` outerClasses
        attr.style $"position:absolute; left:{node.X - 30.0}px; top:{node.Y - 35.0}px; width:60px; height:60px; pointer-events:none;"
        
        div {
            attr.``class`` "node-inner"
            attr.style "pointer-events:auto;" // Allow interaction with buttons/inputs
            match isConfirmingThis with
            | true -> 
                button {
                    attr.``class`` "node-confirm-del"
                    "onpointerdown:stopPropagation" => true
                    on.click (fun _ -> dispatch (DeleteNode node.Id))
                    text "DELETE"
                }
                button {
                    attr.``class`` "node-confirm-cancel"
                    "onpointerdown:stopPropagation" => true
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
                            "onpointerdown:stopPropagation" => true
                            on.click (fun _ -> dispatch (ConfirmDelete node.Id))
                            text "×" 
                        }
                    | false -> ()

                    input {
                        attr.``class`` "nodename"
                        attr.value node.Name
                        "onpointerdown:stopPropagation" => true
                        on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
                    }
                    input {
                        attr.``class`` "nodeweight"
                        attr.value node.Weight
                        "onpointerdown:stopPropagation" => true
                        on.input (fun e -> dispatch (UpdateWeight (node.Id, string e.Value)))
                    }
                    
                    button { 
                        attr.``class`` "nodebutton2"
                        "onpointerdown:stopPropagation" => true
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

    let rec renderAll (node: TreeNode) (prefix: string) : Node =
        concat {
            renderNode node prefix model (isAffected node.Id) dispatch
            for i, child in node.Children |> List.indexed do
                renderAll child $"{prefix}.{i + 1}"
        }

    let containerClasses = 
        String.concat " " [
            "tree-container"
            if model.DraggingId.IsSome then "is-dragging-any"
        ]

    div {
        attr.``class`` containerClasses
        on.pointermove (fun ev -> dispatch (PointerMove ev))
        on.pointerup (fun _ -> dispatch PointerUp)

        div {
            attr.id "tree-canvas-svg"
            attr.``class`` "tree-canvas"
            attr.style $"width:{canvasWidth}px; height:{max 150.0 canvasHeight}px; touch-action:none;"
            on.pointerdown (fun ev -> dispatch (PointerDown ev))
            
            svg {
                attr.``class`` "tree-svg"
                attr.style $"width:{canvasWidth}px; height:{canvasHeight}px;"
                for line in lines do line
            }
            renderAll model.Root "1"
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
    | false -> { Root = layoutTree treeResult.Head 0 (ref 50.0); ConfirmingId = None; DraggingId = None; PendingDragId = None; DropTargetId = None; SvgInfo = None; PointerDownPos = None; LastMoveMs = None }