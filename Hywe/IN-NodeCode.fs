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
      Children: TreeNode list
      Level: int }

type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

type ConfirmAction = 
    | Delete 
    | Elevate 
    | NoAction

type SubModel = 
    { Levels: Map<int, TreeNode>
      ActiveLevel: int
      LevelAnchors: Map<int, System.Guid>
      ConfirmingId: System.Guid option
      ConfirmingAction: ConfirmAction
      ActiveMenuId: System.Guid option
      DraggingId: System.Guid option
      PendingDragId: System.Guid option
      DropTargetId: System.Guid option
      SvgInfo: SvgInfo option
      PointerDownPos: Point option
      LastMoveMs: float option }

type SubMsg =
    | OpenMenu of System.Guid
    | CloseMenu
    | SetLevel of int
    | PrepareAction of System.Guid * ConfirmAction
    | CancelAction
    | ExecuteAction of System.Guid * ConfirmAction
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
    let levels = input.Split(';', StringSplitOptions.RemoveEmptyEntries)
    let pattern = "Q=[A-Z]+"
    let replacement = "Q=" + newSqn
    let regex = System.Text.RegularExpressions.Regex(pattern)
    
    levels 
    |> Array.map (fun lvl ->
        let trimmed = lvl.Trim()
        let isMatch = regex.IsMatch(trimmed)
        match isMatch with
        | true  -> regex.Replace(trimmed, replacement)
        | false -> 
            // Inject Q into the first attribute group (0/...)
            if trimmed.Contains("(0/") then
                trimmed.Replace("(0/", $"(0/Q={newSqn}/")
            else
                trimmed + $"(0/Q={newSqn})"
    )
    |> String.concat " ; "

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
        let newChild = { Id = Guid.NewGuid(); Name = getRandomName(); Weight = "96"; X = 0.0; Y = 0.0; Children = []; Level = node.Level }
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

let rec resetElevatedNodes targetLvl (node: TreeNode) =
    let newNode = if node.Level > targetLvl then { node with Level = targetLvl } else node
    { newNode with Children = newNode.Children |> List.map (resetElevatedNodes targetLvl) }

let rec syncHierarchy (levels: Map<int, TreeNode>) (anchors: Map<int, Guid>) (lvl: int) =
    match anchors |> Map.tryFind (lvl + 1) with
    | Some anchorId ->
        match levels |> Map.tryFind lvl with
        | Some parentTree ->
            match findNodeById anchorId parentTree with
            | Some anchorNode ->
                match levels |> Map.tryFind (lvl + 1) with
                | Some childTree ->
                    let updatedChildTree = { childTree with Name = anchorNode.Name; Weight = anchorNode.Weight }
                    let nextLevels = levels |> Map.add (lvl + 1) updatedChildTree
                    syncHierarchy nextLevels anchors (lvl + 1)
                | None -> levels
            | None -> levels
        | None -> levels
    | None -> levels

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
    | OpenMenu id -> { model with ActiveMenuId = Some id; ConfirmingId = None }, Cmd.none
    | CloseMenu -> { model with ActiveMenuId = None }, Cmd.none
    | SetLevel lvl -> { model with ActiveLevel = lvl; ActiveMenuId = None }, Cmd.none
    | PrepareAction (id, action) -> 
        { model with 
            ConfirmingId = Some id
            ConfirmingAction = action
            ActiveMenuId = None }, Cmd.none
    | CancelAction -> { model with ConfirmingId = None; ConfirmingAction = NoAction }, Cmd.none
    | ExecuteAction (id, action) ->
        match action with
        | Delete ->
            let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
            if id = currentTree.Id then
                // Rule: Base level root can never be deleted
                if model.ActiveLevel = 0 then model, Cmd.none
                else
                    // Rule: Root cannot be deleted if any of its descendants have an elevate node
                    let hasElevatedDescendants = model.Levels.Keys |> Seq.exists (fun k -> k > model.ActiveLevel)
                    if hasElevatedDescendants then model, Cmd.none
                    else
                        // De-elevate: Switch back to parent level and reset the anchor
                        let parentLvl = model.ActiveLevel - 1
                        let anchorId = model.LevelAnchors |> Map.tryFind model.ActiveLevel
                        match anchorId, model.Levels |> Map.tryFind parentLvl with
                        | Some aId, Some pTree ->
                            let updatedParentTree = updateNodeById aId (fun node -> { node with Level = parentLvl }) pTree
                            let laidOutParent = layoutTree updatedParentTree 0 (ref 50.0)
                            let newLevels = 
                                model.Levels 
                                |> Map.remove model.ActiveLevel 
                                |> Map.add parentLvl laidOutParent
                            let newAnchors = model.LevelAnchors |> Map.remove model.ActiveLevel
                            { model with 
                                Levels = newLevels
                                LevelAnchors = newAnchors
                                ActiveLevel = parentLvl
                                ConfirmingId = None
                                ConfirmingAction = NoAction
                                ActiveMenuId = None }, Cmd.none
                        | _ -> model, Cmd.none
            else
                // Normal node deletion or clearing descendants for elevated nodes
                let nodeToDelete = findNodeById id currentTree
                match nodeToDelete with
                | Some n when n.Level > model.ActiveLevel ->
                    // Elevated node: clear descendants instead of deleting
                    let newRoot = updateNodeById id (fun node -> { node with Children = [] }) currentTree
                    let laidOut = layoutTree newRoot 0 (ref 50.0)
                    let newLevels = model.Levels |> Map.add model.ActiveLevel laidOut
                    { model with Levels = newLevels; ConfirmingId = None; ConfirmingAction = NoAction; ActiveMenuId = None }, Cmd.none
                | _ ->
                    let rootResult = removeNodeById id currentTree
                    match rootResult with
                    | Some newRoot -> 
                        let laidOut = layoutTree newRoot 0 (ref 50.0)
                        let newLevels = model.Levels |> Map.add model.ActiveLevel laidOut
                        { model with Levels = newLevels; ConfirmingId = None; ConfirmingAction = NoAction; ActiveMenuId = None }, Cmd.none
                    | None -> model, Cmd.none
        | Elevate ->
            let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
            let node = findNodeById id currentTree
            match node with
            | Some n ->
                // Note: De-elevation is now handled via Delete Root, so this only handles Elevate
                let nextLvlForNode = model.ActiveLevel + 1
                let newAnchors = model.LevelAnchors |> Map.add nextLvlForNode id
                
                // Create or update the tree for the next level, retaining children if it already exists
                let freshRoot = 
                    match model.Levels |> Map.tryFind nextLvlForNode with
                    | Some existingRoot ->
                        { existingRoot with 
                            Id = n.Id
                            Name = n.Name
                            Weight = n.Weight
                            Level = nextLvlForNode }
                        |> fun r -> layoutTree r 0 (ref 50.0)
                    | None ->
                        { n with Level = nextLvlForNode; Children = []; X = 50.0; Y = 50.0 }
                
                let newLevelsAfterAdd = model.Levels |> Map.add nextLvlForNode freshRoot

                // Enforce single elevation: reset others
                let treeWithResets = resetElevatedNodes model.ActiveLevel currentTree
                let updatedCurrentTree = updateNodeById id (fun node -> { node with Level = nextLvlForNode }) treeWithResets
                let finalLevels = newLevelsAfterAdd |> Map.add model.ActiveLevel updatedCurrentTree
                
                { model with Levels = finalLevels; LevelAnchors = newAnchors; ConfirmingId = None; ConfirmingAction = NoAction; ActiveMenuId = None }, Cmd.none
            | None -> model, Cmd.none
        | NoAction -> model, Cmd.none
    | AddChild parentId ->
        let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
        let rec add node =
            if node.Id = parentId then
                let newChild = { Id = Guid.NewGuid(); Name = getRandomName(); Weight = "96"; X = 0.0; Y = 0.0; Children = []; Level = model.ActiveLevel }
                { node with Children = node.Children @ [newChild] }
            else
                { node with Children = node.Children |> List.map add }
        let newRoot = add currentTree
        let laidOut = layoutTree newRoot 0 (ref 50.0)
        let newLevels = model.Levels |> Map.add model.ActiveLevel laidOut
        { model with Levels = newLevels }, Cmd.none
    | UpdateName (id, name) ->
        let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
        let newRoot = updateNodeById id (fun n -> { n with Name = name }) currentTree
        let nextLevels = model.Levels |> Map.add model.ActiveLevel newRoot
        let finalLevels = syncHierarchy nextLevels model.LevelAnchors 0 // Start sync from base level to ensure full propagation
        { model with Levels = finalLevels }, Cmd.none
    | UpdateWeight (id, weight) ->
        let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
        let newRoot = updateNodeById id (fun n -> { n with Weight = weight }) currentTree
        let nextLevels = model.Levels |> Map.add model.ActiveLevel newRoot
        let finalLevels = syncHierarchy nextLevels model.LevelAnchors 0 // Start sync from base level to ensure full propagation
        { model with Levels = finalLevels }, Cmd.none
    | PointerDown ev ->
        let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
        { model with PointerDownPos = None; PendingDragId = None }, Cmd.OfAsync.perform (fun _ -> getSvgInfo js) () (fun info -> 
            let pt = toSvgCoords info (float ev.ClientX) (float ev.ClientY)
            let rec findNode node =
                if pt.X >= node.X - 30.0 && pt.X <= node.X + 30.0 &&
                   pt.Y >= node.Y - 35.0 && pt.Y <= node.Y + 25.0 then Some node.Id
                else node.Children |> List.tryPick findNode
            let hitId = findNode currentTree
            match hitId with
            | Some id when id <> currentTree.Id -> DragStartInternal (id, info, pt)
            | _ -> PointerUpInternal
        )
    | PointerMove ev ->
        let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
        match model.LastMoveMs with
        | Some last when nowMs - last < 16.0 -> model, Cmd.none
        | _ ->
            match model.DraggingId, model.SvgInfo with
            | Some _, Some info ->
                let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
                let pt = toSvgCoords info (float ev.ClientX) (float ev.ClientY)
                let rec findNode node =
                    if pt.X >= node.X - 30.0 && pt.X <= node.X + 30.0 &&
                       pt.Y >= node.Y - 35.0 && pt.Y <= node.Y + 25.0 then Some node.Id
                    else node.Children |> List.tryPick findNode
                
                let targetId = findNode currentTree
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
        let m = { model with PendingDragId = None; PointerDownPos = None }
        let currentTree = m.Levels |> Map.tryFind m.ActiveLevel |> Option.defaultValue m.Levels.[0]
        match m.DraggingId, m.DropTargetId with
        | Some sourceId, Some targetId when sourceId <> targetId ->
            let sourceNode = findNodeById sourceId currentTree
            match sourceNode with
            | None -> { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
            | Some sn ->
                if isDescendant targetId sn then { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
                else
                    let (rootWithoutSource, extracted) = extractNode sourceId currentTree
                    match rootWithoutSource, extracted with
                    | Some rs, Some ex ->
                        let newRoot = insertBefore targetId ex rs
                        let laidOut = layoutTree newRoot 0 (ref 50.0)
                        let newLevels = m.Levels |> Map.add m.ActiveLevel laidOut
                        { m with Levels = newLevels; DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
                    | _ -> { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
        | _ -> { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
    | DragStartInternal (id, info, pt) -> 
        { model with PendingDragId = Some id; SvgInfo = Some info; PointerDownPos = Some pt; DropTargetId = None }, Cmd.none
    | PointerUpInternal ->
        { model with DraggingId = None; PendingDragId = None; DropTargetId = None; SvgInfo = None; PointerDownPos = None }, Cmd.none

// --------------------
// View
// --------------------
let renderNode (node: TreeNode) (prefix: string) (model: SubModel) (isAffected: bool) (dispatch: SubMsg -> unit) =
    let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
    let isRoot = node.Id = currentTree.Id
    let isConfirmingThis = model.ConfirmingId = Some node.Id
    let isMenuOpen = model.ActiveMenuId = Some node.Id
    let isDropTarget = model.DropTargetId = Some node.Id
    let hasHalo = node.Level > model.ActiveLevel
    
    let outerClasses = 
        String.concat " " [
            "node-outer"
            if isAffected && model.ConfirmingAction = Delete && not isRoot then "is-affected"
            if isConfirmingThis && not isRoot then 
                match model.ConfirmingAction with
                | Delete -> "is-confirming"
                | Elevate -> "is-elevating"
                | NoAction -> ""
            if model.DraggingId = Some node.Id && not isRoot then "is-dragging"
            if isDropTarget && not isRoot then "is-drop-target"
            if hasHalo then "is-elevated"
        ]

    div {
        attr.style $"position:absolute; left:{node.X - 30.0}px; top:{node.Y - 35.0}px; width:60px; height:60px; pointer-events:none;"
        
        div {
            attr.``class`` outerClasses
            attr.style "pointer-events:auto;" 
            
            div {
                attr.``class`` "node-inner"
                match isConfirmingThis with
                | true -> 
                    match model.ConfirmingAction with
                    | Delete ->
                        button {
                            attr.``class`` "node-confirm-del"
                            "onpointerdown:stopPropagation" => true
                            on.click (fun _ -> dispatch (ExecuteAction (node.Id, Delete)))
                            text "DELETE"
                        }
                    | Elevate ->
                        button {
                            attr.``class`` "node-confirm-elevate"
                            "onpointerdown:stopPropagation" => true
                            on.click (fun _ -> dispatch (ExecuteAction (node.Id, Elevate)))
                            text "ELEVATE"
                        }
                    | NoAction -> ()

                    button {
                        attr.``class`` "node-confirm-cancel"
                        "onpointerdown:stopPropagation" => true
                        on.click (fun _ -> dispatch CancelAction)
                        text "CANCEL"
                    }
                | false ->
                    concat {
                        div {
                            attr.``class`` "node-menu-container"
                            "onpointerdown:stopPropagation" => true
                            on.pointerdown (fun _ -> dispatch (OpenMenu node.Id))
                            
                            text "☰"
                        }

                        let isAnchorForThisView = 
                            model.ActiveLevel > 0 && 
                            (model.LevelAnchors |> Map.tryFind model.ActiveLevel = Some node.Id)

                        input {
                            attr.``class`` "nodename"
                            attr.value node.Name
                            attr.disabled isAnchorForThisView
                            "onpointerdown:stopPropagation" => true
                            on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
                        }
                        input {
                            attr.``class`` "nodeweight"
                            attr.value node.Weight
                            attr.disabled isAnchorForThisView
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

        if isMenuOpen then
            let hasElevatedDescendants = model.Levels.Keys |> Seq.exists (fun k -> k > model.ActiveLevel)
            
            let canDelete = 
                if isRoot then model.ActiveLevel > 0 && not hasElevatedDescendants
                else 
                    if node.Level > model.ActiveLevel then not node.Children.IsEmpty
                    else true
            
            let canElevate = node.Level = model.ActiveLevel

            div {
                attr.``class`` "node-menu-popup"
                attr.style "pointer-events:auto;"
                "onpointerdown:stopPropagation" => true
                
                div {
                    attr.``class`` (if canDelete then "node-menu-item" else "node-menu-item disabled")
                    match canDelete with
                    | true -> on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, Delete)))
                    | false -> attr.style ""
                    text "Delete"
                }
                
                div {
                    attr.``class`` (if canElevate then "node-menu-item" else "node-menu-item disabled")
                    match canElevate with
                    | true -> on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, Elevate)))
                    | false -> attr.style ""
                    text "Elevate"
                }
            }
    }


let rec findElevatedNode lvl (node: TreeNode) =
    if node.Level = lvl then Some node
    else node.Children |> List.tryPick (findElevatedNode lvl)

let rec filterTreeForLevel lvl (node: TreeNode) =
    { node with Children = node.Children |> List.map (filterTreeForLevel lvl) }

let viewTreeEditor (model: SubModel) (dispatch: SubMsg -> unit) : Node =      
    let currentLvlRoot = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
    let displayTree = currentLvlRoot
    let laidOutDisplayTree = displayTree // Already laid out in update

    let confirmingSubtree = model.ConfirmingId |> Option.bind (fun id -> findNodeById id currentLvlRoot)
    let isAffected nodeId =
        match confirmingSubtree with
        | Some root -> root.Id = nodeId || isDescendant nodeId root
        | None -> false

    let renderConnection (parent: TreeNode) (child: TreeNode) : Node =
        let affected = isAffected child.Id && model.ConfirmingAction = Delete
        let color = if affected then "#E67E22" else "#888"
        let width = if affected then "1.5" else "1"
        svLn().x1($"{parent.X}").y1($"{parent.Y + 5.0}").x2($"{child.X}").y2($"{child.Y - 35.0}")
             .color(color).width(width).Elt()

    let rec flattenTree node = node :: (node.Children |> List.collect flattenTree)
    let nodes = flattenTree laidOutDisplayTree
    let lines = laidOutDisplayTree.Children |> List.collect (fun c -> 
        let rec collect n = n.Children |> List.collect (fun child -> renderConnection n child :: collect child)
        renderConnection laidOutDisplayTree c :: collect c)
    
    let maxX = if nodes.IsEmpty then 60.0 else nodes |> List.map (fun n -> n.X) |> List.max
    let maxY = if nodes.IsEmpty then 60.0 else nodes |> List.map (fun n -> n.Y + 35.0) |> List.max
    let canvasWidth = maxX + 60.0
    let canvasHeight = maxY + 30.0

    let rec renderAll (node: TreeNode) (prefix: string) : Node =
        concat {
            renderNode node prefix model (isAffected node.Id) dispatch
            for i, child in node.Children |> List.indexed do
                renderAll child $"{prefix}.{i + 1}"
        }

    let allNodes = flattenTree currentLvlRoot
    let maxLevel = if model.Levels.IsEmpty then 0 else model.Levels.Keys |> Seq.max
    
    let containerClasses = 
        String.concat " " [
            "tree-container"
            if model.DraggingId.IsSome then "is-dragging-any"
        ]

    let touchAction = if model.DraggingId.IsSome || model.PendingDragId.IsSome then "none" else "pan-x pan-y pinch-zoom"

    let rec flattenTree node = node :: (node.Children |> List.collect flattenTree)
    let allNodes = flattenTree currentLvlRoot
    let maxLevel = model.Levels.Keys |> Seq.max

    concat {
        // Level Nav
        div {
            attr.``class`` "level-controls-container"
            attr.style "width: fit-content; margin-bottom: 5px; z-index: 1000;"
            span { attr.``class`` "level-label"; text "LEVELS:" }
            for i in 0 .. maxLevel do
                button {
                    attr.``class`` (if model.ActiveLevel = i then "level-tab active" else "level-tab")
                    on.pointerdown (fun _ -> dispatch (SetLevel i))
                    text (string i)
                }
        }

        // Tree Container
        div {
            attr.``class`` containerClasses
            on.pointermove (fun ev -> dispatch (PointerMove ev))
            on.pointerup (fun _ -> dispatch PointerUp)

            if model.ActiveMenuId.IsSome then
                div {
                    attr.style "position:fixed; inset:0; z-index:90; background:transparent;"
                    on.click (fun _ -> dispatch CloseMenu)
                }

            div {
                attr.id "tree-canvas-svg"
                attr.``class`` "tree-canvas"
                attr.style $"width:{canvasWidth}px; height:{max 150.0 canvasHeight}px; touch-action:{touchAction};"
                on.pointerdown (fun ev -> dispatch (PointerDown ev))
                
                svg {
                    attr.``class`` "tree-svg"
                    attr.style $"width:{canvasWidth}px; height:{canvasHeight}px;"
                    for line in lines do line
                }
                renderAll laidOutDisplayTree "1"
            }
        }
    }

// --------------------
// Serialization & Initialization
// --------------------
let getOutput (model: SubModel) q w h x o i =
    let rec getNodesWithPrefix prefix node =
        seq {
            yield (node, prefix)
            for i, child in node.Children |> List.indexed do
                yield! getNodesWithPrefix $"{prefix}.{i + 1}" child
        }
    
    let maxLevel = if model.Levels.IsEmpty then 0 else model.Levels.Keys |> Seq.max
    
    let allLvlNodes = 
        [0 .. maxLevel] 
        |> List.map (fun lvl -> 
            model.Levels |> Map.tryFind lvl |> Option.map (fun root -> getNodesWithPrefix "1" root |> Seq.toList)
        )

    let segments = 
        [0 .. maxLevel] |> List.choose (fun lvl ->
            match allLvlNodes.[lvl] with
            | None -> None
            | Some lvlNodes ->
                let eVal = 
                    if lvl = 0 then "0"
                    else
                        // Find prefix of the node in the PREVIOUS level that anchors this level
                        match model.LevelAnchors |> Map.tryFind lvl, allLvlNodes.[lvl - 1] with
                        | Some anchorId, Some prevNodes ->
                            match prevNodes |> List.tryFind (fun (n, _) -> n.Id = anchorId) with
                            | Some (_, prefix) -> prefix
                            | None -> "0"
                        | _ -> "0"

                let attrs = 
                    if lvl = 0 then 
                        $"0/Q={q}/L=0/W={w}/H={h}/X={x}/E={eVal}/O={o}/I={i}"
                    else
                        $"0/Q={q}/L={lvl}/E={eVal}"

                let body = lvlNodes |> List.map (fun (n, p) -> $"({p}/{n.Weight}/{n.Name})") |> String.concat ", "
                Some $"| ({attrs}), {body} |"
        )
    
    String.concat " ; " segments

let buildTreeMap (input: string) : Map<int, TreeNode> =
    let levels = input.Split(';', StringSplitOptions.RemoveEmptyEntries)
    
    let allParts = 
        levels |> Array.mapi (fun i lvl ->
            let clean = lvl.Trim().Trim('|').Trim()
            let firstBlock = clean.Split(')', 2) |> Array.head |> fun s -> s.Trim('(', ' ')
            let isAttrs = firstBlock.StartsWith "0/"
            
            let lVal = if isAttrs && firstBlock.Contains("L=") then firstBlock.Split("L=").[1].Split("/").[0] |> int else 0
            let sVal = if isAttrs && firstBlock.Contains("S=") then Some (firstBlock.Split("S=").[1].Split("/").[0]) else None
            
            let nodesStr = if isAttrs then clean.Split(')', 2) |> Array.last |> fun s -> s.Trim(',', ' ') else clean
            let nodes = nodesStr.Split("),", StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map (fun s -> s.Trim([| '('; ')'; ' ' |]))
                        |> Array.filter (fun s -> not (s.StartsWith "0/") && not (String.IsNullOrWhiteSpace s))
            
            (lVal, sVal, nodes)
        )

    // Flat list of nodes with their level and parent
    let nodeData = 
        allParts |> Array.collect (fun (lvl, s, nodes) ->
            nodes |> Array.map (fun n ->
                let bits = n.Split '/'
                let path = bits.[0].Split('.') |> Array.map int |> Array.toList
                let weight = bits.[1]
                let name = bits.[2]
                (path, weight, name, lvl)
            )
        )
        |> Array.toList
        |> List.distinctBy (fun (p, _, _, _) -> p) // Deduplicate nodes appearing across levels

    let rec build (lvl: int) (prefix: int list) =
        nodeData 
        |> List.filter (fun (path, _, _, nLvl) -> 
            nLvl = lvl && ((prefix = [] && path.Length = 1) || (path.Length = prefix.Length + 1 && List.take prefix.Length path = prefix)))
        |> List.map (fun (path, weight, name, lvl) -> 
            { Id = Guid.NewGuid(); Name = name; Weight = weight; X = 0.0; Y = 0.0; Children = build lvl path; Level = lvl })

    [0 .. (if nodeData.IsEmpty then 0 else nodeData |> List.map (fun (_,_,_,l) -> l) |> List.max)]
    |> List.map (fun lvl -> 
        let root = build lvl [] |> List.tryHead |> Option.defaultValue { Id = Guid.NewGuid(); Name = "Root"; Weight = "96"; X = 0.0; Y = 0.0; Children = []; Level = lvl }
        lvl, layoutTree root 0 (ref 50.0))
    |> Map.ofList

let initModel (inputString: string) : SubModel =
    let levelsMap = buildTreeMap inputString
    
    { Levels = levelsMap
      ActiveLevel = 0
      LevelAnchors = Map.empty
      ConfirmingId = None
      ConfirmingAction = NoAction
      ActiveMenuId = None
      DraggingId = None
      PendingDragId = None
      DropTargetId = None
      SvgInfo = None
      PointerDownPos = None
      LastMoveMs = None }



