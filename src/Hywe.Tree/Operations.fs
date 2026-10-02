/// <summary>
/// Tree mutation and structural operations including insertion, removal, descendant checking,
/// hierarchy synchronization, and automatic 2D layout.
/// </summary>
module TreeOps

open System

/// <summary> Generates a random area weight string between 75 and 150. </summary>
let getRandomWeight () = 
    let rng = Random()
    rng.Next(75, 151).ToString()

let randomNames = ["<Hive>"; "<Cell>"; "<Comb>"; "<Hex>"; "<Core>"; "<Dock>"; "<Ring>"; "<Link>"; "<Arc>"; "<Mod>"; "<Buzz>"; "<Wax>"; "<Sting>"; "<Veil>"; "<Arch>"; "<Glow>"; "<Path>"; "<Air>"; "<Clad>"; "<Echo>"; "<Dawn>"; "<Brood>"; "<Guard>"; "<Swarm>"; "<Nect>"; "<Pupa>"; "<Drone>"; "<Queen>"; "<Field>"; "<Trail>"]

/// <summary> Selects a random architectural room name from a predefined pool. </summary>
let getRandomName () = 
    let rng = Random()
    randomNames.[rng.Next(randomNames.Length)]

/// <summary>
/// Checks whether a target node ID exists as a descendant of a potential parent node.
/// </summary>
let rec isDescendant (targetId: Guid) (potentialParent: TreeNode) : bool =
    potentialParent.Children |> List.exists (fun c -> c.Id = targetId || isDescendant targetId c)

/// <summary> Recursively searches for a tree node by its unique identifier. </summary>
let rec findNodeById (id: Guid) (node: TreeNode) : TreeNode option =
    if node.Id = id then Some node
    else node.Children |> List.tryPick (findNodeById id)

/// <summary>
/// Appends a new child node with random name and default attributes to the target parent.
/// </summary>
let rec addChildToNodeById (node: TreeNode) (parentId: Guid) : TreeNode =
    if node.Id = parentId then
        let childLevel = match node.Children with c :: _ -> c.Level | [] -> node.Level
        let newChild = { Id = Guid.NewGuid(); Name = getRandomName(); Weight = "96"; X = 0.0; Y = 0.0; Children = []; Level = childLevel; Extrusion = 3.0; Base = None; Color = None }
        { node with Children = node.Children @ [newChild] }
    else
        { node with Children = node.Children |> List.map (fun c -> addChildToNodeById c parentId) }

/// <summary> Recursively removes the node matching the given identifier from the tree. </summary>
let rec removeNodeById (id: Guid) (node: TreeNode) : TreeNode option =
    if node.Id = id then None
    else
        let newChildren = node.Children |> List.choose (removeNodeById id)
        Some { node with Children = newChildren }

/// <summary> Applies a transformation function to the node matching the given identifier. </summary>
let rec updateNodeById (id: Guid) (updateFn: TreeNode -> TreeNode) (node: TreeNode) : TreeNode =
    if node.Id = id then updateFn node
    else { node with Children = node.Children |> List.map (updateNodeById id updateFn) }

/// <summary>
/// Resets node levels that exceed a target level down to that target level.
/// </summary>
let rec resetElevatedNodes (targetLvl: int) (node: TreeNode) : TreeNode =
    let newNode = if node.Level > targetLvl then { node with Level = targetLvl } else node
    { newNode with Children = newNode.Children |> List.map (resetElevatedNodes targetLvl) }

/// <summary>
/// Synchronizes node attributes across level boundaries based on anchor identifiers.
/// </summary>
let rec syncHierarchy (levels: Map<int, TreeNode>) (anchors: Map<int, Guid>) (lvl: int) =
    match anchors |> Map.tryFind (lvl + 1) with
    | Some anchorId ->
        match levels |> Map.tryFind lvl |> Option.bind (findNodeById anchorId) with
        | Some anchorNode ->
            match levels |> Map.tryFind (lvl + 1) with
            | Some childTree ->
                let updatedChildTree = { childTree with Name = anchorNode.Name; Weight = anchorNode.Weight }
                let nextLevels = levels |> Map.add (lvl + 1) updatedChildTree
                syncHierarchy nextLevels anchors (lvl + 1)
            | None -> levels
        | None -> levels
    | None -> levels

/// <summary>
/// Detaches and returns a node by ID from a tree along with the remaining tree structure.
/// </summary>
let rec extractNode (id: Guid) (node: TreeNode) : TreeNode option * TreeNode option =
    if node.Id = id then (None, Some node)
    else
        let newChildrenRev, extracted = 
            (([], None), node.Children)
            ||> List.fold (fun (acc, found) c ->
                let (newNode, maybeFound) = extractNode id c
                let nextAcc = match newNode with Some n -> n :: acc | None -> acc
                let nextFound = Option.orElse maybeFound found
                nextAcc, nextFound
            )
        (Some { node with Children = List.rev newChildrenRev }, extracted)

/// <summary>
/// Inserts a node before a specified sibling node within the tree hierarchy.
/// </summary>
let rec insertBefore (targetId: Guid) (nodeToInsert: TreeNode) (node: TreeNode) : TreeNode =
    let rec insertInList list =
        match list with
        | [] -> []
        | h :: t when h.Id = targetId -> nodeToInsert :: h :: t
        | h :: t -> h :: insertInList t
    
    if node.Children |> List.exists (fun c -> c.Id = targetId) then
        { node with Children = insertInList node.Children }
    else
        { node with Children = node.Children |> List.map (insertBefore targetId nodeToInsert) }

/// <summary>
/// Inserts a node after a specified sibling node within the tree hierarchy.
/// </summary>
let rec insertAfter (targetId: Guid) (nodeToInsert: TreeNode) (node: TreeNode) : TreeNode =
    let rec insertInList list =
        match list with
        | [] -> []
        | h :: t when h.Id = targetId -> h :: nodeToInsert :: t
        | h :: t -> h :: insertInList t
    
    if node.Children |> List.exists (fun c -> c.Id = targetId) then
        { node with Children = insertInList node.Children }
    else
        { node with Children = node.Children |> List.map (insertAfter targetId nodeToInsert) }

/// <summary>
/// Appends a node as a child of a specified parent node within the tree hierarchy.
/// </summary>
let rec attachChild (parentId: Guid) (nodeToInsert: TreeNode) (node: TreeNode) : TreeNode =
    if node.Id = parentId then
        { node with Children = node.Children @ [nodeToInsert] }
    else
        { node with Children = node.Children |> List.map (attachChild parentId nodeToInsert) }

/// <summary>
/// Computes 2D Cartesian coordinates (X, Y) for tree nodes based on depth and subtree widths.
/// </summary>
let rec layoutTree (node: TreeNode) (depth: int) (xStart: float) : TreeNode * float =
    let y = float depth * 65.0 + 30.0
    if node.Children.IsEmpty then
        let x = xStart
        { node with X = x; Y = y }, x + 76.0
    else
        let laidOutChildren, finalX = 
            (xStart, node.Children)
            ||> List.mapFold (fun currentX child ->
                layoutTree child (depth + 1) currentX)
        let firstX = laidOutChildren.Head.X
        let lastX = (List.last laidOutChildren).X
        let x = (firstX + lastX) / 2.0
        { node with X = x; Y = y; Children = laidOutChildren }, finalX
