namespace Hywe.Node

open System

module TreeOps =

    let getRandomWeight () = 
        let rng = Random()
        rng.Next(75, 151).ToString()

    let randomNames = ["<Hive>"; "<Cell>"; "<Comb>"; "<Hex>"; "<Core>"; "<Dock>"; "<Ring>"; "<Link>"; "<Arc>"; "<Mod>"; "<Buzz>"; "<Wax>"; "<Sting>"; "<Veil>"; "<Arch>"; "<Glow>"; "<Path>"; "<Air>"; "<Clad>"; "<Echo>"; "<Dawn>"; "<Brood>"; "<Guard>"; "<Swarm>"; "<Nect>"; "<Pupa>"; "<Drone>"; "<Queen>"; "<Field>"; "<Trail>"]

    let getRandomName () = 
        let rng = Random()
        randomNames.[rng.Next(randomNames.Length)]

    let rec isDescendant (targetId: Guid) (potentialParent: TreeNode) : bool =
        potentialParent.Children |> List.exists (fun c -> c.Id = targetId || isDescendant targetId c)

    let rec findNodeById (id: Guid) (node: TreeNode) : TreeNode option =
        match node.Id = id with
        | true -> Some node
        | false -> node.Children |> List.tryPick (findNodeById id)

    let rec addChildToNodeById (node: TreeNode) parentId =
        match node.Id = parentId with
        | true ->
            let newChild = { Id = Guid.NewGuid(); Name = getRandomName(); Weight = "96"; X = 0.0; Y = 0.0; Children = []; Level = node.Level; Extrusion = 3.0; Base = None }
            { node with Children = node.Children @ [newChild] }
        | false -> { node with Children = node.Children |> List.map (fun c -> addChildToNodeById c parentId) }

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

    let rec resetElevatedNodes targetLvl (node: TreeNode) =
        let newNode = match node.Level > targetLvl with true -> { node with Level = targetLvl } | false -> node
        { newNode with Children = newNode.Children |> List.map (resetElevatedNodes targetLvl) }

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

    let rec extractNode (id: Guid) (node: TreeNode) : TreeNode option * TreeNode option =
        match node.Id = id with
        | true -> (None, Some node)
        | false ->
            let newChildren, extracted = 
                node.Children |> List.fold (fun (acc, found) c ->
                    let (newNode, maybeFound) = extractNode id c
                    let nextAcc = match newNode with Some n -> acc @ [n] | None -> acc
                    let nextFound = match maybeFound with Some _ -> maybeFound | None -> found
                    nextAcc, nextFound
                ) ([], None)
            (Some { node with Children = newChildren }, extracted)

    let rec insertBefore (targetId: Guid) (nodeToInsert: TreeNode) (node: TreeNode) : TreeNode =
        let rec insertInList list =
            match list with
            | [] -> []
            | h :: t ->
                match h.Id = targetId with
                | true -> nodeToInsert :: h :: t
                | false -> h :: insertInList t
        
        match node.Children |> List.exists (fun c -> c.Id = targetId) with
        | true -> { node with Children = insertInList node.Children }
        | false -> { node with Children = node.Children |> List.map (insertBefore targetId nodeToInsert) }

    let rec layoutTree (node: TreeNode) (depth: int) (xStart: float) : TreeNode * float =
        let y = float depth * 65.0 + 30.0
        match node.Children.IsEmpty with
        | true ->
            let x = xStart
            { node with X = x; Y = y }, x + 60.0
        | false ->
            let laidOutChildren, finalX = 
                node.Children |> List.fold (fun (acc, currentX) child ->
                    let newNode, nextX = layoutTree child (depth + 1) currentX
                    acc @ [newNode], nextX
                ) ([], xStart)
            let firstX = laidOutChildren.Head.X
            let lastX = (List.last laidOutChildren).X
            let x = (firstX + lastX) / 2.0
            { node with X = x; Y = y; Children = laidOutChildren }, finalX
