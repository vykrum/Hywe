namespace Hywe.Node

open System
open Elmish
open Hywe.Core.Lexel

module Serialization =

    /// Standardizes Hywe syntax, ensuring markers, attributes and correctly formatted paths
    let preprocessCode (code: string) : string =
        match String.IsNullOrWhiteSpace code with
        | true -> ""
        | false ->
            let levels = processFullString code
            match levels.IsEmpty with
            | true -> 
                match code.Contains "/" with
                | true -> $"L0(Q=VRCCNE/L=0/X=1)({code.Trim()})"
                | false -> code
            | false ->
                let processedLevels = 
                    levels |> List.mapi (fun i levelData ->
                        let marker, attrs, tree = 
                            match levelData with
                            | Level l -> l.Marker, l.Attributes, l.Tree
                            | Nest n -> n.Marker, n.Attributes, n.Tree
                            
                        let finalMarker = 
                            match String.IsNullOrWhiteSpace marker with
                            | true -> sprintf "L%d" i 
                            | false -> marker
                        
                        let attrStr = 
                            let w = match attrs.Width with Some v -> $"/W={v}" | None -> ""
                            let h = match attrs.Height with Some v -> $"/H={v}" | None -> ""
                            $"Q={attrs.Sequence}/L={attrs.Level}/X={attrs.Scale}/E={attrs.Entry}/O={attrs.OuterBoundary}/I={attrs.Islands}/T={attrs.Thickness}{w}{h}"
                        
                        let nodes = 
                            tree 
                            |> List.collect id 
                            |> List.map (fun n -> 
                                let extr = match n.Extrusion with Some v -> $"/{v}" | None -> ""
                                let baseStr = match n.Base with Some b -> $"/B={b}" | None -> ""
                                let idx = n.Id.IndexOf('.')
                                let rawId = match idx >= 0 with | true -> n.Id.Substring(idx + 1) | false -> n.Id
                                $"({rawId}/{n.Area}/{n.Label}{extr}{baseStr})")
                            |> String.concat ""

                        $"{finalMarker}({attrStr}){nodes}"
                    )

                String.Join("", processedLevels)

    /// Parses code into a list of hierarchical trees (one per level)
    let parseToTrees (code: string) : TreeNode list =
        let processed = preprocessCode code
        let levels = processFullString processed
        
        levels |> List.mapi (fun lvlIdx levelData ->
            let tree = match levelData with | Level l -> l.Tree | Nest n -> n.Tree
            let nodes = tree |> List.collect id
            
            // Find the root (path "1") or default to the first node
            let rootNode = 
                nodes |> List.tryFind (fun n -> 
                    let idx = n.Id.IndexOf('.')
                    let rawId = match idx >= 0 with | true -> n.Id.Substring(idx + 1) | false -> n.Id
                    rawId = "1")
                |> Option.orElse (nodes |> List.tryHead)

            match rootNode with
            | None ->
                { Id = Guid.NewGuid(); Name = sprintf "Level %d" lvlIdx; Weight = "0"; X = 0.0; Y = 0.0; Children = []; Level = lvlIdx; Extrusion = 3.0; Base = None }
            | Some root ->
                let rec build (n: LexelNode) =
                    let children = 
                        nodes 
                        |> List.filter (fun c -> c.Id.StartsWith(n.Id + ".") && not (c.Id.Substring(n.Id.Length + 1).Contains(".")))
                        |> List.map build
                    { 
                        Id = Guid.NewGuid()
                        Name = n.Label
                        Weight = string n.Area
                        X = 0.0
                        Y = 0.0
                        Children = children
                        Level = lvlIdx
                        Extrusion = n.Extrusion |> Option.defaultValue 3.0
                        Base = n.Base
                    }
                build root
        )

    let getElevations (model: SubModel) =
        let maxLevel = match model.Levels.IsEmpty with | true -> 0 | false -> model.Levels.Keys |> Seq.max
        let bases = 
            [0 .. maxLevel - 1] |> List.scan (fun currentSum lvl ->
                let root = model.Levels |> Map.tryFind lvl |> Option.defaultValue { Id = Guid.NewGuid(); Name = "Root"; Weight = "100"; X = 0.0; Y = 0.0; Children = []; Level = lvl; Extrusion = 3.0; Base = None }
                currentSum + root.Extrusion
            ) 0.0
        
        let lastBase = match List.tryLast bases with Some b -> b | None -> 0.0
        let terminal = lastBase + model.TopExtrusion
        (bases @ [terminal]) |> List.toArray

    let getOutput (model: SubModel) (qMap: Map<int, string>) w h x b o i =
        let rec getNodesWithPrefix prefix node =
            seq {
                yield (node, prefix)
                yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getNodesWithPrefix $"{prefix}.{i + 1}" child)
            }
        
        let maxLevel = match model.Levels.IsEmpty with | true -> 0 | false -> model.Levels.Keys |> Seq.max
        let elevations = getElevations model

        let allLvlNodes = 
            [0 .. maxLevel] 
            |> List.map (fun lvl -> 
                model.Levels |> Map.tryFind lvl |> Option.map (fun root -> getNodesWithPrefix "1" root |> Seq.toList)
            )

        let segments = 
            [0 .. maxLevel] |> List.choose (fun lvl ->
                match allLvlNodes |> List.tryItem lvl |> Option.flatten with
                | None -> None
                | Some lvlNodes ->
                    let eVal = 
                        match lvl = 0 with
                        | true -> "0"
                        | false ->
                            match model.LevelAnchors |> Map.tryFind lvl, allLvlNodes |> List.tryItem (lvl - 1) |> Option.flatten with
                            | Some anchorId, Some prevNodes ->
                                match prevNodes |> List.tryFind (fun (n, _) -> n.Id = anchorId) with
                                | Some (_, prefix) -> prefix
                                | None -> "0"
                            | _ -> "0"

                    let currentL = elevations.[lvl]
                    let lStr = match currentL = floor currentL with | true -> string (int currentL) | false -> string currentL

                    let tAttr = 
                        match lvl = maxLevel with
                        | true -> 
                            let tVal = model.TopExtrusion
                            match tVal = floor tVal with true -> $"/T={int tVal}" | false -> $"/T={tVal}" 
                        | false -> ""
                    
                    let qVal = qMap |> Map.tryFind lvl |> Option.defaultValue "VRCCNE"
                    let bVal = match lvl = 0 with | true -> (if String.IsNullOrWhiteSpace b then "0" else b) | false -> eVal
                    let attrs = $"Q={qVal}/L={lStr}/W={w}/H={h}/X={x}/E={eVal}/B={bVal}/O={o}/I={i}{tAttr}"

                    let body = 
                        lvlNodes 
                        |> List.map (fun (n, p) -> 
                            let extrStr = match n.Extrusion = 3.0 with true -> "" | false -> $"/{n.Extrusion}"
                            let baseStr = match n.Base with Some b -> $"/B={b}" | None -> ""
                            $"({p}/{n.Weight}/{n.Name}{extrStr}{baseStr})") 
                        |> String.concat ""
                    
                    let marker = match lvl = 0 with | true -> "L0" | false -> sprintf "L%d" lvl
                    Some $"{marker}({attrs}){body}"
            )
        
        let nestSegments =
            model.Nests |> Map.toList |> List.map (fun (nId, root) ->
                let nodes = getNodesWithPrefix "1" root |> Seq.toList
                let anchorId = model.NestAnchors |> Map.tryFind nId |> Option.defaultValue Guid.Empty
                let anchorInfo = 
                    allLvlNodes |> List.indexed |> List.tryPick (fun (lvl, lvlNodesOpt) ->
                        match lvlNodesOpt with
                        | Some nodesList -> 
                            nodesList |> List.tryFind (fun (n, _) -> n.Id = anchorId)
                            |> Option.map (fun (_, prefix) -> (lvl, prefix))
                        | None -> None
                    )
                
                let (parentLvl, bVal) = match anchorInfo with Some (l, p) -> (l, p) | None -> (0, "0")
                
                let currentL = match parentLvl < elevations.Length with | true -> elevations.[parentLvl] | false -> 0.0
                
                let attrs = $"Q=VRCCNE/L={parentLvl}/W={w}/H={h}/X={x}/E=0/B={bVal}/O={o}/I={i}"
                
                let body = 
                    nodes 
                    |> List.map (fun (n, p) -> 
                        let extrStr = match n.Extrusion = 3.0 with true -> "" | false -> $"/{n.Extrusion}"
                        let baseStr = match n.Base with Some b -> $"/B={b}" | None -> ""
                        $"({p}/{n.Weight}/{n.Name}{extrStr}{baseStr})") 
                    |> String.concat ""
                
                $"N{nId}({attrs}){body}"
            )

        String.concat "" (segments @ nestSegments)

    let buildTreeMap (input: string) =
        let blocks = processFullString input
        let levelBlocks = blocks |> List.choose (function | Level l -> Some l | _ -> None)
        let nestBlocks = blocks |> List.choose (function | Nest n -> Some n | _ -> None)
        
        let allParts = 
            levelBlocks |> List.toArray |> Array.mapi (fun i l ->
                let attrs = l.Attributes
                let tree = l.Tree
                let tVal = attrs.Thickness
                let sVal = Some attrs.Scale
                let eVal = attrs.Entry
                let nodes = tree |> List.collect id
                (i, tVal, sVal, nodes, eVal)
            )

        let nodeData = 
            allParts |> Array.collect (fun (lvl, t, s, nodes, e) ->
                nodes |> List.map (fun n ->
                    let idx = n.Id.IndexOf('.')
                    let rawId = match idx >= 0 with | true -> n.Id.Substring(idx + 1) | false -> n.Id
                    (rawId.Split('.') |> Array.map int |> Array.toList, string n.Area, n.Label, n.Extrusion |> Option.defaultValue 3.0, lvl, n.Base)
                ) |> List.toArray
            )
            |> Array.toList
            |> List.distinctBy (fun (p, _, _, _, l, _) -> (p, l))

        let rec build (lvl: int) (prefix: int list) (currentGuidMap: Map<int * string, Guid>) (srcData: (int list * string * string * float * int * string option) list) (isNest: bool) =
            let nodesToBuild = 
                srcData 
                |> List.filter (fun (path, _, _, _, nLvl, _) -> 
                    nLvl = lvl && ((prefix = [] && path.Length = 1) || (path.Length = prefix.Length + 1 && List.take prefix.Length path = prefix)))
            
            nodesToBuild |> List.fold (fun (nodes, guidMap) (path, weight, name, extrusion, lvl, bVal) ->
                let pathStr = String.Join(".", path)
                let id, guidMapAfterId = 
                    match lvl > 0 && prefix = [] && not isNest with
                    | true ->
                        let _, _, _, _, eVal = allParts.[lvl]
                        match eVal <> "0" with
                        | true ->
                            match guidMap |> Map.tryFind (lvl - 1, eVal) with
                            | Some parentGuid -> parentGuid, guidMap
                            | None -> 
                                let g = Guid.NewGuid()
                                g, guidMap |> Map.add (lvl, pathStr) g
                        | false -> 
                            let g = Guid.NewGuid()
                            g, guidMap |> Map.add (lvl, pathStr) g
                    | false -> 
                        let g = Guid.NewGuid()
                        g, guidMap |> Map.add (lvl, pathStr) g
                
                let children, guidMapAfterChildren = build lvl path guidMapAfterId srcData isNest
                let node = { Id = id; Name = name; Weight = weight; X = 0.0; Y = 0.0; Children = children; Level = lvl; Extrusion = extrusion; Base = bVal }
                nodes @ [node], guidMapAfterChildren
            ) ([], currentGuidMap)

        let maxLvl = match nodeData.IsEmpty with true -> 0 | false -> nodeData |> List.map (fun (_,_,_,_,l,_) -> l) |> List.max
        
        let levelsList, finalGuidMap = 
            [0 .. maxLvl] |> List.fold (fun (acc, currentGuidMap) lvl ->
                let rootNodes, nextGuidMap = build lvl [] currentGuidMap nodeData false
                let root = rootNodes |> List.tryHead |> Option.defaultValue { Id = Guid.NewGuid(); Name = "Root"; Weight = TreeOps.getRandomWeight(); X = 0.0; Y = 0.0; Children = []; Level = lvl; Extrusion = 3.0; Base = None }
                let laidOut = fst (TreeOps.layoutTree root 0 50.0)
                acc @ [lvl, laidOut], nextGuidMap
            ) ([], Map.empty)

        let levelsMap = Map.ofList levelsList

        let levelAnchors = 
            allParts |> Array.choose (fun (lvl, _, _, _, eVal) ->
                match lvl > 0 && eVal <> "0" with
                | true ->
                    match finalGuidMap |> Map.tryFind (lvl - 1, eVal) with
                    | Some guid -> Some (lvl, guid)
                    | None -> None
                | false -> None
            ) |> Map.ofArray

        let levelsMapFinal =
            levelsMap |> Map.map (fun lvl tree ->
                let higherAnchors = levelAnchors |> Map.filter (fun k _ -> k > lvl)
                let rec update (n: TreeNode) =
                    let nLvl =
                        higherAnchors |> Map.tryPick (fun k v -> match v = n.Id with true -> Some k | false -> None)
                        |> Option.defaultValue n.Level
                    { n with Level = nLvl; Children = n.Children |> List.map update }
                update tree
            )

        let topExtrusion = 
            match Array.tryLast allParts with Some (_, t, _, _, _) -> t | None -> 3.0

        let nestDataList =
            nestBlocks |> List.map (fun n ->
                let nId = match Int32.TryParse(n.Marker.Substring(1)) with true, v -> v | _ -> 1
                let attrs = n.Attributes
                let tree = n.Tree
                let bVal = attrs.Base
                let parentLvl = attrs.Level
                let nodes = tree |> List.collect id
                let parsedNodes = 
                    nodes |> List.map (fun nd ->
                        let idx = nd.Id.IndexOf('.')
                        let rawId = match idx >= 0 with | true -> nd.Id.Substring(idx + 1) | false -> nd.Id
                        (rawId.Split('.') |> Array.map int |> Array.toList, string nd.Area, nd.Label, nd.Extrusion |> Option.defaultValue 3.0, parentLvl, nd.Base)
                    )
                (nId, bVal, parentLvl, parsedNodes)
            )

        let nestsMapAndAnchors =
            nestDataList |> List.map (fun (nId, bVal, parentLvl, nodesData) ->
                let rootNodes, _ = build parentLvl [] Map.empty nodesData true
                let root = rootNodes |> List.tryHead |> Option.defaultValue { Id = Guid.NewGuid(); Name = "<nest>"; Weight = "100"; X = 0.0; Y = 0.0; Children = []; Level = parentLvl; Extrusion = 3.0; Base = None }
                let laidOut = fst (TreeOps.layoutTree root 0 50.0)
                let anchorId = 
                    match finalGuidMap |> Map.tryFind (parentLvl, bVal) with
                    | Some g -> g
                    | None -> Guid.Empty
                (nId, laidOut), (nId, anchorId)
            )
            
        let nestsMapFinal = nestsMapAndAnchors |> List.map fst |> Map.ofList
        let nestAnchorsFinal = nestsMapAndAnchors |> List.map snd |> Map.ofList

        levelsMapFinal, levelAnchors, topExtrusion, nestsMapFinal, nestAnchorsFinal

    let initModel (inputString: string) : SubModel =
        let levelsMap, levelAnchors, topExtrusion, nestsMap, nestAnchors = buildTreeMap inputString
        
        { Levels = levelsMap
          Nests = nestsMap
          ActiveLevel = 0
          ActiveNest = None
          LevelAnchors = levelAnchors
          NestAnchors = nestAnchors
          ConfirmingId = None
          ActiveActionId = ActionIds.NoAction
          ActiveMenuId = None
          DraggingId = None
          PendingDragId = None
          DropTargetId = None
          SvgInfo = None
          PointerDownPos = None
          LastMoveMs = None
          TopExtrusion = topExtrusion }
