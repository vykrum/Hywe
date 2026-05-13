namespace Hywe.Node

open System
open Elmish
open Hywe.Core.Lexel

module Serialization =

    /// Standardizes Hywe syntax, ensuring markers, attributes and correctly formatted paths
    let preprocessCode (code: string) : string =
        if String.IsNullOrWhiteSpace code then "" else
        
        let levels = processFullString code
        if levels.IsEmpty then 
            // Fallback for raw node list without parentheses
            if code.Contains "/" then $"L0(Q=VRCCNE/L=0/X=1)({code.Trim()})"
            else code
        else
            let processedLevels = 
                levels |> List.mapi (fun i levelData ->
                    let marker = 
                        if String.IsNullOrWhiteSpace levelData.Marker then sprintf "L%d" i 
                        else levelData.Marker
                    
                    let attrs = levelData.Attributes
                    let attrStr = 
                        let w = match attrs.Width with Some v -> $"/W={v}" | None -> ""
                        let h = match attrs.Height with Some v -> $"/H={v}" | None -> ""
                        $"Q={attrs.Sequence}/L={attrs.Level}/X={attrs.Scale}/E={attrs.Entry}/O={attrs.OuterBoundary}/I={attrs.Islands}/T={attrs.Thickness}{w}{h}"
                    
                    let nodes = 
                        levelData.Tree 
                        |> List.collect id 
                        |> List.map (fun n -> 
                            let extr = match n.Extrusion with Some v -> $"/{v}" | None -> ""
                            let baseStr = match n.Base with Some b -> $"/B={b}" | None -> ""
                            $"({n.Id}/{n.Area}/{n.Label}{extr}{baseStr})")
                        |> String.concat ""

                    $"{marker}({attrStr}){nodes}"
                )

            String.Join("", processedLevels)

    /// Parses code into a list of hierarchical trees (one per level)
    let parseToTrees (code: string) : TreeNode list =
        let processed = preprocessCode code
        let levels = processFullString processed
        
        levels |> List.mapi (fun lvlIdx levelData ->
            let nodes = levelData.Tree |> List.collect id
            
            // Find the root (path "1") or default to the first node
            let rootNode = 
                nodes |> List.tryFind (fun n -> n.Id = "1")
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
        let maxLevel = if model.Levels.IsEmpty then 0 else model.Levels.Keys |> Seq.max
        let bases = 
            [0 .. maxLevel - 1] |> List.scan (fun currentSum lvl ->
                let root = model.Levels |> Map.tryFind lvl |> Option.defaultValue { Id = Guid.NewGuid(); Name = "Root"; Weight = "100"; X = 0.0; Y = 0.0; Children = []; Level = lvl; Extrusion = 3.0; Base = None }
                currentSum + root.Extrusion
            ) 0.0
        
        let lastBase = match List.tryLast bases with Some b -> b | None -> 0.0
        let terminal = lastBase + model.TopExtrusion
        (bases @ [terminal]) |> List.toArray

    let getOutput (model: SubModel) (qMap: Map<int, string>) w h x o i =
        let rec getNodesWithPrefix prefix node =
            seq {
                yield (node, prefix)
                yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getNodesWithPrefix $"{prefix}.{i + 1}" child)
            }
        
        let maxLevel = if model.Levels.IsEmpty then 0 else model.Levels.Keys |> Seq.max
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
                    let lStr = if currentL = floor currentL then string (int currentL) else string currentL

                    let tAttr = 
                        match lvl = maxLevel with
                        | true -> 
                            let tVal = model.TopExtrusion
                            match tVal = floor tVal with true -> $"/T={int tVal}" | false -> $"/T={tVal}" 
                        | false -> ""
                    
                    let qVal = qMap |> Map.tryFind lvl |> Option.defaultValue "VRCCNE"
                    let bVal = if lvl = 0 then "0" else eVal
                    let attrs = $"Q={qVal}/L={lStr}/W={w}/H={h}/X={x}/E={eVal}/B={bVal}/O={o}/I={i}{tAttr}"

                    let body = 
                        lvlNodes 
                        |> List.map (fun (n, p) -> 
                            let extrStr = match n.Extrusion = 3.0 with true -> "" | false -> $"/{n.Extrusion}"
                            let baseStr = match n.Base with Some b -> $"/B={b}" | None -> ""
                            $"({p}/{n.Weight}/{n.Name}{extrStr}{baseStr})") 
                        |> String.concat ""
                    
                    let marker = if lvl = 0 then "L0" else sprintf "L%d" lvl
                    Some $"{marker}({attrs}){body}"
            )
        
        String.concat "" segments

    let buildTreeMap (input: string) =
        let levels = processFullString input
        
        let allParts = 
            levels |> List.toArray |> Array.mapi (fun i levelData ->
                let attrs = levelData.Attributes
                let tVal = attrs.Thickness
                let sVal = Some attrs.Scale
                let eVal = attrs.Entry
                let nodes = levelData.Tree |> List.collect id
                (i, tVal, sVal, nodes, eVal)
            )

        let nodeData = 
            allParts |> Array.collect (fun (lvl, t, s, nodes, e) ->
                nodes |> List.map (fun n ->
                    (n.Id.Split('.') |> Array.map int |> Array.toList, string n.Area, n.Label, n.Extrusion |> Option.defaultValue 3.0, lvl, n.Base)
                ) |> List.toArray
            )
            |> Array.toList
            |> List.distinctBy (fun (p, _, _, _, l, _) -> (p, l))

        let rec build (lvl: int) (prefix: int list) (currentGuidMap: Map<int * string, Guid>) =
            let nodesToBuild = 
                nodeData 
                |> List.filter (fun (path, _, _, _, nLvl, _) -> 
                    nLvl = lvl && ((prefix = [] && path.Length = 1) || (path.Length = prefix.Length + 1 && List.take prefix.Length path = prefix)))
            
            nodesToBuild |> List.fold (fun (nodes, guidMap) (path, weight, name, extrusion, lvl, bVal) ->
                let pathStr = String.Join(".", path)
                let id, guidMapAfterId = 
                    match lvl > 0 && prefix = [] with
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
                
                let children, guidMapAfterChildren = build lvl path guidMapAfterId
                let node = { Id = id; Name = name; Weight = weight; X = 0.0; Y = 0.0; Children = children; Level = lvl; Extrusion = extrusion; Base = bVal }
                nodes @ [node], guidMapAfterChildren
            ) ([], currentGuidMap)

        let maxLvl = match nodeData.IsEmpty with true -> 0 | false -> nodeData |> List.map (fun (_,_,_,_,l,_) -> l) |> List.max
        
        let levelsList, finalGuidMap = 
            [0 .. maxLvl] |> List.fold (fun (acc, currentGuidMap) lvl ->
                let rootNodes, nextGuidMap = build lvl [] currentGuidMap
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

        levelsMapFinal, levelAnchors, topExtrusion

    let initModel (inputString: string) : SubModel =
        let levelsMap, levelAnchors, topExtrusion = buildTreeMap inputString
        
        { Levels = levelsMap
          ActiveLevel = 0
          LevelAnchors = levelAnchors
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
