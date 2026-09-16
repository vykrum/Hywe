module PageTreeFiltering

open System
open Hywe.Node
open Hywe.Core
open Hywe.Core.Coxel
open Hywe.Core.Hexel
open ModelTypes

let getHierarchicalIdMap (tree: Hywe.Node.SubModel) =
    let rec traverse (m: string) (prefix: string) (node: Hywe.Node.TreeNode) =
        seq {
            yield node.Id, $"{m}.{prefix}"
            yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
        }
    
    seq {
        for kvp in tree.Levels do
            yield! traverse $"L{kvp.Key}" "1" kvp.Value
        for kvp in tree.Nests do
            yield! traverse $"N{kvp.Key}" "1" kvp.Value
    } |> Map.ofSeq

let rec getIds (m: string) (prefix: string) (node: Hywe.Node.TreeNode) =
    seq {
        yield $"{m}.{prefix}"
        yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getIds m $"{prefix}.{i + 1}" child)
    }

let getValidIdsForMarkerSeq (tree: Hywe.Node.SubModel) (marker: string) =
    if marker.StartsWith("N") then
        let nestId = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
        match tree.Nests |> Map.tryFind nestId with
        | Some nestNode -> getIds marker "1" nestNode
        | None -> Seq.empty
    else
        let lvl = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0
        match tree.Levels |> Map.tryFind lvl with
        | Some levelNode -> getIds marker "1" levelNode
        | None -> Seq.empty

let getValidIdsForMarker (tree: Hywe.Node.SubModel) (marker: string) =
    getValidIdsForMarkerSeq tree marker |> Set.ofSeq

let getValidIds (tree: Hywe.Node.SubModel) =
    match tree.ActiveNest with
    | Some nestId -> getValidIdsForMarker tree $"N{nestId}"
    | None -> getValidIdsForMarker tree (match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}")

let getIdToNodeMap (tree: Hywe.Node.SubModel) =
    let rec traverse (m: string) (prefix: string) (node: Hywe.Node.TreeNode) =
        seq {
            yield $"{m}.{prefix}", node
            yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
        }
    seq {
        for kvp in tree.Levels do
            yield! traverse $"L{kvp.Key}" "1" kvp.Value
        for kvp in tree.Nests do
            yield! traverse $"N{kvp.Key}" "1" kvp.Value
    } |> Map.ofSeq

let filterBatchConfigForMarker (computeExpensive: bool) (tree: Hywe.Node.SubModel) (marker: string) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
    let validIdsSeq = getValidIdsForMarkerSeq tree marker |> Seq.toArray
    let validIds = validIdsSeq |> Set.ofArray
    if validIds.IsEmpty then config
    else
        let idToNode = getIdToNodeMap tree
        
        let idToIndex = 
            config.cxCxl1 
            |> Array.indexed 
            |> Array.choose (fun (i, (c: Hywe.Core.Coxel.Cxl)) -> 
                let id = Hywe.Core.Coxel.prpVlu c.Rfid
                if validIds.Contains id then Some(id, i) else None)
            |> Map.ofArray
            
        let fallbackSqn = if config.cxCxl1.Length > 0 then config.cxCxl1.[0].Seqn else Hywe.Core.Hexel.VRCCNE
        let fallbackElv = if config.cxCxl1.Length > 0 then let (_, _, z) = Hywe.Core.Hexel.hxlCrd config.cxCxl1.[0].Base in z else 0
        
        let synthCxls = System.Collections.Generic.List<Hywe.Core.Coxel.Cxl>()
        let synthClrs = System.Collections.Generic.List<string>()
        let synthAvls = System.Collections.Generic.List<int>()
        let synthB36s = System.Collections.Generic.List<string>()
        let synthShapes = System.Collections.Generic.List<ModelTypes.BatchComponent>()
        
        for id in validIdsSeq do
            match idToIndex |> Map.tryFind id with
            | Some i ->
                let origCxl = config.cxCxl1.[i]
                let origShp = config.shapes.[i]
                let nodeOpt = idToNode |> Map.tryFind id
                let fixedCxl =
                    match nodeOpt with
                    | Some node when not (System.String.IsNullOrWhiteSpace node.Name) && (System.String.IsNullOrWhiteSpace (Hywe.Core.Coxel.prpVlu origCxl.Name) || Hywe.Core.Coxel.prpVlu origCxl.Name = id) ->
                        { origCxl with Name = Hywe.Core.Coxel.Label node.Name }
                    | _ -> origCxl
                let fixedShp =
                    match nodeOpt with
                    | Some node when not (System.String.IsNullOrWhiteSpace node.Name) && (System.String.IsNullOrWhiteSpace origShp.name || origShp.name = id) ->
                        {| origShp with name = node.Name |}
                    | _ -> origShp
                synthCxls.Add(fixedCxl)
                synthClrs.Add(config.cxClr1.[i])
                synthAvls.Add(config.cxlAvl.[i])
                synthB36s.Add(config.cxB36.[i])
                synthShapes.Add(fixedShp)
            | None ->
                match idToNode |> Map.tryFind id with
                | Some node ->
                    let count = match System.Int32.TryParse node.Weight with true, v -> v | _ -> 0
                    let labelName = if System.String.IsNullOrWhiteSpace node.Name then id else node.Name
                    let fakeCxl = { Hywe.Core.Coxel.Name = Hywe.Core.Coxel.Label labelName
                                    Hywe.Core.Coxel.Rfid = Hywe.Core.Coxel.Refid id
                                    Hywe.Core.Coxel.Size = Hywe.Core.Coxel.Count count
                                    Hywe.Core.Coxel.Seqn = fallbackSqn
                                    Hywe.Core.Coxel.Base = Hywe.Core.Hexel.identity fallbackElv
                                    Hywe.Core.Coxel.Hxls = [||] }
                    synthCxls.Add(fakeCxl)
                    synthClrs.Add("#eee")
                    synthAvls.Add(0)
                    synthB36s.Add("")
                    synthShapes.Add({| name = labelName; points = [||]; color = "#eee"; lx = 0.0; ly = 0.0 |})
                | None -> ()
        
        let cxls = synthCxls.ToArray()
        let clrs = synthClrs.ToArray()
        let avls = synthAvls.ToArray()
        let b36s = synthB36s.ToArray()
        let shapes = synthShapes.ToArray()
        
        let adj = if computeExpensive then Hywe.Core.Coxel.cxlAdj cxls else config.cxAdj1

        let wtmkShapes = 
            if marker.StartsWith("N") then
                let nestId = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
                match tree.Nests |> Map.tryFind nestId with
                | Some nestNode ->
                    let lvlMarker = match nestNode.Level with | 0 -> "L0" | lvl -> $"L{lvl}"
                    let levelIds = 
                        match tree.Levels |> Map.tryFind nestNode.Level with
                        | Some levelNode -> getIds lvlMarker "1" levelNode |> Set.ofSeq
                        | None -> Set.empty
                    
                    let isParentCxl (rfid: string) =
                        match nestNode.Base with
                        | Some targetId -> rfid = targetId || rfid.EndsWith("." + targetId)
                        | None -> false

                    let bgCxl = config.cxCxl1 |> Array.tryFind (fun c -> isParentCxl (Hywe.Core.Coxel.prpVlu c.Rfid))
                    let bgCxlId = bgCxl |> Option.map (fun c -> Hywe.Core.Coxel.prpVlu c.Rfid)

                    let wtmkIndices = 
                        config.cxCxl1
                        |> Array.indexed
                        |> Array.filter (fun (_, c) -> 
                            let id = Hywe.Core.Coxel.prpVlu c.Rfid
                            levelIds.Contains(id) && (Some id <> bgCxlId))
                        |> Array.map fst

                    Some (wtmkIndices |> Array.map (fun i -> config.shapes.[i]))
                | None -> None
            else
                None

        {| config with 
            cxCxl1 = cxls
            cxClr1 = clrs
            cxlAvl = avls
            shapes = shapes
            wtmkShapes = wtmkShapes
            cxAdj1 = adj
            cxB36 = b36s
            cxSol1 = config.cxSol1 |}

let filterBatchConfig (computeExpensive: bool) (tree: Hywe.Node.SubModel) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
    filterBatchConfigForMarker computeExpensive tree (match tree.ActiveNest with | Some nestId -> $"N{nestId}" | None -> match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}") config
