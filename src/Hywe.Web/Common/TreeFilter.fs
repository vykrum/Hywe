module TreeFilter

open System
open Hywe.Core
open Hywe.Core.Coxel
open Hywe.Core.Hexel
open ModelTypes
open TreeTypes

let getHierarchicalIdMap (tree: SubModel) =
    let rec traverse (m: string) (prefix: string) (node: TreeNode) =
        seq {
            yield node.Id, $"{m}.{prefix}"
            yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
        }
    
    let levels = tree.Levels |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"L{k}" "1" v)
    let nests  = tree.Nests  |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"N{k}" "1" v)

    Seq.append levels nests |> Map.ofSeq

let rec getIds (m: string) (prefix: string) (node: TreeNode) =
    seq {
        yield $"{m}.{prefix}"
        yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getIds m $"{prefix}.{i + 1}" child)
    }

let getValidIdsForMarkerSeq (tree: SubModel) (marker: string) =
    match marker.StartsWith("N") with
    | true ->
        let nestId = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
        match tree.Nests |> Map.tryFind nestId with
        | Some nestNode -> getIds marker "1" nestNode
        | None -> Seq.empty
    | false ->
        let lvl = match System.Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0
        match tree.Levels |> Map.tryFind lvl with
        | Some levelNode -> getIds marker "1" levelNode
        | None -> Seq.empty

let getValidIdsForMarker (tree: SubModel) (marker: string) =
    getValidIdsForMarkerSeq tree marker |> Set.ofSeq

let getValidIds (tree: SubModel) =
    match tree.ActiveNest with
    | Some nestId -> getValidIdsForMarker tree $"N{nestId}"
    | None -> getValidIdsForMarker tree (match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}")

let getIdToNodeMap (tree: SubModel) =
    let rec traverse (m: string) (prefix: string) (node: TreeNode) =
        seq {
            yield $"{m}.{prefix}", node
            yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
        }
    let levels = tree.Levels |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"L{k}" "1" v)
    let nests  = tree.Nests  |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"N{k}" "1" v)

    Seq.append levels nests |> Map.ofSeq

let filterBatchConfigForMarker (computeExpensive: bool) (tree: SubModel) (marker: string) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
    let validIdsSeq = getValidIdsForMarkerSeq tree marker |> Seq.toArray
    let validIds = validIdsSeq |> Set.ofArray
    match validIds.IsEmpty with
    | true -> config
    | false ->
        let idToNode = getIdToNodeMap tree
        
        let idToIndex = 
            config.cxCxl1 
            |> Array.indexed 
            |> Array.choose (fun (i, (c: Hywe.Core.Coxel.Cxl)) -> 
                let id = Hywe.Core.Coxel.prpVlu c.Rfid
                match validIds.Contains id with
                | true -> Some(id, i)
                | false -> None)
            |> Map.ofArray
            
        let fallbackSqn = 
            match config.cxCxl1 with
            | [||] -> Hywe.Core.Hexel.VRCCNE
            | arr -> arr.[0].Seqn

        let fallbackElv = 
            match config.cxCxl1 with
            | [||] -> 0
            | arr -> 
                let (_, _, z) = Hywe.Core.Hexel.hxlCrd arr.[0].Base 
                z
        
        let items =
            validIdsSeq
            |> Array.choose (fun id ->
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
                    Some (fixedCxl, config.cxClr1.[i], config.cxlAvl.[i], config.cxB36.[i], fixedShp)
                | None ->
                    match idToNode |> Map.tryFind id with
                    | Some node ->
                        let count = match System.Int32.TryParse node.Weight with true, v -> v | _ -> 0
                        let labelName = 
                            match System.String.IsNullOrWhiteSpace node.Name with
                            | true -> id
                            | false -> node.Name
                        let fakeCxl = { Hywe.Core.Coxel.Name = Hywe.Core.Coxel.Label labelName
                                        Hywe.Core.Coxel.Rfid = Hywe.Core.Coxel.Refid id
                                        Hywe.Core.Coxel.Size = Hywe.Core.Coxel.Count count
                                        Hywe.Core.Coxel.Seqn = fallbackSqn
                                        Hywe.Core.Coxel.Base = Hywe.Core.Hexel.identity fallbackElv
                                        Hywe.Core.Coxel.Hxls = [||] }
                        Some (fakeCxl, "#eee", 0, "", {| name = labelName; points = [||]; color = "#eee"; lx = 0.0; ly = 0.0 |})
                    | None -> None)

        let cxls = items |> Array.map (fun (c, _, _, _, _) -> c)
        let clrs = items |> Array.map (fun (_, cl, _, _, _) -> cl)
        let avls = items |> Array.map (fun (_, _, a, _, _) -> a)
        let b36s = items |> Array.map (fun (_, _, _, b, _) -> b)
        let shapes = items |> Array.map (fun (_, _, _, _, s) -> s)
        
        let adj = 
            match computeExpensive with
            | true -> Hywe.Core.Coxel.cxlAdj cxls
            | false -> config.cxAdj1

        let wtmkShapes = 
            match marker.StartsWith("N") with
            | true ->
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
            | false ->
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

let filterBatchConfig (computeExpensive: bool) (tree: SubModel) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
    filterBatchConfigForMarker computeExpensive tree (match tree.ActiveNest with | Some nestId -> $"N{nestId}" | None -> match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}") config
