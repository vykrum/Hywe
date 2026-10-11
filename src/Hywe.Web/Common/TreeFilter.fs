/// <summary>
/// Provides hierarchical ID resolution, filtering, and model synchronization utilities
/// for hierarchical tree structures (<see cref="SubModel"/>) and coxel batch configurations (<see cref="BatchConfgrtns"/>).
/// </summary>
module TreeFilter

open System
open Hywe.Core
open Hywe.Core.Coxel
open Hywe.Core.Hexel
open ModelTypes
open TreeTypes

/// <summary>
/// Recursively maps every tree node's unique ID to its hierarchical dot-notation path
/// across all levels (<c>L{k}</c>) and nests (<c>N{k}</c>).
/// </summary>
/// <param name="tree">The <see cref="SubModel"/> containing level and nest trees.</param>
/// <returns>A map of node GUIDs/IDs to dot-notation hierarchical paths (e.g. <c>"L0.1.2"</c>, <c>"N1.1"</c>).</returns>
let getHierarchicalIdMap (tree: SubModel) =
    let rec traverse (m: string) (prefix: string) (node: TreeNode) =
        seq {
            yield node.Id, $"{m}.{prefix}"
            yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
        }
    
    let levels = tree.Levels |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"L{k}" "1" v)
    let nests  = tree.Nests  |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"N{k}" "1" v)

    Seq.append levels nests |> Map.ofSeq

/// <summary>
/// Recursively enumerates hierarchical dot-notation path strings for a given node and all its descendants.
/// </summary>
/// <param name="m">Hierarchy marker prefix (e.g. <c>"L0"</c> or <c>"N1"</c>).</param>
/// <param name="prefix">Current 1-based index path (e.g. <c>"1"</c> or <c>"1.2"</c>).</param>
/// <param name="node">The root <see cref="TreeNode"/> of the branch to traverse.</param>
/// <returns>A sequence of hierarchical path strings representing the node and its children.</returns>
let rec getIds (m: string) (prefix: string) (node: TreeNode) =
    seq {
        yield $"{m}.{prefix}"
        yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> getIds m $"{prefix}.{i + 1}" child)
    }

/// <summary>
/// Retrieves the sequence of all valid hierarchical path IDs corresponding to the specified level or nest marker.
/// </summary>
/// <param name="tree">The <see cref="SubModel"/> containing levels and nests.</param>
/// <param name="marker">The hierarchy marker string (e.g. <c>"L0"</c> for level 0, <c>"N1"</c> for nest 1).</param>
/// <returns>A sequence of valid hierarchical path IDs for the given marker, or an empty sequence if not found.</returns>
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

/// <summary>
/// Retrieves the set of all valid hierarchical path IDs corresponding to the specified level or nest marker.
/// </summary>
/// <param name="tree">The <see cref="SubModel"/> containing levels and nests.</param>
/// <param name="marker">The hierarchy marker string (e.g. <c>"L0"</c> or <c>"N1"</c>).</param>
/// <returns>A <see cref="Set{string}"/> of valid hierarchical IDs for the marker.</returns>
let getValidIdsForMarker (tree: SubModel) (marker: string) =
    getValidIdsForMarkerSeq tree marker |> Set.ofSeq

/// <summary>
/// Retrieves the set of valid hierarchical path IDs for the current active view (active nest or active level).
/// </summary>
/// <param name="tree">The <see cref="SubModel"/> with <see cref="SubModel.ActiveNest"/> and <see cref="SubModel.ActiveLevel"/>.</param>
/// <returns>A set of valid hierarchical path IDs for the active view.</returns>
let getValidIds (tree: SubModel) =
    match tree.ActiveNest with
    | Some nestId -> getValidIdsForMarker tree $"N{nestId}"
    | None -> getValidIdsForMarker tree (match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}")

/// <summary>
/// Builds a lookup map from hierarchical dot-notation ID (e.g. <c>"L0.1"</c>, <c>"N1.1.2"</c>) to the corresponding <see cref="TreeNode"/>.
/// </summary>
/// <param name="tree">The <see cref="SubModel"/> containing level and nest trees.</param>
/// <returns>A map of hierarchical IDs to their respective <see cref="TreeNode"/> instances.</returns>
let getIdToNodeMap (tree: SubModel) =
    let rec traverse (m: string) (prefix: string) (node: TreeNode) =
        seq {
            yield $"{m}.{prefix}", node
            yield! node.Children |> List.indexed |> Seq.collect (fun (i, child) -> traverse m $"{prefix}.{i + 1}" child)
        }
    let levels = tree.Levels |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"L{k}" "1" v)
    let nests  = tree.Nests  |> Map.toSeq |> Seq.collect (fun (k, v) -> traverse $"N{k}" "1" v)

    Seq.append levels nests |> Map.ofSeq

/// <summary>
/// Filters a batch configuration to retain only coxels, colors, and boundary shapes belonging to the specified level or nest marker,
/// synthesizing fallback/placeholder coxels for unplaced nodes and generating watermark shapes for nests.
/// </summary>
/// <param name="computeExpensive">Specifies whether to recompute the coxel adjacency matrix (<c>cxAdj1</c>).</param>
/// <param name="tree">The source hierarchical <see cref="SubModel"/>.</param>
/// <param name="marker">Hierarchy marker string (e.g. <c>"L0"</c> or <c>"N1"</c>).</param>
/// <param name="config">The input <see cref="BatchConfgrtns"/> to filter.</param>
/// <returns>A filtered and synchronized <see cref="BatchConfgrtns"/> record.</returns>
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

/// <summary>
/// Filters a batch configuration for the currently active level or nest in the tree hierarchy.
/// </summary>
/// <param name="computeExpensive">Specifies whether to recompute the coxel adjacency matrix (<c>cxAdj1</c>).</param>
/// <param name="tree">The <see cref="SubModel"/> with current active level or nest selection.</param>
/// <param name="config">The input <see cref="BatchConfgrtns"/> to filter.</param>
/// <returns>A filtered and synchronized <see cref="BatchConfgrtns"/> for the active level or nest.</returns>
let filterBatchConfig (computeExpensive: bool) (tree: SubModel) (config: ModelTypes.BatchConfgrtns) : ModelTypes.BatchConfgrtns =
    filterBatchConfigForMarker computeExpensive tree (match tree.ActiveNest with | Some nestId -> $"N{nestId}" | None -> match tree.ActiveLevel with | 0 -> "L0" | lvl -> $"L{lvl}") config
