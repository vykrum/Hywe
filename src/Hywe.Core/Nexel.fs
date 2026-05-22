namespace Hywe.Core

/// <summary> 
/// Nexel (Nested Engine) 
/// Handles logic for Nested Coxels (N# markers).
/// </summary>
module Nexel =
    open System
    open Lexel

    type NestData = {
        Marker: string
        BaseNodeId: string
        Attributes: LexelAttributes
        Nodes: LexelNode list
    }

    /// <summary> 
    /// Checks if a marker is a Nest marker (e.g., N1, N2).
    /// </summary>
    let isNestMarker (marker: string) =
        not (String.IsNullOrEmpty(marker)) && marker.StartsWith("N")

    /// <summary> 
    /// Extracts the nest index from a marker.
    /// </summary>
    let getNestIndex (marker: string) =
        Some marker
        |> Option.filter isNestMarker
        |> Option.bind (fun m -> 
            match Int32.TryParse(m.Substring(1)) with
            | true, v -> Some v
            | _ -> None)

    /// <summary> Generates nested layout using the Xyxel layout engine. </summary>
    let generateNestLayout (nest: NestBlock) (hostCxl: Coxel.Cxl) (hostThickness: float) (allCxls: Coxel.Cxl[]) (sqnStrOverride: string option) =
        let attrs = nest.Attributes
        let elv = 
            let _, _, z = Hexel.hxlCrd hostCxl.Base
            z
        
        let hostSeqn = hostCxl.Seqn
        // Format parent's perimeter for Xyxel's OuterStr by getting the outer offset
        let outerHxls = Hexel.hxlOfs hostSeqn elv hostCxl.Hxls
        let expandedHxls = Array.append hostCxl.Hxls outerHxls |> Hexel.hxlUni 1
        let expandedCxl = { hostCxl with Hxls = expandedHxls }
        
        let pts = 
            Coxel.cxlPrm expandedCxl elv
            |> Goxel.cleanPolygon hostSeqn

        let hostPerimeter = 
            pts
            |> Array.map (fun (x, y) -> sprintf "%d,%d" x y)
            |> String.concat ","
            
        let minX, minY, maxX, maxY = Goxel.bounds pts
        let w = maxX - minX
        let h = maxY - minY
            
        let sqnStr = sqnStrOverride |> Option.defaultValue attrs.Sequence
        let attrMap = Map.ofList [ "Q", sqnStr; "L", string elv; "X", "0"; "W", string w; "H", string h; "O", hostPerimeter; "I", ""; "T", string hostThickness ]

        let rawTree = nest.Tree |> List.map (fun g -> g |> List.map (fun n -> (n.Id, n.Area, n.Label)) |> List.toArray) |> List.toArray
        let treeObj = Xyxel.LayoutTree.Create rawTree

        let ratio = 
            match treeObj.TotalArea > 0 with
            | true -> (float hostCxl.Hxls.Length) / (float treeObj.TotalArea)
            | false -> 0.25

        let coreHexel = 
            hostCxl.Hxls 
            |> Array.tryLast 
            |> Option.defaultValue hostCxl.Base
                    
        let hxX, hxY, _ = Hexel.hxlCrd coreHexel
        let boundaryWall = outerHxls |> Array.map (fun h -> let x, y, z = Hexel.hxlCrd h in Hexel.RV(x, y, z))
        
        let opts: Xyxel.LayoutOptions = { 
            EntryFallback = sprintf "%d,%d" hxX hxY
            InitialOcc = boundaryWall
            Seq = None
            Width = Some w
            Height = Some h
            OuterStr = Some hostPerimeter
            IslandsStr = None
            ParentCxl = None
            Ratio = Some ratio
            Elevation = Some elv
        }

        let ctx = Xyxel.prepareLayoutContext attrMap treeObj opts
        
        match Xyxel.generateBaseCxl ctx with
        | Some (root, occ) ->
            let cxls, bounds, rto = Xyxel.generateCxlLayout ctx root occ
            
            Some (cxls, bounds, rto)
        | None -> None
