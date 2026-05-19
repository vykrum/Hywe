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
        // Format parent's perimeter for Xyxel's OuterStr
        let hostPerimeter = 
            Coxel.cxlPrm hostCxl elv
            |> Goxel.cleanPolygon hostSeqn
            |> Array.map (fun (x, y) -> sprintf "%d,%d" x y)
            |> String.concat ","
            
        let sqnStr = sqnStrOverride |> Option.defaultValue attrs.Sequence
        let attrMap = Map.ofList [ "Q", sqnStr; "L", string elv; "X", string attrs.Scale; "O", hostPerimeter; "I", ""; "T", string hostThickness ]

        let rawTree = nest.Tree |> List.map (fun g -> g |> List.map (fun n -> (n.Id, n.Area, n.Label)) |> List.toArray) |> List.toArray
        let treeObj = Xyxel.LayoutTree.Create rawTree

        let opts: Xyxel.LayoutOptions = { 
            EntryFallback = "0,0"
            InitialOcc = allCxls |> Array.collect (fun c -> Array.append [|c.Base|] c.Hxls) |> Hexel.hxlUni 1
            Seq = None
            Width = None
            Height = None
            OuterStr = Some hostPerimeter
            IslandsStr = None
            ParentCxl = Some hostCxl
            Ratio = None // Nests calculate their own ratio based on the boundary
            Elevation = Some elv
        }

        let ctx = Xyxel.prepareLayoutContext attrMap treeObj opts
        
        match Xyxel.generateBaseCxl ctx with
        | Some (root, occ) ->
            let cxls, bounds, rto = Xyxel.generateCxlLayout ctx root occ
            Some (cxls, bounds, rto)
        | None -> None
