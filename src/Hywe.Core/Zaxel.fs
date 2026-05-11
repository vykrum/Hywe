namespace Hywe.Core

/// <summary> 
/// Zaxel (Z-Axis Layout Engine) 
/// Orchestrates collections of Xyxels to manage multi-level spatial logic and elevation-based construction. 
/// Completes the vertical assembly as the volumetric massing stage in the hierarchy: Hexel-Coxel-Xyxel-Zaxel.
/// </summary>
module Zaxel =
    open System
    open Hexel
    open Coxel
    open Goxel
    open Lexel
    open Xyxel

    /// <summary> Internal state for multi-level construction. </summary>
    type BuildState = {
        Cxls: Cxl[]
        Bounds: (int*int)[][][]
        Elvs: float[]
        Ratios: float[]
        Ratio: float option
        RootW: int option
        RootH: int option
        RootO: string option
        RootI: string option
    }

    /// <summary> Recursively processes all levels using a purely functional state-carrying approach. </summary>
    let generateMultiLevelLayout (fullStr: string) (entryAtrFallback: string) (initialOcc: Hxl[]) (seqOverride: (int * Sqn) option) (ouStrOverride: string option) (ilStrOverride: string option) =
        let parsedLevels = processFullString fullStr
        
        let initialState = {
            Cxls = [||]
            Bounds = [||]
            Elvs = [||]
            Ratios = [||]
            Ratio = None
            RootW = None
            RootH = None
            RootO = ouStrOverride
            RootI = ilStrOverride
        }

        let finalState = 
            (initialState, Array.indexed (List.toArray parsedLevels)) 
            ||> Array.fold (fun state (i, segment) ->
                let attrs = segment.Attributes
                let tree = segment.Tree
                
                // Convert anonymous record attributes to Map for Xyxel compatibility
                let attrMap = 
                    Map.ofList [
                        "Q", attrs.Sequence
                        "L", string attrs.Level
                        "X", string attrs.Scale
                        "E", attrs.Entry
                        "O", attrs.OuterBoundary
                        "I", attrs.Islands
                        "T", string attrs.Thickness
                    ]
                    |> (fun m -> match attrs.Width with Some w -> m |> Map.add "W" (string w) | _ -> m)
                    |> (fun m -> match attrs.Height with Some h -> m |> Map.add "H" (string h) | _ -> m)

                let curW = attrs.Width |> Option.map int |> Option.filter (fun v -> v > 0) |> Option.orElse state.RootW
                let curH = attrs.Height |> Option.map int |> Option.orElse state.RootH
                let curO = ouStrOverride |> Option.orElse (if attrs.OuterBoundary <> "" then Some attrs.OuterBoundary else None) |> Option.orElse state.RootO
                let curI = ilStrOverride |> Option.orElse (if attrs.Islands <> "" then Some attrs.Islands else None) |> Option.orElse state.RootI
                let curElv = float attrs.Level

                let eStr = attrs.Entry
                let bsHx = 
                    match i with
                    | 0 -> None
                    | _ -> 
                        let prevMarker = (List.toArray parsedLevels).[i-1].Marker
                        let targetId = if String.IsNullOrWhiteSpace prevMarker then eStr else sprintf "%s-%s" prevMarker eStr
                        state.Cxls 
                        |> Array.filter (fun c -> let (_, _, z) = hxlCrd c.Base in z = i - 1) 
                        |> Array.tryFind (fun c -> prpVlu c.Rfid = targetId)

                let curSeqOverride = 
                    match seqOverride with
                    | Some (targetLvl, s) when targetLvl = i -> Some s
                    | _ -> None

                let rawTree = 
                    tree 
                    |> List.map (fun g -> g |> List.map (fun n -> (n.Id, n.Area, n.Label)) |> List.toArray)
                    |> List.toArray

                let treeObj = LayoutTree.Create rawTree
                let opts = {
                    EntryFallback = entryAtrFallback
                    InitialOcc = initialOcc
                    Seq = curSeqOverride
                    Width = curW
                    Height = curH
                    OuterStr = curO
                    IslandsStr = curI
                    ParentCxl = bsHx
                    Ratio = state.Ratio
                    Elevation = Some i
                }

                let ctx = prepareLayoutContext attrMap treeObj opts
                let baseRes = generateBaseCxl ctx
                
                let cxls, bounds, ratio = 
                    match baseRes with
                    | Some (root, occ) -> generateCxlLayout ctx root occ
                    | None -> [||], [||], 0.0

                match i with
                | 0 ->
                    { state with 
                        Cxls = Array.append state.Cxls cxls
                        Bounds = Array.append state.Bounds [| bounds |]
                        Elvs = Array.append state.Elvs [| curElv |]
                        Ratios = Array.append state.Ratios [| ratio |]
                        Ratio = Some ratio
                        RootW = curW
                        RootH = curH
                        RootO = curO
                        RootI = curI }
                | _ ->
                    { state with 
                        Cxls = Array.append state.Cxls cxls
                        Bounds = Array.append state.Bounds [| bounds |]
                        Ratios = Array.append state.Ratios [| ratio |]
                        Elvs = Array.append state.Elvs [| curElv |] }
            )

        let finalElvs = 
            match List.tryLast parsedLevels with
            | Some lastSegment ->
                let attrs = lastSegment.Attributes
                Array.append finalState.Elvs [| Array.last finalState.Elvs + attrs.Thickness |]
            | None -> finalState.Elvs

        finalState.Cxls, finalState.Bounds, finalElvs, finalState.Ratios
