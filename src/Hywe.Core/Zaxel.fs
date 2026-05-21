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
    open Nexel

    /// <summary> Internal state for multi-level construction. </summary>
    type BuildState = {
        Cxls: Cxl[]
        Bounds: (int*int)[][][]
        Elvs: float[]
        Ratio: float option
        RootW: int option
        RootH: int option
        RootO: string option
        RootI: string option
    }

    /// <summary> Recursively processes all levels using a purely functional state-carrying approach. </summary>
    let generateMultiLevelLayout (fullStr: string) (entryAtrFallback: string) (initialOcc: Hxl[]) (seqOverride: (int * Sqn) option) (ouStrOverride: string option) (ilStrOverride: string option) =
        let parsedLevels = processFullString fullStr
        
        // --- Pass 1: Resolve Attributes & Global Ratio ---
        let resolvedLevels, globalRatio =
            let initial = (ouStrOverride, ilStrOverride, None, None, None, 0) // O, I, W, H, Ratio, LvlIdx
            ((initial, []), parsedLevels)
            ||> List.fold (fun ((ou, il, w, h, r, lvlIdx), acc) segment ->
                match segment with
                | Level a_block ->
                    let a = a_block.Attributes
                    let curW = a.Width |> Option.map int |> Option.filter (fun v -> v > 0) |> Option.orElse w
                    let curH = a.Height |> Option.map int |> Option.orElse h
                    let curO = ouStrOverride |> Option.orElse (match a.OuterBoundary <> "" with true -> Some a.OuterBoundary | false -> None) |> Option.orElse ou
                    let curI = ilStrOverride |> Option.orElse (match a.Islands <> "" with true -> Some a.Islands | false -> None) |> Option.orElse il
                    
                    let rawTree = a_block.Tree |> List.map (fun g -> g |> List.map (fun n -> (n.Id, n.Area, n.Label)) |> List.toArray) |> List.toArray
                    let treeObj = LayoutTree.Create rawTree
                    let attrMap = Map.ofList [ "Q", a.Sequence; "L", string a.Level; "X", string a.Scale; "O", a.OuterBoundary; "I", a.Islands ]
                    
                    let ratio = calculateTargetRatio attrMap treeObj { 
                        EntryFallback = entryAtrFallback; InitialOcc = initialOcc; Seq = None
                        Width = curW; Height = curH; OuterStr = curO; IslandsStr = curI
                        ParentCxl = None; Ratio = None; Elevation = Some lvlIdx 
                    }
                    
                    let nextR = match r with None -> Some ratio | Some current -> Some (min current ratio)
                    (curO, curI, curW, curH, nextR, lvlIdx + 1), (segment, Some treeObj, curW, curH, curO, curI, lvlIdx) :: acc
                | Nest n_block ->
                    (ou, il, w, h, r, lvlIdx), (segment, None, None, None, None, None, lvlIdx - 1) :: acc
            )
            |> fun ((_, _, _, _, r, _), levels) -> List.rev levels, r

        // --- Pass 2: Generation ---
        let initialState = { Cxls = [||]; Bounds = [||]; Elvs = [||]; Ratio = globalRatio; RootW = None; RootH = None; RootO = None; RootI = None }
        
        let finalState = 
            (initialState, resolvedLevels) 
            ||> List.fold (fun state (segment, treeObjOpt, w, h, o, iStr, lvlIdx) ->
                match segment with
                | Level a_block ->
                    let attrs = a_block.Attributes
                    let treeObj = treeObjOpt.Value
                    let attrMap = Map.ofList [ "Q", attrs.Sequence; "L", string attrs.Level; "X", string attrs.Scale; "E", attrs.Entry; "O", attrs.OuterBoundary; "I", attrs.Islands; "T", string attrs.Thickness ]
                    
                    let bsHx = 
                        match lvlIdx with
                        | 0 -> None
                        | _ -> 
                            let targetId = attrs.Entry
                            state.Cxls |> Array.filter (fun c -> let (_, _, z) = hxlCrd c.Base in z = lvlIdx - 1) |> Array.tryFind (fun c -> prpVlu c.Rfid = targetId)

                    let ctx = prepareLayoutContext attrMap treeObj { 
                        EntryFallback = entryAtrFallback; InitialOcc = initialOcc; Ratio = state.Ratio
                        Seq = match seqOverride with Some (l, s) when l = lvlIdx -> Some s | _ -> None
                        Width = w; Height = h; OuterStr = o; IslandsStr = iStr; ParentCxl = bsHx; Elevation = Some lvlIdx 
                    }
                    
                    match generateBaseCxl ctx with
                    | Some (root, occ) ->
                        let cxls, bounds, _ = generateCxlLayout ctx root occ
                        { state with Cxls = Array.append state.Cxls cxls; Bounds = Array.append state.Bounds [| bounds |]; Elvs = Array.append state.Elvs [| float attrs.Level |] }
                    | None -> { state with Elvs = Array.append state.Elvs [| float attrs.Level |] }
                | Nest n_block ->
                    let targetId = n_block.Attributes.Base
                    match state.Cxls |> Array.tryFind (fun c -> prpVlu c.Rfid = targetId) with
                    | Some hostCxl ->
                        let hostThickness = 
                            resolvedLevels 
                            |> List.tryPick (function 
                                | Level l, _, _, _, _, _, lIdx when lIdx = lvlIdx -> Some l.Attributes.Thickness
                                | _ -> None)
                            |> Option.defaultValue 3.0
                        match Nexel.generateNestLayout n_block hostCxl hostThickness state.Cxls None with
                        | Some (cxls, bounds, _) ->
                            { state with Cxls = Array.append state.Cxls cxls; Bounds = Array.append state.Bounds [| bounds |] }
                        | None -> state
                    | None -> state
            )

        let finalElvs = 
            match List.tryLast parsedLevels with
            | Some lastSegment -> 
                let t = match lastSegment with Level l -> l.Attributes.Thickness | Nest n -> n.Attributes.Thickness
                Array.append finalState.Elvs [| Array.last finalState.Elvs + t |]
            | None -> finalState.Elvs

        finalState.Cxls, finalState.Bounds, finalElvs, Array.replicate parsedLevels.Length (finalState.Ratio |> Option.defaultValue 0.0)
