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
            let initial = (ouStrOverride, ilStrOverride, None, None, None) // O, I, W, H, Ratio
            ((initial, []), Array.indexed (List.toArray parsedLevels))
            ||> Array.fold (fun ((ou, il, w, h, r), acc) (i, segment) ->
                let a = segment.Attributes
                let curW = a.Width |> Option.map int |> Option.filter (fun v -> v > 0) |> Option.orElse w
                let curH = a.Height |> Option.map int |> Option.orElse h
                let curO = ouStrOverride |> Option.orElse (match a.OuterBoundary <> "" with true -> Some a.OuterBoundary | false -> None) |> Option.orElse ou
                let curI = ilStrOverride |> Option.orElse (match a.Islands <> "" with true -> Some a.Islands | false -> None) |> Option.orElse il
                
                let rawTree = segment.Tree |> List.map (fun g -> g |> List.map (fun n -> (n.Id, n.Area, n.Label)) |> List.toArray) |> List.toArray
                let treeObj = LayoutTree.Create rawTree
                let attrMap = Map.ofList [ "Q", a.Sequence; "L", string a.Level; "X", string a.Scale; "O", a.OuterBoundary; "I", a.Islands ]
                
                let ratio = calculateTargetRatio attrMap treeObj { 
                    EntryFallback = entryAtrFallback; InitialOcc = initialOcc; Seq = None
                    Width = curW; Height = curH; OuterStr = curO; IslandsStr = curI
                    ParentCxl = None; Ratio = None; Elevation = Some i 
                }
                
                let nextR = match r with None -> Some ratio | Some current -> Some (min current ratio)
                (curO, curI, curW, curH, nextR), (segment, treeObj, curW, curH, curO, curI) :: acc
            )
            |> fun ((_, _, _, _, r), levels) -> List.rev levels, r

        // --- Pass 2: Generation ---
        let initialState = { Cxls = [||]; Bounds = [||]; Elvs = [||]; Ratio = globalRatio; RootW = None; RootH = None; RootO = None; RootI = None }
        
        let finalState = 
            (initialState, Array.indexed (List.toArray resolvedLevels)) 
            ||> Array.fold (fun state (i, (segment, treeObj, w, h, o, iStr)) ->
                let attrs = segment.Attributes
                let attrMap = Map.ofList [ "Q", attrs.Sequence; "L", string attrs.Level; "X", string attrs.Scale; "E", attrs.Entry; "O", attrs.OuterBoundary; "I", attrs.Islands; "T", string attrs.Thickness ]
                
                let bsHx = 
                    match i with
                    | 0 -> None
                    | _ -> 
                        let targetId = attrs.Entry
                        state.Cxls |> Array.filter (fun c -> let (_, _, z) = hxlCrd c.Base in z = i - 1) |> Array.tryFind (fun c -> prpVlu c.Rfid = targetId)

                let ctx = prepareLayoutContext attrMap treeObj { 
                    EntryFallback = entryAtrFallback; InitialOcc = initialOcc; Ratio = state.Ratio
                    Seq = match seqOverride with Some (l, s) when l = i -> Some s | _ -> None
                    Width = w; Height = h; OuterStr = o; IslandsStr = iStr; ParentCxl = bsHx; Elevation = Some i 
                }
                
                match generateBaseCxl ctx with
                | Some (root, occ) ->
                    let cxls, bounds, _ = generateCxlLayout ctx root occ
                    { state with Cxls = Array.append state.Cxls cxls; Bounds = Array.append state.Bounds [| bounds |]; Elvs = Array.append state.Elvs [| float attrs.Level |] }
                | None -> { state with Elvs = Array.append state.Elvs [| float attrs.Level |] }
            )

        let finalElvs = 
            match List.tryLast parsedLevels with
            | Some lastSegment -> Array.append finalState.Elvs [| Array.last finalState.Elvs + lastSegment.Attributes.Thickness |]
            | None -> finalState.Elvs

        finalState.Cxls, finalState.Bounds, finalElvs, Array.replicate parsedLevels.Length (finalState.Ratio |> Option.defaultValue 0.0)
