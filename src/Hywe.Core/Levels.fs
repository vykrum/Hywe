namespace Hywe.Core

module Levels =
    open System
    open Hexel
    open Coxel
    open Geometry
    open Parse
    open Layout

    /// <summary> Internal state for multi-level construction. </summary>
    type BuildState = {
        Cxls: Cxl[]
        Bounds: (int*int)[][]
        Elvs: float[]
        Ratios: float[]
        Ratio: float option
        RootW: int option
        RootH: int option
        RootO: string option
        RootI: string option
    }

    /// <summary> Recursively processes all levels using a purely functional state-carrying approach. </summary>
    let generateMultiLevelLayout (fullStr: string) (entryAtrFallback: string) (initialOcc: Hxl[]) (seqOverride: Sqn option) (ouStrOverride: string option) (ilStrOverride: string option) =
        let levels = splitIntoLevels fullStr
        
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
            (initialState, Array.indexed levels) 
            ||> Array.fold (fun state (i, levelData) ->
                let attrs, tree = processLevel levelData
                
                let curW = attrs |> Map.tryFind "W" |> Option.bind (function Int v -> Some v | _ -> None) |> Option.filter (fun v -> v > 0) |> Option.orElse state.RootW
                let curH = attrs |> Map.tryFind "H" |> Option.bind (function Int v -> Some v | _ -> None) |> Option.orElse state.RootH
                let curO = ouStrOverride |> Option.orElse (attrs |> Map.tryFind "O") |> Option.orElse state.RootO
                let curI = ilStrOverride |> Option.orElse (attrs |> Map.tryFind "I") |> Option.orElse state.RootI
                let curElv = attrs |> Map.tryFind "L" |> Option.bind (function Float v -> Some v | _ -> None) |> Option.defaultValue (float i * 3.0) 

                let eStr = attrs |> Map.tryFind "E" |> Option.defaultValue "0"
                let bsHx = 
                    match i with
                    | 0 -> None
                    | _ -> 
                        let prevMarker = levels.[i-1].Marker
                        let targetId = if String.IsNullOrWhiteSpace prevMarker then eStr else sprintf "%s-%s" prevMarker eStr
                        state.Cxls 
                        |> Array.filter (fun c -> let (_, _, z) = hxlCrd c.Base in z = i - 1) 
                        |> Array.tryFind (fun c -> prpVlu c.Rfid = targetId)

                let cxls, bounds, ratio = generateCxlLayout attrs tree entryAtrFallback seqOverride curW curH curO curI initialOcc bsHx state.Ratio (Some i)

                match i with
                | 0 ->
                    { state with 
                        Cxls = Array.append state.Cxls cxls
                        Bounds = Array.append state.Bounds bounds
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
                        Bounds = Array.append state.Bounds bounds
                        Ratios = Array.append state.Ratios [| ratio |]
                        Elvs = Array.append state.Elvs [| curElv |] }
            )

        let finalElvs = 
            match Array.tryLast levels with
            | Some lastLvl ->
                let attrs, _ = processLevel lastLvl
                match attrs |> Map.tryFind "T" |> Option.bind (function Float v -> Some v | _ -> None) with
                | Some t -> Array.append finalState.Elvs [| Array.last finalState.Elvs + t |]
                | None -> Array.append finalState.Elvs [| Array.last finalState.Elvs + 3.0 |]
            | None -> finalState.Elvs

        finalState.Cxls, finalState.Bounds, finalElvs, finalState.Ratios
