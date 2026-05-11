namespace Hywe.Core

/// <summary> 
/// Xyxel (XY Plane Layout Engine) 
/// Orchestrates collections of Coxels to resolve spatial distribution and boundary logic. 
/// Handles the horizontal "weaving" as the third stage in the hierarchy: Hexel-Coxel-Xyxel-Zaxel.
/// </summary>

module Xyxel =
    open System
    open Microsoft.FSharp.Reflection
    open Hexel
    open Coxel
    open Goxel
    open Lexel

    /// <summary> Active pattern for safe integer parsing. </summary>
    let (|Int|_|) (s: string) =
        match Int32.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Active pattern for safe float parsing. </summary>
    let (|Float|_|) (s: string) =
        match Double.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Parses a string into a discriminated union case safely. </summary>
    let tryParseUnion<'T> (s: string) : 'T option =
        match FSharpType.IsUnion typeof<'T> with
        | false -> None
        | true ->
            FSharpType.GetUnionCases typeof<'T>
            |> Array.tryFind (fun c -> c.Name.Equals(s, StringComparison.OrdinalIgnoreCase))
            |> Option.map (fun c -> FSharpValue.MakeUnion(c,[||]) :?> 'T)
    
    /// <summary> Represents the raw architectural tree with pre-calculated metrics to avoid redundant processing. </summary>
    type LayoutTree = {
        Raw: (string * int * string)[][]
        TotalArea: int
    }
    with 
        static member Create (raw: (string * int * string)[][]) =
            { Raw = raw; TotalArea = raw |> Array.concat |> Array.distinctBy (fun (id, _, _) -> id) |> Array.sumBy (fun (_, a, _) -> a) }

    /// <summary> Contextual parameters and overrides for layout generation. </summary>
    type LayoutOptions = {
        EntryFallback: string
        InitialOcc: Hxl[]
        Seq: Sqn option
        Width: int option
        Height: int option
        OuterStr: string option
        IslandsStr: string option
        ParentCxl: Cxl option
        Ratio: float option
        Elevation: int option
    }

    /// <summary> Fully resolved layout context, ready for spatial execution. </summary>
    type LayoutContext = {
        Seq: Sqn
        Elevation: int
        Boundary: (int*int)[]
        Islands: (int*int)[][]
        EntryAtr: string
        EntryFallback: Hxl
        InitialOcc: Hxl[]
        ScaledTree: (Prp * Prp * Prp)[][]
        Ratio: float
        Parent: Cxl option
    }

    /// <summary> Scales node areas to fit boundary. Purely functional transformation. </summary>
    let applyReproportioning (boundaryArea: int64) (tree: LayoutTree) (ratioOverride: float option) =
        let isUnbound = boundaryArea <= 0L
        
        let totalRequestedArea = float tree.TotalArea

        let ratio = 
            match ratioOverride with
            | Some r -> r
            | None ->
                match isUnbound || totalRequestedArea <= 0.0 with
                | true -> 0.25
                | false -> (float boundaryArea / (totalRequestedArea * 4.0))
        
        let tree = 
            tree.Raw |> Array.map (fun row ->
                row |> Array.map (fun (id, area, lb) ->
                    let finalCount = 
                        match area > 0 with
                        | true -> max 1 (int (System.Math.Round(float area * ratio)))
                        | false -> 0
                    (Refid id, Count finalCount, Label lb)
                )
            )
        tree, ratio

    /// <summary> Resolves all attributes and performs scaling to prepare for layout. </summary>
    let prepareLayoutContext (attrs: Map<string, string>) (tree: LayoutTree) (opts: LayoutOptions) =
        let seq = 
            opts.Seq 
            |> Option.orElse (attrs |> Map.tryFind "Q" |> Option.bind tryParseUnion<Sqn>)
            |> Option.defaultValue VRCWEE
    
        let elv = opts.Elevation |> Option.defaultValue (attrs |> Map.tryFind "L" |> Option.bind (function Float v -> Some (int v) | _ -> None) |> Option.defaultValue 0)
        let bdWd = opts.Width |> Option.orElse (attrs |> Map.tryFind "W" |> Option.bind (function Int v -> Some v | _ -> None) |> Option.filter (fun v -> v > 0)) |> Option.defaultValue 0
        let bdHt = opts.Height |> Option.orElse (attrs |> Map.tryFind "H" |> Option.bind (function Int v -> Some v | _ -> None)) |> Option.defaultValue bdWd
        
        let bdOu =
            match attrs |> Map.tryFind "X" with
            | Some "1" -> [||]
            | _ ->
                match opts.OuterStr with
                | Some ou when ou <> "" -> parseCoords ou
                | _ ->
                    match attrs |> Map.tryFind "O" with 
                    | Some a when a <> "" -> parseCoords a
                    | _ ->
                        match opts.ParentCxl with
                        | Some parent when bdWd > 0 -> cxlPrm parent elv |> Goxel.cleanPolygon parent.Seqn
                        | _ -> 
                            match bdWd > 0 with
                            | true -> parseCoords $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                            | false -> [||]
    
        let bdIs =
            match opts.IslandsStr with
            | Some il when il <> "" -> parseIslands il
            | _ -> 
                match attrs |> Map.tryFind "I" with 
                | Some a -> parseIslands a 
                | None -> [||]
    
        let entryAtrRaw = attrs |> Map.tryFind "E" |> Option.defaultValue "0"
        let entryAtr = 
            if entryAtrRaw = "0" then "0"
            else
                entryAtrRaw.Split ','
                |> Array.map (fun s -> match System.Double.TryParse s with true, v -> string (int (Math.Round(v))) | _ -> s)
                |> String.concat ","
    
        let entryFallback =
            match opts.EntryFallback.Split ',' with
            | [| Int x; Int y |] -> AV(x, y, elv)
            | _ -> 
                match bdWd with
                | 0 -> identity elv
                | _ -> AV(bdWd/2+2, bdHt/2+2, elv)

        let ntArea = polygonWithHolesArea bdOu bdIs
        let scaledTree, finalRatio = applyReproportioning ntArea tree opts.Ratio

        {
            Seq = seq
            Elevation = elv
            Boundary = bdOu
            Islands = bdIs
            EntryAtr = entryAtr
            EntryFallback = entryFallback
            InitialOcc = opts.InitialOcc |> Array.map (fun h -> let (x, y, _) = hxlCrd h in AV(x, y, elv))
            ScaledTree = scaledTree
            Ratio = finalRatio
            Parent = opts.ParentCxl
        }

    /// <summary> Extracts the first node as the base coxel for layout. </summary>
    let generateBaseCxl (ctx: LayoutContext) =
        let bndSet = 
            let ntArea = Goxel.polygonWithHolesArea ctx.Boundary ctx.Islands
            if ntArea > 0L then 
                let allVtx = Array.append ctx.Boundary (ctx.Islands |> Array.collect id)
                let minX = allVtx |> Array.minBy fst |> fst |> (fun x -> x - 2)
                let maxX = allVtx |> Array.maxBy fst |> fst |> (fun x -> x + 2)
                let minY = allVtx |> Array.minBy snd |> snd |> (fun y -> y - 2)
                let maxY = allVtx |> Array.maxBy snd |> snd |> (fun y -> y + 2)
    
                let blocked = ResizeArray<Hxl>()
                for x in minX .. maxX do
                    for y in minY .. maxY do
                        let pt = (x, y)
                        let isOutside = not (Goxel.pointInPolygon pt ctx.Boundary)
                        let isInsideIsland = ctx.Islands |> Array.exists (Goxel.pointInPolygon pt)
                        if isOutside || isInsideIsland then
                            blocked.Add(RV(x, y, ctx.Elevation))
                blocked.ToArray()
            else [||]
    
        let occ = Array.append ctx.InitialOcc bndSet |> hxlUni 1
        let flatEntries = ctx.ScaledTree |> Array.concat
    
        match Array.tryHead flatEntries with
        | None -> None
        | Some (id, ct, lb) ->
            let rootCxl, nextOcc = createBaseCoxel ctx.Seq ctx.Elevation ctx.EntryAtr ctx.EntryFallback id ct lb occ ctx.Parent
            Some (rootCxl, nextOcc)

    /// <summary> Core layout construction for a single level, starting from a pre-generated base coxel. </summary>
    let generateCxlLayout (ctx: LayoutContext) (rootCxl: Cxl) (nextOcc: Hxl[]) : Cxl[] * (int*int)[][] * float =
        let seq = rootCxl.Seqn
        let _, _, elv = hxlCrd rootCxl.Base

        let rec buildTree (tre : (Prp*Prp*Prp)[][]) acc occ =
            match tre with 
            | [||] -> acc
            | _ ->
                let group = Array.head tre
                let parentId = group |> Array.head |> fun (i,_,_) -> i
                match acc |> Array.tryFindIndex (fun x -> x.Rfid = parentId) with
                | Some idx ->
                    let host = acc.[idx]
                    let updatedHost, newCxls, newOcc = coxelChildren seq elv host group occ
                    let nextAcc = Array.append (acc |> Array.mapi (fun i cx -> match i = idx with true -> updatedHost | false -> cx)) newCxls
                    buildTree (Array.tail tre) nextAcc newOcc
                | None -> buildTree (Array.tail tre) acc occ

        let result =
            let flatCount = ctx.ScaledTree |> Array.concat |> Array.length
            match flatCount < 2 with
            | true -> [| rootCxl |]
            | false -> buildTree ctx.ScaledTree [| rootCxl |] nextOcc

        result, Array.append [|ctx.Boundary|] ctx.Islands, ctx.Ratio

