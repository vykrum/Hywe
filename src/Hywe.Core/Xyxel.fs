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

    /// <summary>
    /// Attempts to parse a string value into an F# discriminated union case value by name (case-insensitive).
    /// </summary>
    /// <typeparam name="'T">The discriminated union type.</typeparam>
    /// <param name="s">String representation of the case name.</param>
    /// <returns>Some case value if matched; otherwise None.</returns>
    /// <example>
    /// <code>
    /// let sqnOpt = Xyxel.tryParseUnion&lt;Hexel.Sqn&gt; "VRCWEE"
    /// </code>
    /// </example>
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

    /// <summary>
    /// Proportionally scales architectural room areas to fit within a bounded boundary polygon, preserving relative sizing.
    /// </summary>
    /// <param name="boundaryArea">Total available area of the boundary polygon in grid units.</param>
    /// <param name="tree">Raw architectural room tree.</param>
    /// <param name="ratioOverride">Optional scaling ratio override.</param>
    /// <returns>Tuple of the scaled room tree (Refid, Count, Label)[][] and the final applied ratio.</returns>
    /// <example>
    /// <code>
    /// let raw = [| [| ("1", 40, "Living"); ("1.1", 20, "Kitchen") |] |]
    /// let tree = Xyxel.LayoutTree.Create raw
    /// let scaledTree, ratio = Xyxel.applyReproportioning 400L tree None
    /// </code>
    /// </example>
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
                        | true -> max 1 (int (Math.Round(float area * ratio)))
                        | false -> 0
                    (Refid id, Count finalCount, Label lb)
                )
            )
        tree, ratio

    /// <summary>
    /// Resolves all attributes, parses boundary geometries, and calculates scaling ratios into a LayoutContext ready for layout execution.
    /// </summary>
    /// <param name="attrs">Map of attribute key-value pairs (e.g. "Q", "L", "W", "H", "O", "I").</param>
    /// <param name="tree">Architectural layout tree.</param>
    /// <param name="opts">Contextual options and overrides.</param>
    /// <returns>A fully resolved LayoutContext ready for generation.</returns>
    /// <example>
    /// <code>
    /// let attrs = Map.ofList [ ("Q", "VRCWEE"); ("W", "30"); ("H", "30") ]
    /// let raw = [| [| ("1", 30, "Living"); ("1.1", 15, "Kitchen") |] |]
    /// let tree = Xyxel.LayoutTree.Create raw
    /// let opts = { EntryFallback = "0,0"; InitialOcc = [||]; Seq = None; Width = Some 30; Height = Some 30; OuterStr = None; IslandsStr = None; ParentCxl = None; Ratio = None; Elevation = Some 0 }
    /// let ctx = Xyxel.prepareLayoutContext attrs tree opts
    /// </code>
    /// </example>
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
            match entryAtrRaw with
            | "0" -> "0"
            | _ ->
                entryAtrRaw.Split ','
                |> Array.map (fun s -> match Double.TryParse s with true, v -> string (int (Math.Round(v))) | _ -> s)
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

    /// <summary>
    /// Calculates the ideal room area scaling ratio for a given layout configuration without preparing the full LayoutContext.
    /// </summary>
    /// <param name="attrs">Map of attribute key-value pairs.</param>
    /// <param name="tree">Architectural layout tree.</param>
    /// <param name="opts">Contextual options and overrides.</param>
    /// <returns>Computed area scaling ratio float.</returns>
    /// <example>
    /// <code>
    /// let attrs = Map.ofList [ ("W", "20"); ("H", "20") ]
    /// let tree = Xyxel.LayoutTree.Create [| [| ("1", 20, "Room") |] |]
    /// let opts = { EntryFallback = "0,0"; InitialOcc = [||]; Seq = None; Width = Some 20; Height = Some 20; OuterStr = None; IslandsStr = None; ParentCxl = None; Ratio = None; Elevation = Some 0 }
    /// let ratio = Xyxel.calculateTargetRatio attrs tree opts
    /// </code>
    /// </example>
    let calculateTargetRatio (attrs: Map<string, string>) (tree: LayoutTree) (opts: LayoutOptions) =
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
                        | Some parent when bdWd > 0 -> cxlPrm parent (opts.Elevation |> Option.defaultValue 0) |> Goxel.cleanPolygon parent.Seqn
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
        
        let ntArea = polygonWithHolesArea bdOu bdIs
        let _, ratio = applyReproportioning ntArea tree opts.Ratio
        ratio

    /// <summary>
    /// Extracts the root node from the LayoutContext, establishes boundary obstacle hexels, and generates the initial base Coxel.
    /// </summary>
    /// <param name="ctx">Resolved layout context.</param>
    /// <returns>Option containing a tuple of the root Coxel and the updated occupied hexels.</returns>
    /// <example>
    /// <code>
    /// match Xyxel.generateBaseCxl ctx with
    /// | Some (rootCxl, occ) -> ()
    /// | None -> ()
    /// </code>
    /// </example>
    let generateBaseCxl (ctx: LayoutContext) =
        let bndSet = 
            match Goxel.polygonWithHolesArea ctx.Boundary ctx.Islands > 0L with
            | true -> 
                let allVtx = Array.append ctx.Boundary (ctx.Islands |> Array.collect id)
                let minX = allVtx |> Array.minBy fst |> fst |> (fun x -> x - 30)
                let maxX = allVtx |> Array.maxBy fst |> fst |> (fun x -> x + 30)
                let minY = allVtx |> Array.minBy snd |> snd |> (fun y -> y - 30)
                let maxY = allVtx |> Array.maxBy snd |> snd |> (fun y -> y + 30)
    
                [| minX .. maxX |]
                |> Array.collect (fun x ->
                    [| minY .. maxY |]
                    |> Array.choose (fun y ->
                        let pt = (x, y)
                        let isOutside = not (Goxel.pointInPolygon pt ctx.Boundary)
                        let isInsideIsland = ctx.Islands |> Array.exists (Goxel.pointInPolygon pt)
                        match isOutside || isInsideIsland with
                        | true -> Some (RV(x, y, ctx.Elevation))
                        | false -> None
                    )
                )
            | false -> [||]
    
        let occ = Array.append ctx.InitialOcc bndSet |> hxlUni 1
        let flatEntries = ctx.ScaledTree |> Array.concat
    
        match Array.tryHead flatEntries with
        | None -> None
        | Some (id, ct, lb) ->
            let rootCxl, nextOcc = createBaseCoxel ctx.Seq ctx.Elevation ctx.EntryAtr ctx.EntryFallback id ct lb occ ctx.Parent
            Some (rootCxl, nextOcc)

    /// <summary>
    /// Executes spatial construction for a single level, growing child Coxels recursively from the root Coxel according to the architectural tree.
    /// </summary>
    /// <param name="ctx">Resolved layout context.</param>
    /// <param name="rootCxl">Root base Coxel.</param>
    /// <param name="nextOcc">Occupied hexel coordinates including boundaries and previous rooms.</param>
    /// <returns>Tuple of generated Coxel array, boundary polygon vertex arrays, and applied scaling ratio.</returns>
    /// <example>
    /// <code>
    /// match Xyxel.generateBaseCxl ctx with
    /// | Some (rootCxl, occ) ->
    ///     let rooms, bounds, ratio = Xyxel.generateCxlLayout ctx rootCxl occ
    /// | None -> ()
    /// </code>
    /// </example>
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

        let bounds =
            match Array.isEmpty ctx.Boundary with
            | true -> ctx.Islands
            | false -> Array.append [|ctx.Boundary|] ctx.Islands

        result, bounds, ctx.Ratio

