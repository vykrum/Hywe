module Cache

    open System
    open ModelTypes
    open Hywe.Core
    open Hywe.Core.Coxel
    open Hywe.Core.Paxel
    
    /// <summary>
    /// Computes the full layout data for all levels.
    /// </summary>
    let computeFullLayout (src: string) (sqn: Hexel.Sqn) (polyExport: PolygonExportData) (elv: int) =
        Zaxel.generateMultiLevelLayout 
            src 
            polyExport.EntryStr 
            [||] 
            (Some (elv, sqn)) 
            (Some polyExport.OuterStr) 
            (Some polyExport.IslandsStr)

    /// <summary>
    /// Extracts level-specific configuration from full layout data.
    /// </summary>
    let fromFullLayout (data: Cxl[] * (int * int)[][] * float[] * float[]) (sqn: Hexel.Sqn) (elv: int) : BatchConfgrtns =
        let cxls, cxOuIl, cxElv1, cxRto1 = data
        let sqnStr = sprintf "%A" sqn
        let derived = deriveDataFromLayout cxls cxOuIl cxElv1 cxRto1 elv
        let d = Layout.getStaticGeometry cxls derived.cxClr1 elv 1
        
        {| sqnName = sqnStr
           shapes = d.shapes |> Array.map (fun s -> 
             {| color = s.color; points = s.points; name = s.name; lx = s.lx; ly = s.ly |}) 
           w = d.w; h = d.h
           cxCxl1 = cxls
           cxElv1 = cxElv1
           cxlAvl = derived.cxlAvl
           cxOuIl = cxOuIl
           cxAdj1 = derived.cxAdj1
           cxB36 = derived.cxB36
           cxRto1 = cxRto1 |}

    /// <summary>
    /// Generates a single configuration for a specific level and orientation.
    /// This is the "single source" of processing.
    /// </summary>
    let generateSingleConfig (src: string) (sqn: Hexel.Sqn) (polyExport: PolygonExportData) (elv: int) =
        let data = computeFullLayout src sqn polyExport elv
        fromFullLayout data sqn elv

    /// <summary>
    /// Initializes an empty cache for all current levels.
    /// </summary>
    let init (levels: int list) : LayoutCache =
        levels |> List.map (fun l -> l, Array.replicate 24 None) |> Map.ofList

    /// <summary>
    /// Gets a specific configuration from the cache.
    /// </summary>
    let get (lvl: int) (sqnIdx: int) (cache: LayoutCache) : BatchConfgrtns option =
        cache |> Map.tryFind lvl |> Option.bind (fun arr -> if sqnIdx >= 0 && sqnIdx < 24 then arr.[sqnIdx] else None)

    /// <summary>
    /// Checks if a level has any generated configuration.
    /// </summary>
    let hasAny (lvl: int) (cache: LayoutCache) : bool =
        cache |> Map.tryFind lvl |> Option.map (Array.exists Option.isSome) |> Option.defaultValue false

    /// <summary>
    /// Updates the cache with a new configuration.
    /// </summary>
    let update (lvl: int) (sqnIdx: int) (data: BatchConfgrtns) (cache: LayoutCache) : LayoutCache =
        match cache |> Map.tryFind lvl with
        | Some arr ->
            let newArr = Array.copy arr
            newArr.[sqnIdx] <- Some data
            cache |> Map.add lvl newArr
        | None ->
            let arr = Array.replicate 24 None
            arr.[sqnIdx] <- Some data
            cache |> Map.add lvl arr

    /// <summary>
    /// Finds the first missing configuration for a specific level.
    /// </summary>
    let nextMissingInLevel (lvl: int) (cache: LayoutCache) : int option =
        match cache |> Map.tryFind lvl with
        | Some arr -> arr |> Array.tryFindIndex Option.isNone
        | None -> Some 0

    /// <summary>
    /// Finds the next missing configuration across all levels.
    /// </summary>
    let nextMissingGlobal (cache: LayoutCache) : (int * int) option =
        cache 
        |> Map.toList 
        |> List.sortBy fst 
        |> List.tryPick (fun (lvl, arr) -> 
            arr |> Array.tryFindIndex Option.isNone |> Option.map (fun idx -> lvl, idx))

    /// <summary>
    /// Reconstructs DerivedData from a cached configuration.
    /// </summary>
    let toDerived (config: BatchConfgrtns) : DerivedData =
        { cxCxl1 = config.cxCxl1
          cxlAvl = config.cxlAvl
          cxClr1 = config.shapes |> Array.map (fun s -> s.color)
          cxOuIl = config.cxOuIl
          cxElv1 = config.cxElv1
          cxRto1 = config.cxRto1
          cxAdj1 = config.cxAdj1
          cxB36 = config.cxB36 }

    /// <summary>
    /// Returns all cached variations for a specific level.
    /// </summary>
    let getAllVariations (elv: int) (cache: LayoutCache) =
        [0..23] |> List.choose (fun i -> get elv i cache) |> List.toArray

    /// <summary>
    /// Generates DerivedData directly from source strings and sequences (convenience helper).
    /// </summary>
    let deriveFromSource (src: string) (sqns: Map<int, string>) (poly: PolygonExportData) (lvl: int) =
        let sqnIdx = 
            sqns 
            |> Map.tryFind lvl 
            |> Option.bind (fun s -> Hexel.sqnArray |> Array.tryFindIndex (fun x -> (sprintf "%A" x).Equals(s, StringComparison.OrdinalIgnoreCase))) 
            |> Option.defaultValue 11
        generateSingleConfig src Hexel.sqnArray.[sqnIdx] poly lvl |> toDerived
