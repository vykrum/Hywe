module Cache

    open System
    open ModelTypes
    open Hywe.Core
    open Hywe.Core.Coxel
    open Hywe.Core.Lexel
    open System.Text.RegularExpressions
    
    /// <summary>
    /// Populates the O= attribute in the source string for any N-blocks using the computed footprint.
    /// </summary>
    let populateNestBoundaries (src: string) (cxls: Cxl[]) =
        let regex = Regex(@"N(\d+)\(([^)]*)\)")
        regex.Replace(src, fun (m: Match) ->
            let idStr = m.Groups.[1].Value
            let attrsStr = m.Groups.[2].Value
            let attrs = Lexel.parseAttributes ("(" + attrsStr + ")")
            let bVal = attrs.Base
            
            match cxls |> Array.tryFind (fun c -> let id = Coxel.prpVlu c.Rfid in id = bVal || id.EndsWith("." + bVal)) with
            | Some hostCxl ->
                let _, _, z = Hexel.hxlCrd hostCxl.Base
                let pts = Coxel.cxlPrm hostCxl z |> Goxel.cleanPolygon hostCxl.Seqn
                let oStr = pts |> Array.map (fun (x,y) -> $"{x},{y}") |> String.concat ","
                
                let minX, minY, maxX, maxY = Goxel.bounds pts
                let w = maxX - minX
                let h = maxY - minY
                
                let s1 = Regex.Replace(attrsStr, @"O=[^/]*", "O=" + oStr)
                let s2 = Regex.Replace(s1, @"W=[^/]*", "W=" + string w)
                let s3 = Regex.Replace(s2, @"H=[^/]*", "H=" + string h)
                let s4 = Regex.Replace(s3, @"X=[^/]*", "X=0")
                
                $"N{idStr}({s4})"
            | None -> 
                m.Value
        )

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
    let fromFullLayout (data: Cxl[] * (int * int)[][][] * float[] * float[]) (sqn: Hexel.Sqn) (elv: int) : BatchConfgrtns =
        let cxls, allBounds, cxElv1, cxRto1 = data
        let cxOuIl = if elv >= 0 && elv < allBounds.Length then allBounds.[elv] else [||]
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
           cxRto1 = cxRto1
           cxClr1 = derived.cxClr1 |}

    /// <summary>
    /// Generates a single configuration for a specific level and orientation.
    /// This is the "single source" of processing.
    /// </summary>
    let generateSingleConfig (src: string) (sqn: Hexel.Sqn) (polyExport: PolygonExportData) (elv: int) =
        let data = computeFullLayout src sqn polyExport elv
        fromFullLayout data sqn elv

    /// <summary>
    /// Initializes an empty cache for all current levels/markers.
    /// </summary>
    let init (markers: string list) : LayoutCache =
        markers |> List.map (fun m -> m, Array.replicate 24 None) |> Map.ofList

    /// <summary>
    /// Gets a specific configuration from the cache.
    /// </summary>
    let get (marker: string) (sqnIdx: int) (cache: LayoutCache) : BatchConfgrtns option =
        cache |> Map.tryFind marker |> Option.bind (fun arr -> if sqnIdx >= 0 && sqnIdx < 24 then arr.[sqnIdx] else None)

    /// <summary>
    /// Checks if a level has any generated configuration.
    /// </summary>
    let hasAny (marker: string) (cache: LayoutCache) : bool =
        cache |> Map.tryFind marker |> Option.map (Array.exists Option.isSome) |> Option.defaultValue false

    /// <summary>
    /// Updates the cache with a new configuration.
    /// </summary>
    let update (marker: string) (sqnIdx: int) (data: BatchConfgrtns) (cache: LayoutCache) : LayoutCache =
        match cache |> Map.tryFind marker with
        | Some arr ->
            let newArr = Array.copy arr
            newArr.[sqnIdx] <- Some data
            cache |> Map.add marker newArr
        | None ->
            let arr = Array.replicate 24 None
            arr.[sqnIdx] <- Some data
            cache |> Map.add marker arr

    /// <summary>
    /// Finds the first missing configuration for a specific level.
    /// </summary>
    let nextMissingInMarker (marker: string) (cache: LayoutCache) : int option =
        match cache |> Map.tryFind marker with
        | Some arr -> arr |> Array.tryFindIndex Option.isNone
        | None -> Some 0

    /// <summary>
    /// Finds the next missing configuration across all levels.
    /// </summary>
    let nextMissingGlobal (cache: LayoutCache) : (string * int) option =
        cache 
        |> Map.toList 
        |> List.sortBy fst 
        |> List.tryPick (fun (marker, arr) -> 
            arr |> Array.tryFindIndex Option.isNone |> Option.map (fun idx -> marker, idx))

    /// <summary>
    /// Reconstructs DerivedData from a cached configuration.
    /// </summary>
    let toDerived (config: BatchConfgrtns) : DerivedData =
        { cxCxl1 = config.cxCxl1
          cxlAvl = config.cxlAvl
          cxClr1 = config.cxClr1
          cxOuIl = config.cxOuIl
          cxElv1 = config.cxElv1
          cxRto1 = config.cxRto1
          cxAdj1 = config.cxAdj1
          cxB36 = config.cxB36 }

    /// <summary>
    /// Returns all cached variations for a specific marker.
    /// </summary>
    let getAllVariations (marker: string) (cache: LayoutCache) =
        [0..23] |> List.choose (fun i -> get marker i cache) |> List.toArray

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
