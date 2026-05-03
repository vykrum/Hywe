namespace Hywe.Core

module Parsing =
    open Hexel
    open Coxel
    open Geometry
    open System
    open Microsoft.FSharp.Reflection
    open System.Text.RegularExpressions

    // ==========================================================================================
    // SECTION: Basic Parsing Utilities
    // ==========================================================================================
    
    /// <summary> Parses a string into a discriminated union case. </summary>
    let tryParseUnion<'T> (s: string) : 'T option =
        if not (FSharpType.IsUnion typeof<'T>) then None
        else
            FSharpType.GetUnionCases typeof<'T>
            |> Array.tryFind (fun c -> c.Name = s)
            |> Option.map (fun c -> FSharpValue.MakeUnion(c,[||]) :?> 'T)

    /// <summary> Parses a string into a float safely. </summary>
    let tryParseFloat (s: string) =
        match Double.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Parses a string into an integer safely. </summary>
    let tryParseInt (s: string) =
        match Int32.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Injects or updates the sequence (Sqn) attribute in a Hywe string segment. </summary>
    let injectSqn (input: string) (newSqn: string) =
        let regex = Regex(@"\(([^)]*)\)")
        let matches = regex.Matches(input)
        if matches.Count = 0 then input
        else
            let firstMatch = matches.[0]
            let attrs = firstMatch.Groups.[1].Value
            let newAttrs = 
                if attrs.Contains("Q=") then
                    Regex.Replace(attrs, "Q=[^/)]*", "Q=" + newSqn)
                else
                    attrs + "/Q=" + newSqn
            input.Remove(firstMatch.Index, firstMatch.Length).Insert(firstMatch.Index, "(" + newAttrs + ")")

    // ==========================================================================================
    // SECTION: Geometry & Polygon Parsing
    // ==========================================================================================
    
    /// <summary> Parses a comma-separated string of coordinates into a float array of points. </summary>
    let parsePolygon (s: string) : (float * float)[] =
        s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun x ->
            match Double.TryParse(x.Trim()) with
            | true, v -> Some v
            | _ -> None)
        |> fun nums ->
            match nums.Length % 2 with
            | 0 ->
                nums
                |> Array.chunkBySize 2
                |> Array.choose (function
                    | [| a; b |] -> Some (a, b)
                    | _ -> None)
            | _ -> [||]

    /// <summary> Parses a hyphen-separated string of polygons (islands). </summary>
    let parsePolyIslands (s: string) : (float * float)[][] =
        if String.IsNullOrWhiteSpace s then [||]
        else
            s.Split('-', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parsePolygon

    /// <summary> Converts float tuples to Point records with scaling. </summary>
    let polyToPoints scale (seg: (float*float)[]) =
        seg |> Array.map (fun (x,y) -> { Hexel.Point.X = x * scale; Y = y * scale })

    /// <summary> Parses coordinates from a string and converts them to scaled Points. </summary>
    let parseCoords value =
        parsePolygon value
        |> polyToPoints 10.0

    /// <summary> Parses multiple island polygons from a string and converts them to scaled Points. </summary>
    let parseIslands value =
        parsePolyIslands value
        |> Array.map (polyToPoints 10.0)

    // ==========================================================================================
    // SECTION: Main Hywe Syntax Parsing
    // ==========================================================================================
    
    /// <summary> Extracts key-value attributes from a Hywe string segment (e.g., "(W=100/B=200)"). </summary>
    let extractAttrsFromHyw (text: string) =
        let cleanText = text.Trim().Trim('|').Trim()
        let m = Regex.Match(cleanText, @"\(([^)]*)\)")
        if not m.Success then Map.empty
        else
            m.Groups.[1].Value.Split('/', StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun token ->
                match token.Split('=', 2) with
                | [| k; v |] -> Some (k.Trim(), v.Trim())
                | _ -> None)
            |> Map.ofArray

    /// <summary> Parses a single level hywe string into attributes and a structured tree. </summary>
    let spaceSeq (spaceStr: string) = 
        let splitTopLevel (s: string) : string[] =
            let rec loop (acc: string list) (curr: string) (depth: int) (chars: char list) =
                match chars with
                | [] -> List.rev (curr :: acc)
                | c::cs ->
                    match c with
                    | '(' -> loop acc (curr + string c) (depth + 1) cs
                    | ')' -> loop acc (curr + string c) (depth - 1) cs
                    | ',' when depth = 0 -> loop (curr :: acc) "" depth cs
                    | _ -> loop acc (curr + string c) depth cs
            loop [] "" 0 (Seq.toList s) |> List.toArray
        
        let cleanStr = spaceStr.Replace("\n","").Replace("\t","").Replace(" ","")
        let spcMp1 = 
            cleanStr
            |> splitTopLevel
            |> Array.choose (fun x -> 
                if String.IsNullOrWhiteSpace x then None
                else 
                    let trimmed = x.Trim('(', ')')
                    if String.IsNullOrWhiteSpace trimmed then None
                    else Some (trimmed.Split '/'))
        
        if Array.isEmpty spcMp1 then Map.empty, [||]
        else
            let spcMp2 = if (spcMp1 |> Array.head |> Array.head) = "0" then spcMp1 else Array.append [|[|"0";"Q=VRCWEE"|]|] spcMp1
            
            let spcAt1 = spcMp2 
                        |> Array.head 
                        |> Array.tail
                        |> Array.map (fun (x: string) -> x.Split("="))
                        |> Array.choose (fun (x: string[]) -> if x.Length = 2 then Some (x.[0], x.[1]) else None)
                        |> Map.ofArray

            let spcMp6 = spcMp2 
                        |> Array.tail
                        |> Array.map (fun (x: string[]) -> (x.[0], (float x.[1], x.[2]))) 
                        |> Array.sortBy fst
                        |> Map.ofArray

            let spcKy01 = 
                spcMp6 
                |> Map.keys 
                |> Array.ofSeq 
                |> Array.groupBy(fun x -> if x.Length <= 1 then "0" else x.Substring (0, x.LastIndexOf(".")))

            let spcKy02 = 
                match (spcKy01 |> Array.tryFind (fun (k,_) -> k = "0")) with
                | Some (_, v) when v.Length >= 2 -> 
                    v |> Array.windowed 2 |> Array.map(fun (x: string[]) -> x.[0], [|x.[1]|])
                | _ -> [||]
            
            let spcKy03 = 
                spcKy01 
                |> Array.filter (fun (k,_) -> k <> "0")
                |> Array.partition (fun (x,_) -> x.Length = 1)
            
            let spcKy04 = 
                (Array.append spcKy02 (fst spcKy03)) 
                |> Array.groupBy fst
                |> Array.map snd
                |> Array.map (fun (x: (string * string array) array) -> (Array.map(fun (y,z) -> Array.append[|y|] z)) x)
                |> Array.map Array.concat
                |> Array.map Array.distinct
                |> Array.map Array.sort
            
            let spcKy05 = 
                (snd spcKy03)
                |> Array.map (fun (x, (y: string[])) -> Array.append [|x|] y)
                |> Array.append spcKy04
                |> Array.sortBy Array.head

            let spcKy06 = 
                let a = if Array.isEmpty spcKy05 then [|[|"1"|]|] else spcKy05
                a |> Array.map(fun (x: string[]) -> (Array.map (fun (y: string) -> y, spcMp6 |> Map.find y)) x)

            let spcKey =
                spcKy06
                |> Array.map (fun (z: (string * (float * string)) array) -> (Array.map (fun (x,y) -> x, fst y, snd y)) z)

            spcAt1, spcKey

    // ==========================================================================================
    // SECTION: Layout Generation Engine
    // ==========================================================================================
    
    /// <summary> Consolidates all reproportioning logic into one call. Scales node areas to fit boundary. </summary>
    let applyReproportioning (boundaryArea: float) (totalNodeArea: float) (rawTree: (string * float * string)[][]) (ratioOverride: float option) =
        let hxlAreaFactor = 1.0
        let isUnbound = boundaryArea <= 0.0
        let reductionFactor = 0.96 
        let ratio = 
            match ratioOverride with
            | Some r -> 
                if isUnbound || totalNodeArea <= 0.0 then r
                else 
                    let localRatio = (boundaryArea / totalNodeArea) * reductionFactor
                    min r localRatio
            | None ->
                if isUnbound || totalNodeArea <= 0.0 then 1.0 
                else (boundaryArea / totalNodeArea) * reductionFactor
        
        let tree = 
            rawTree |> Array.map (fun row ->
                row |> Array.map (fun (id, area, lb) ->
                    let idealCount = (area * ratio) / hxlAreaFactor
                    let finalCount = if area > 0.0 then max 1 (int (Math.Round(idealCount))) else 0
                    (Refid id, Count finalCount, Label lb)
                )
            )
        tree, ratio

    /// <summary> The primary layout generation function for Coxels. </summary>
    let generateCxlLayout (str: string) (entryAtrFallback: string) (seqOverride: Sqn option) (widthOverride: int option) (heightOverride: int option) (ouStrOverride: string option) (ilStrOverride: string option) (initialOcc : Hxl[]) (bsHxSetOverride: Cxl option) (ratioOverride: float option) (elvOverride: int option) : Cxl[] * (float*float)[][] * float =
        let spcAt1, spcTree = spaceSeq str
        let seq = 
            match seqOverride with
            | Some s -> s
            | None -> spcAt1 |> Map.tryFind "Q" |> Option.bind tryParseUnion<Sqn> |> Option.defaultValue VRCWEE
    
        let elv = 
            match elvOverride with
            | Some e -> e
            | None ->
                match spcAt1 |> Map.tryFind "L" with 
                | Some a -> match Double.TryParse a with | true, v -> int v | _ -> 0
                | None -> 0
    
        let bdWd = 
            match widthOverride with
            | Some w -> w
            | None ->
                match spcAt1 |> Map.tryFind "W" with 
                | Some (a: string) -> if int a > 0 then int a else 0
                | None -> 0
    
        let bdHt = 
            match heightOverride with
            | Some h -> h
            | None ->
                match spcAt1 |> Map.tryFind "H" with 
                | Some (a: string) -> int a
                | None -> bdWd
        
        let bdOu =
            let isAbs = spcAt1 |> Map.tryFind "X" |> Option.defaultValue "0" = "1"
            if isAbs then [||]
            else
                match ouStrOverride with
                | Some ou when ou <> "" -> parsePolygon ou
                | _ ->
                    match spcAt1 |> Map.tryFind "O" with 
                    | Some a when a <> "" -> parsePolygon a
                    | _ ->
                        match bsHxSetOverride with
                        | Some parent when bdWd > 0 -> cxlPrm parent elv |> Geometry.cleanPolygon parent.Seqn
                        | _ -> if bdWd > 0 then parsePolygon $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0" else [||]
    
        let bdIs =
            match ilStrOverride with
            | Some il when il <> "" -> parsePolyIslands il
            | _ -> match spcAt1 |> Map.tryFind "I" with | Some a -> parsePolyIslands a | None -> [||]
    
        let entryAtr = spcAt1 |> Map.tryFind "E" |> Option.defaultValue "0"
        let entryFallback =
            let fromEditor = 
                let parts = entryAtrFallback.Split ','
                match parts with
                | [| xStr; yStr |] ->
                    match Int32.TryParse(xStr.Trim()), Int32.TryParse(yStr.Trim()) with
                    | (true, x), (true, y) -> Some (AV(x, y, elv))
                    | _ -> None
                | _ -> None
    
            match fromEditor with
            | Some h -> h
            | None -> if bdWd = 0 then identity elv else AV(bdWd/2+2, bdHt/2+2, elv)
    
        let ntArea = polygonWithHolesArea bdOu bdIs
        let bdrBlocked = Geometry.hxlBdrFill seq elv bdOu bdIs
        let occ = 
            let list = System.Collections.Generic.List<Hxl>(initialOcc.Length + bdrBlocked.Length)
            list.AddRange(initialOcc)
            list.AddRange(bdrBlocked)
            list.ToArray() |> hxlUni 1
    
        let totalRequested = spcTree |> Array.concat |> Array.distinctBy (fun (id, _, _) -> id) |> Array.sumBy (fun (_, a, _) -> a)
        let tree01, finalRatio = applyReproportioning ntArea totalRequested spcTree ratioOverride
        let flatEntries = tree01 |> Array.concat
    
        let result =
            match Array.tryHead flatEntries with
            | None -> [||]
            | Some (id, ct, lb) ->
                let rec cxCxCx (tre : (Prp*Prp*Prp)[][]) (currentAcc: Cxl[]) (currentOcc: Hxl[]) =
                    match Array.tryHead tre with 
                    | Some a -> 
                        let parentPrp = a |> Array.head |> fun (i,_,_) -> i
                        match currentAcc |> Array.tryFindIndex (fun x -> x.Rfid = parentPrp) with
                        | Some bsIdx ->
                            let bsCx = currentAcc.[bsIdx]
                            let updatedHost, newCxls, newOcc = coxelChildren seq elv bsCx a currentOcc
                            let nextAcc = 
                                let mappedAcc = currentAcc |> Array.mapi (fun i cx -> if i = bsIdx then updatedHost else cx)
                                Array.append mappedAcc newCxls
                            cxCxCx (Array.tail tre) nextAcc newOcc
                        | None -> cxCxCx (Array.tail tre) currentAcc currentOcc
                    | None -> currentAcc
    
                let rootCxl, initialGlobalOcc = createBaseCoxel seq elv entryAtr entryFallback id ct lb occ bsHxSetOverride
                let ac1 = [| rootCxl |]
                if Array.length flatEntries < 2 then ac1 else cxCxCx tree01 ac1 initialGlobalOcc
    
        result, Array.append [|bdOu|] bdIs, finalRatio
    
    /// <summary> Processes multiple levels separated by ';' in a Hywe string. </summary>
    let generateMultiLevelLayout (fullStr: string) (entryAtrFallback: string) (initialOcc: Hxl[]) (seqOverride: Sqn option) (ouStrOverride: string option) (ilStrOverride: string option) : Cxl[] * (float*float)[][] * float[] =
        let cleanStr = fullStr.Replace("\n","").Replace("\t","")
        let levels = cleanStr.Split(';', StringSplitOptions.RemoveEmptyEntries)
        
        let mutable allCxls = [||]
        let mutable allBoundaries = [||]
        let mutable allElevations = [||]
        let mutable baseRatio = None
        
        let mutable rootW = None
        let mutable rootH = None
        let mutable rootO = ouStrOverride
        let mutable rootI = ilStrOverride
        
        for i, levelStr in levels |> Array.indexed do
            let cleanLvl = levelStr.Trim().Trim('|').Trim()
            let attrs = extractAttrsFromHyw cleanLvl
            
            if i = 0 then
                rootW <- attrs |> Map.tryFind "W" |> Option.bind tryParseInt
                rootH <- attrs |> Map.tryFind "H" |> Option.bind tryParseInt
                if rootO.IsNone then rootO <- attrs |> Map.tryFind "O"
                if rootI.IsNone then rootI <- attrs |> Map.tryFind "I"
     
            let eStr = attrs |> Map.tryFind "E" |> Option.defaultValue "0"
            let curW = attrs |> Map.tryFind "W" |> Option.bind tryParseInt |> Option.orElse rootW
            let curH = attrs |> Map.tryFind "H" |> Option.bind tryParseInt |> Option.orElse rootH
            let curO = attrs |> Map.tryFind "O" |> Option.orElse rootO
            let curI = attrs |> Map.tryFind "I" |> Option.orElse rootI
     
            let curElv = attrs |> Map.tryFind "L" |> Option.bind tryParseFloat |> Option.defaultValue 0.0
            allElevations <- Array.append allElevations [| curElv |]
    
            if Regex.IsMatch(levelStr, @"\d+/") then
                let bsHxOverride =
                    if i = 0 then None
                    else 
                        allCxls 
                        |> Array.filter (fun c -> let (_, _, z) = hxlCrd c.Base in z = i - 1)
                        |> Array.tryFind (fun c -> prpVlu c.Rfid = eStr)
                    
                let cxls, bounds, ratio = generateCxlLayout cleanLvl entryAtrFallback seqOverride curW curH curO curI initialOcc bsHxOverride baseRatio (Some i)
                if i = 0 then baseRatio <- Some ratio
                allCxls <- Array.append allCxls cxls
                allBoundaries <- Array.append allBoundaries bounds
            
        let finalElevations = 
            if levels.Length > 0 then
                let lastLvl = levels.[levels.Length - 1].Trim().Trim('|').Trim()
                let lastAttrs = extractAttrsFromHyw lastLvl
                match lastAttrs |> Map.tryFind "T" |> Option.bind tryParseFloat with
                | Some t -> 
                    let lastBase = allElevations.[allElevations.Length - 1]
                    Array.append allElevations [| lastBase + t |]
                | None -> 
                    let lastBase = allElevations.[allElevations.Length - 1]
                    Array.append allElevations [| lastBase + 3.0 |]
            else allElevations
        
        allCxls, allBoundaries, finalElevations
