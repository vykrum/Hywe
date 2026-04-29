module Parse

open Hexel
open Coxel
open Geometry
open PolygonEditor
open System
open Microsoft.FSharp.Reflection

// ==========================================================================================
// SECTION: Basic Parsing Utilities
// ==========================================================================================

/// <summary> Parses a string into a discriminated union case. </summary>
/// <param name="s"> The string to parse. </param>
/// <returns> The union case or None. </returns>
let tryParseUnion<'T> (s: string) : 'T option =
    if not (FSharpType.IsUnion typeof<'T>) then None
    else
        FSharpType.GetUnionCases typeof<'T>
        |> Array.tryFind (fun c -> c.Name = s)
        |> Option.map (fun c -> FSharpValue.MakeUnion(c,[||]) :?> 'T)

/// <summary> Parses a string into a float safely. </summary>
/// <param name="s"> The string to parse. </param>
/// <returns> The parsed float or None. </returns>
let tryParseFloat (s: string) =
    match Double.TryParse s with
    | true, v -> Some v
    | _ -> None

/// <summary> Parses a string into an integer safely. </summary>
/// <param name="s"> The string to parse. </param>
/// <returns> The parsed integer or None. </returns>
let tryParseInt (s: string) =
    match Int32.TryParse s with
    | true, v -> Some v
    | _ -> None

// ==========================================================================================
// SECTION: Geometry & Polygon Parsing
// ==========================================================================================

/// <summary> Parses a comma-separated string of coordinates into a float array of points. </summary>
/// <param name="s"> The coordinate string (e.g., "0,0,10,0"). </param>
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
        | _ -> 
            [||]

/// <summary> Parses a hyphen-separated string of polygons (islands). </summary>
let parsePolyIslands (s: string) : (float * float)[][] =
    match String.IsNullOrWhiteSpace s with
    | true -> [||]
    | false ->
        s.Split('-', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parsePolygon

/// <summary> Converts float tuples to Point records with scaling. </summary>
let polyToPoints scale (seg: (float*float)[]) =
    seg |> Array.map (fun (x,y) -> { Point.X = x * scale; Y = y * scale })

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
let private extractAttrsFromHyw (text: string) =
    let cleanText = text.Trim().Trim('|').Trim()
    let m = System.Text.RegularExpressions.Regex.Match(cleanText, @"\(([^)]*)\)")
    match m.Success with
    | false -> Map.empty
    | true ->
        m.Groups.[1].Value.Split('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun token ->
            match token.Split('=', 2) with
            | [| k; v |] -> Some (k.Trim(), v.Trim())
            | _ -> None)
        |> Map.ofArray

/// <summary> Parses a single level hywe string into attributes and a structured tree. </summary>
/// <param name="spaceStr"> Properly formatted string (RefId/Count/Label). </param>
/// <returns> A tuple of parsed attributes and space tree definitions. </returns>
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
        let spcMp2 = match (spcMp1 |> Array.head |> Array.head) = "0" with
                        | true -> spcMp1
                        | false -> Array.append [|[|"0";"Q=VRCWEE"|]|] spcMp1
        
        let spcAt1 = spcMp2 
                    |> Array.head 
                    |> Array.tail
                    |> Array.map (fun (x: string) -> x.Split("="))
                    |> Array.choose (fun (x: string[]) -> if x.Length = 2 then Some (x.[0], x.[1]) else None)
                    |> Map.ofArray

        let spcMp6 = spcMp2 
                    |> Array.tail
                    |> Array.map (fun (x: string[]) -> (x.[0], (float x.[1], x.[2]))) 
                    |> Array.sortBy (fun (x,_) -> x)
                    |> Map.ofArray

        let spcKy01 = 
            spcMp6 
            |> Map.keys 
            |> Array.ofSeq 
            |> Array.groupBy(fun x 
                                -> match (x.Length <= 1) with 
                                    |true -> "0"
                                    |false -> x.Substring (0, x.LastIndexOf(".")))
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
            |> Array.groupBy (fun (x,_) -> x)
            |> Array.map (fun x -> snd x)
            |> Array.map (fun (x: (string * string array) array)
                                    -> (Array.map(fun (y,z)
                                                            -> Array.append[|y|] z))x)
            |> Array.map (fun (x: string array array) -> Array.concat x)
            |> Array.map (fun (x: string array) -> Array.distinct x)
            |> Array.map (fun (x: string array) -> Array.sort x)
        
        let spcKy05 = 
            (snd spcKy03)
            |> Array.map (fun (x, (y: string[])) 
                                    -> Array.append [|x|] y)
            |> Array.append spcKy04
            |> Array.sortBy (fun (x: string[]) -> Array.head x)
        let spcKy06 = 
            let a = match (Array.isEmpty spcKy05) with 
                    |  true -> [|[|"1"|]|]
                    | false -> spcKy05
            a
            |> Array.map(fun (x: string[]) 
                                        -> (Array.map (fun (y: string) 
                                                                    -> y, spcMp6 
                                                                    |> Map.find y))x)
        let spcKey =
            spcKy06
            |> Array.map (fun (z: (string * (float * string)) array) 
                                    -> (Array.map (fun (x,y) 
                                                                -> x, fst y, snd y))z)
        spcAt1,spcKey

// ==========================================================================================
// SECTION: Layout Generation Engine
// ==========================================================================================

/// <summary> Consolidates all reproportioning logic into one call. Scales node areas to fit boundary. </summary>
let applyReproportioning (boundaryArea: float) (totalNodeArea: float) (rawTree: (string * float * string)[][]) (ratioOverride: float option) =
    let hxlAreaFactor = 4.0
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
                let finalCount = 
                    if area > 0.0 then max 1 (int (Math.Round(idealCount)))
                    else 0
                (Refid id, Count finalCount, Label lb)
            )
        )
    tree, ratio

/// <summary> The primary layout generation function for Coxels. </summary>
let generateCxlLayout 
    (str: string) 
    (entryAtrFallback: string)
    (seqOverride: Sqn option) 
    (widthOverride: int option)
    (heightOverride: int option)
    (ouStrOverride: string option) 
    (ilStrOverride: string option) 
    (initialOcc : Hxl[])
    (bsHxSetOverride: Cxl option)
    (ratioOverride: float option)
    (elvOverride: int option) : Cxl[] * (float*float)[][] * float =
    
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
            | Some a -> 
                match Double.TryParse a with
                | true, v -> int v
                | _ -> 0
            | None -> 0

    let bdWd = 
        match widthOverride with
        | Some w -> w
        | None ->
            match spcAt1 |> Map.tryFind "W" with 
            | Some (a: string) -> match int a > 0 with | true -> int a | false -> 0
            | None -> 0

    let bdHt = 
        match heightOverride with
        | Some h -> h
        | None ->
            match spcAt1 |> Map.tryFind "H" with 
            | Some (a: string) -> int a
            | None -> bdWd
    
    // Outer Boundary
    let bdOu =
        match ouStrOverride with
        | Some ou when ou <> "" -> parsePolygon ou
        | _ ->
            match spcAt1 |> Map.tryFind "O" with 
            | Some a when a <> "" -> parsePolygon a
            | _ ->
                match bsHxSetOverride with
                | Some parent when bdWd > 0 -> 
                    cxlPrm parent elv
                    |> Geometry.cleanPolygon parent.Seqn
                | _ ->
                    match bdWd > 0 with 
                    | true -> 
                        let a = $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                        parsePolygon a
                    | false -> [||]

    // Island Boundary
    let bdIs =
        match ilStrOverride with
        | Some il when il <> "" -> parsePolyIslands il
        | _ ->
            match spcAt1 |> Map.tryFind "I" with 
            | Some a -> parsePolyIslands a
            | None -> [||]

    // Entry Hexel / Base Shifting Logic
    let entryAtr = spcAt1 |> Map.tryFind "E" |> Option.defaultValue "0"
    
    let entryFallback =
        let fromEditor = 
            let parts = entryAtrFallback.Split ','
            match parts with
            | [| xStr; yStr |] ->
                match System.Int32.TryParse(xStr.Trim()), System.Int32.TryParse(yStr.Trim()) with
                | (true, x), (true, y) -> Some (AV(x, y, elv))
                | _ -> None
            | _ -> None

        match fromEditor with
        | Some h -> h
        | None ->
            match bdWd = 0 with
            | true -> identity elv
            | false -> AV(bdWd/2+2, bdHt/2+2, elv)

    let ntArea = polygonWithHolesArea bdOu bdIs
    let bdrBlocked = Geometry.hxlBdrFill seq elv bdOu bdIs
    let occ = 
        let list = System.Collections.Generic.List<Hxl>(initialOcc.Length + bdrBlocked.Length)
        list.AddRange(initialOcc)
        list.AddRange(bdrBlocked)
        list.ToArray() |> hxlUni 1

    let totalRequested = 
        spcTree 
        |> Array.concat 
        |> Array.distinctBy (fun (id, _, _) -> id) 
        |> Array.sumBy (fun (_, a, _) -> a)
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

            let rootCxl, initialGlobalOcc = Coxel.createBaseCoxel seq elv entryAtr entryFallback id ct lb occ bsHxSetOverride
            let ac1 = [| rootCxl |]

            match Array.length flatEntries < 2 with
            | true -> ac1
            | false -> cxCxCx tree01 ac1 initialGlobalOcc

    result, Array.append [|bdOu|] bdIs, finalRatio

/// <summary> Processes multiple levels separated by ';' in a Hywe string. </summary>
let generateMultiLevelLayout 
    (fullStr: string) 
    (entryAtrFallback: string) 
    (initialOcc: Hxl[])
    (seqOverride: Sqn option)
    (ouStrOverride: string option)
    (ilStrOverride: string option) : Cxl[] * (float*float)[][] * float[] =
    
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

        // Skip layout generation if this is likely a terminal elevation-only segment (no functional nodes)
        if levelStr.Contains("1/") || levelStr.Contains("(1/") then
            let bsHxOverride =
                if i = 0 then None
                else 
                    allCxls 
                    |> Array.filter (fun c -> 
                        let (_, _, z) = Hexel.hxlCrd c.Base
                        z = i - 1)
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
                // Fallback if T is missing (e.g. legacy files)
                let lastBase = allElevations.[allElevations.Length - 1]
                Array.append allElevations [| lastBase + 3.0 |]
        else allElevations
    
    allCxls, allBoundaries, finalElevations

/// <summary> Formats a layout into a Base36-encoded string representation. </summary>
let generateCxlArray (str: string) (seq: Sqn) (ouStr: string) (ilStr: string) (enStr: string) (initialOcc : Hxl[]) = 
    let toBase36 (value: int64) =
        let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let rec convert v acc =
            match v = 0L with
            | true -> match acc = "" with | true -> "0" | false -> acc
            | false -> convert (v / 36L) (string chars.[int (v % 36L)] + acc)
        let prefix = if value < 0L then "-" else ""
        prefix + convert (abs value) ""

    let finalBatch, _, _ = generateMultiLevelLayout str enStr initialOcc (Some seq) (Some ouStr) (Some ilStr)

    finalBatch
    |> Array.map (fun cxl ->
        cxl.Hxls 
        |> Array.map (fun h ->
            let (ax, ay, _) = hxlCrd h
            toBase36 (int64 ax) + "." + toBase36 (int64 ay)
        )
        |> String.concat " "
    )
    |> String.concat ";"

// ==========================================================================================
// SECTION: Dataset Generation Parsing
// ==========================================================================================

/// <summary> Mapping logic for dataset generation. Currently unused in main flow. </summary>
let getTree01 (ntArea: float) (totalRequested: float) (spcTree: (string * float * string) array array) =
    let ratio = match totalRequested > 0.0 with | true -> ntArea / totalRequested | false -> 1.0
    spcTree |> Array.map (fun row -> 
        row |> Array.map (fun (id, area, lb) -> 
            (Refid id, Count (int (Math.Round((area * ratio) / 4.0))), Label lb)))

/// <summary> Recursive Coxel generation logic for dataset generation. Currently unused in main flow. </summary>
let rec cxCxCx (seq: Sqn) (elv: int) (tre: (Prp*Prp*Prp)[][]) (occ: Hxl[]) (acc: Cxl[]) =
    match Array.tryHead tre with 
    | None -> acc
    | Some currentBatch ->
        let newOcc = Array.append occ (Array.concat (acc |> Array.map (fun x -> x.Hxls)))
        let nextTre = Array.tail tre
        
        let hostId = currentBatch |> Array.head |> fun (id, _, _) -> id
        let bsCx = acc |> Array.find (fun x -> x.Rfid = hostId)
        
        let chHx = bsCx.Hxls |> Array.filter (fun x -> (AV(hxlCrd x)) = x)
        let cnt = (Array.length currentBatch) - 1
        let chBs = 
            match (Array.length chHx) >= cnt with 
            | true -> 
                let divs = (Array.length chHx) / cnt 
                Array.chunkBySize divs chHx |> Array.map Array.head |> Array.take cnt
            | false -> 
                Array.append chHx (Array.replicate (cnt - Array.length chHx) (identity elv))
        
        let cxc1 = coxel seq elv (Array.map2 (fun a (_, c, d) -> a, hostId, c, d) chBs (Array.tail currentBatch)) newOcc
        let chOc1 = hxlUni 2 (Array.append newOcc (Array.concat (cxc1 |> Array.map (fun x -> x.Hxls))))
        let cxc2 = cxc1 |> Array.map (fun x -> { x with Hxls = hxlChk seq elv chOc1 x.Hxls; Base = hxlChk seq elv chOc1 [|x.Base|] |> Array.head })
        
        cxCxCx seq elv nextTre newOcc (Array.append acc cxc2)

// ==========================================================================================
// SECTION: Editor Integration
// ==========================================================================================

/// <summary> Parses .hyw content and updates a PolygonEditorModel. </summary>
let importFromHyw (content: string) (current: PolygonEditorModel) : EditorState =

    let cleanStr = content.Replace("\n","").Replace("\t","")
    let sortedLevels = 
        cleanStr.Split(';', StringSplitOptions.RemoveEmptyEntries) |> Array.truncate 1
    
    let mutable finalState = current
    for lvl in sortedLevels do
        let attrs = extractAttrsFromHyw lvl
        finalState <- attrs |> Map.fold (fun (m: PolygonEditorModel) key v ->
            match key with
            | "W" -> match tryParseFloat v with | Some num -> { m with LogicalWidth = num * 10.0 } | None -> m
            | "H" -> match tryParseFloat v with | Some num -> { m with LogicalHeight = num * 10.0 } | None -> m
            | "L" -> match tryParseFloat v with | Some num -> { m with Elevation = int num } | None -> m
            | "S" -> { m with BaseStr = v }
            | "X" -> { m with UseAbsolute = (v = "1") }
            | "E" -> match parseCoords v with | pts when pts.Length = 1 -> { m with EntryPoint = pts.[0] } | _ -> m
            | "O" -> match parseCoords v with | pts when pts.Length > 0 -> { m with Outer = pts } | _ -> m
            | "I" -> { m with Islands = parseIslands v }
            | _ -> m
        ) finalState

    FreshlyImported finalState
