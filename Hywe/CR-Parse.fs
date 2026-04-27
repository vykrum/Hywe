module Parse

open Hexel
open Coxel
open Geometry
open PolygonEditor
open System
open Microsoft.FSharp.Reflection

// Sample Space Program Input Format
let spaceStr =
     "(1/15/Foyer),(2/20/Living),(3/20/Dining),
    (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"
///

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

/// <summary> Retrieves an attribute from a map with a fallback. </summary>
/// <param name="key"> The key to look up. </param>
/// <param name="f"> The conversion function. </param>
/// <param name="fallback"> The default value if parsing fails. </param>
/// <param name="m"> The map to search. </param>
/// <returns> The resulting parsed value. </returns>
let getAttr key f fallback (m: Map<string,string>) =
    m
    |> Map.tryFind key
    |> Option.bind (fun v -> try Some (f v) with _ -> None)
    |> Option.defaultValue fallback
///

/// <summary> Categorize constituent Hexels within a Coxel. </summary>
/// <param name="spaceStr"> Properly formatted string (RefId,Count,Lablel) </param>
/// <returns> A tuple of parsed attributes and space tree definitions. </returns>
let spaceSeq 
    (spaceStr:string) = 
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

    let spcMp1 = ((spaceStr.Replace ("\n",""))
                    .Replace("\t","")
                    .Replace(" ",""))
                    |> splitTopLevel
                    |> Array.map(fun x -> x.Remove(0,1)) 
                    |> Array.map(fun x -> x.Remove(x.Length-1,1))
                    |> Array.map (fun x -> x.Split "/")
    
    let spcMp2 = match ((spcMp1 |> Array.head |> Array.head) = "0") with
                    | true -> spcMp1
                    | false -> Array.append [|[|"0";"Q=22"|]|] spcMp1
    
    let spcAt1 = spcMp2 
                |> Array.head 
                |> Array.tail
                |> Array.map (fun x -> x.Split("="))
                |> Array.map (fun x -> x[0],x[1])
                |> Map.ofArray

    let spcCt1 = spcMp2 |> Array.tail |> Array.map(fun x -> x[1])
    let spcMp3 = spcMp2 |> Array.tail
    let spcMp4 = Array.map2 (fun x y -> Array.set x 1 y) spcMp3 spcCt1
    let spcMp5 = Array.append [|spcMp2 |> Array.head|] spcMp3
    let spcMp6 = spcMp5 
                |> Array.tail
                |> Array.map (fun x -> (x[0],(float x[1],x[2]))) 
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
        spcKy01 
        |> Array.head 
        |> snd 
        |> Array.windowed 2 
        |> Array.map(fun x -> x[0],[|x[1]|])
    
    let spcKy03 = 
        spcKy01 
        |> Array.tail 
        |> Array.partition (fun (x,_) -> x.Length = 1)
    
    let spcKy04 = 
        (Array.append spcKy02 (fst spcKy03)) 
        |> Array.groupBy (fun (x,_) -> x)
        |> Array.map (fun x -> snd x)
        |> Array.map (fun x 
                                -> (Array.map(fun (y,z)
                                                        -> Array.append[|y|] z))x)
        |> Array.map (fun x -> Array.concat x)
        |> Array.map (fun x -> Array.distinct x)
        |> Array.map (fun x -> Array.sort x)
    
    let spcKy05 = 
        (snd spcKy03)
        |> Array.map (fun (x,y) 
                                -> Array.append [|x|] y)
        |> Array.append spcKy04
        |> Array.sortBy (fun x -> Array.head x)
    let spcKy06 = 
        let a = match (Array.isEmpty spcKy05) with 
                |  true -> [|[|"1"|]|]
                | false -> spcKy05
        a
        |> Array.map(fun x 
                                    -> (Array.map (fun y 
                                                                -> y, spcMp6 
                                                                |> Map.find y))x)
    let spcKey =
        spcKy06
        |> Array.map (fun z 
                                -> (Array.map (fun (x,y) 
                                                            -> x, fst y, snd y))z)
    spcAt1,spcKey
///

/// Parses "0,0,10,0-2,2,4,4" into float segments
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
            // better to fail loudly during dev
            [||]

let parsePolyIslands (s: string) : (float * float)[][] =
    match String.IsNullOrWhiteSpace s with
    | true -> [||]
    | false ->
        s.Split('-', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parsePolygon

let polyToPoints scale (seg: (float*float)[]) =
    seg |> Array.map (fun (x,y) -> { Point.X = x * scale; Y = y * scale })

let parseCoords value =
    parsePolygon value
    |> polyToPoints 10.0

let parseIslands value =
    parsePolyIslands value
    |> Array.map (polyToPoints 10.0)
///

/// Area Logic
let getNtArea (bdOu: (float*float)[]) (bdIs: (float*float)[][]) = 
    polygonWithHolesArea bdOu bdIs

/// Tree Mapping (Dataset Generation Version)
let getTree01 (ntArea: float) (totalRequested: float) (spcTree: (string * float * string) array array) =
    let ratio = match totalRequested > 0.0 with | true -> ntArea / totalRequested | false -> 1.0
    spcTree |> Array.map (fun row -> 
        row |> Array.map (fun (id, area, lb) -> 
            (Refid id, Count (int (Math.Round((area * ratio) / 4.0))), Label lb)))

/// Recursive Generation (Dataset Generation Version)
let rec cxCxCx (seq: Sqn) (elv: int) (tre: (Prp*Prp*Prp)[][]) (occ: Hxl[]) (acc: Cxl[]) =
    match Array.tryHead tre with 
    | None -> acc
    | Some currentBatch ->
        let newOcc = Array.append occ (Array.concat (acc |> Array.map (fun x -> x.Hxls)))
        let nextTre = Array.tail tre
        
        // Use the triple pattern to find the hostId
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
///
let hxlAreaX = 4
let hxlAreaFactor = 4.0

/// <summary> Consolidates all reproportioning logic into one call. </summary>
/// <param name="boundaryArea"> Net site area in sq units. </param>
/// <param name="totalNodeArea"> Sum of all requested node areas. </param>
/// <param name="rawTree"> Nested space tree of (id * area * label). </param>
/// <returns> Tree with counts scaled to fill the available site area. </returns>
let applyReproportioning (boundaryArea: float) (totalNodeArea: float) (rawTree: (string * float * string)[][]) =
    let hxlAreaFactor = 4.0
    
    // 1. Determine Scaling Ratio
    let isUnbound = boundaryArea <= 0.0
    // Provide 4% breathing room. Scaled nodes will target 96% of the available site area.
    // This allows later nodes in the sequence to find available space despite geometric inefficiencies.
    let reductionFactor = 0.96 
    let ratio = 
        if isUnbound || totalNodeArea <= 0.0 then 1.0 
        else (boundaryArea / totalNodeArea) * reductionFactor

    // 2. Scale and Round each node
    rawTree |> Array.map (fun row ->
        row |> Array.map (fun (id, area, lb) ->
            // idealCount = (node area * scale ratio) / area per hexel (4.0)
            let idealCount = (area * ratio) / hxlAreaFactor
            let finalCount = 
                if area > 0.0 then max 1 (int (Math.Round(idealCount)))
                else 0
            (Refid id, Count finalCount, Label lb)
        )
    )
///

let generateCxlLayout 
    (str: string) 
    (seqOverride: Sqn option) 
    (ouStrOverride: string option) 
    (ilStrOverride: string option) 
    (initialOcc : Hxl[]) : Cxl[] * (float*float)[][] =
    
    // 1. Get Attributes and Tree
    let spcAt1, spcTree = spaceSeq str

    // 2. Sequence
    let seq = 
        match seqOverride with
        | Some s -> s
        | None -> spcAt1 |> Map.tryFind "Q" |> Option.bind tryParseUnion<Sqn> |> Option.defaultValue VRCWEE

    // 3. Elevation
    let elv = match spcAt1 |> Map.tryFind "L" with | Some a -> a |> int | None -> 0

    // 4. Width and Height
    let bdWd = 
        match spcAt1 |> Map.tryFind "W" with 
        | Some a -> match a |> int > 0 with | true -> a |> int | false -> 0
        | None -> 0

    let bdHt = 
        match spcAt1 |> Map.tryFind "H" with 
        | Some a -> match bdWd > 0 with | true -> a |> int | false -> bdWd
        | None -> bdWd

    // 5. Outer Boundary
    let bdOu =
        match ouStrOverride with
        | Some ou when ou <> "" -> parsePolygon ou
        | _ ->
            match spcAt1 |> Map.tryFind "O" with 
            | Some a -> parsePolygon a
            | None ->
                match bdWd > 0 with 
                | true -> 
                    let a = $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                    parsePolygon a
                | false -> [||]

    // 6. Island Boundary
    let bdIs =
        match ilStrOverride with
        | Some il when il <> "" -> parsePolyIslands il
        | _ ->
            match spcAt1 |> Map.tryFind "I" with 
            | Some a -> parsePolyIslands a
            | None -> [||]

    // 7. Entry Hexel
    let bsHx =
        match spcAt1 |> Map.tryFind "E" with
        | Some a ->
            let parts = a.Split ',' |> Array.choose (fun s -> match System.Int32.TryParse(s.Trim()) with | true, v -> Some v | _ -> None)
            match parts with 
            | [| x; y |] -> hxlLin seq elv (identity elv) (AV(x, y, elv)) |> hxlUni 1 |> Array.last 
            | _ -> identity elv
        | None ->
            match bdWd = 0 with
            | true -> AV(0, 0, elv)
            | false -> hxlLin seq elv (identity elv) (AV(bdWd/2+2, bdHt/2+2, elv)) |> hxlUni 1 |> Array.last

    // 8. Site Net Area: Geometric area of the site boundary string (in sq units).
    let ntArea = polygonWithHolesArea bdOu bdIs

    // 9. Occupancy: Defines blocked hexels. 
    // Boundary hexels (ouHx) are currently added to 'occ', making them obstacles for nodes.
    let ouHx = match Array.isEmpty bdOu with | true -> [||] | false -> hxlPgn seq elv bdOu 
    let ilHx = match Array.isEmpty bdIs with | true -> [||] | false -> bdIs |> Array.collect (hxlPgn seq elv)
    let occ = 
        let list = System.Collections.Generic.List<Hxl>(initialOcc.Length + ouHx.Length + ilHx.Length)
        list.AddRange(initialOcc)
        list.AddRange(ouHx)
        list.AddRange(ilHx)
        list.ToArray()

    // 10. Reproportioning: Calculates node counts (Size) based on ntArea and requested areas.
    // Sum the areas of all unique nodes to determine the scaling ratio.
    let totalRequested = 
        spcTree 
        |> Array.concat 
        |> Array.distinctBy (fun (id, _, _) -> id) 
        |> Array.sumBy (fun (_, a, _) -> a)
    let tree01 = applyReproportioning ntArea totalRequested spcTree
    let flatEntries = tree01 |> Array.concat

    // 11. Core Recursive Logic
    let result =
        match Array.tryHead flatEntries with
        | None -> [||]
        | Some (id, ct, lb) ->
            let cti = match ct with | Count x when x > 0 -> Count (x - 1) | _ -> Count 0
            let ac0 = coxel seq elv [| bsHx, id, cti, lb |] occ

            match Array.tryHead ac0 with
            | None -> [||]
            | Some firstCxl ->
                let ac1 = [| { firstCxl with Hxls = Array.except occ (Array.append [| firstCxl.Base |] firstCxl.Hxls) } |]
                let initialGlobalOcc = [| occ; [|bsHx|]; ac1.[0].Hxls |] |> Array.concat
                
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

                match Array.length flatEntries < 2 with
                | true -> ac1
                | false -> cxCxCx tree01 ac1 initialGlobalOcc

    result, Array.append [|bdOu|] bdIs
///



let private extractAttrsFromHyw (text: string) =
    let m = System.Text.RegularExpressions.Regex.Match(text, @"\(([^)]*)\)")
    match m.Success with
    | false -> Map.empty
    | true ->
        m.Groups.[1].Value.Split('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun token ->
            match token.Split('=', 2) with
            | [| k; v |] -> Some (k.Trim(), v.Trim())
            | _ -> None)
        |> Map.ofArray

/// Parses the .hyw content and returns an updated PolygonEditorModel
let importFromHyw (content: string) (current: PolygonEditorModel) : EditorState =

    let attrs = extractAttrsFromHyw content

    let baseModel = 
        { current with 
            Islands = [||]
            UseBoundary = true
            PolygonEnabled = true }

    let finalModel =
        attrs
        |> Map.fold (fun m key v ->
            match key with
            | "W" ->
                match tryParseFloat v with
                | Some num -> { m with LogicalWidth = num * 10.0 }
                | None -> m

            | "H" ->
                match tryParseFloat v with
                | Some num -> { m with LogicalHeight = num * 10.0 }
                | None -> m

            | "O" ->
                match parseCoords v with
                | pts when pts.Length > 0 -> { m with Outer = pts }
                | _ -> m

            | "I" ->
                { m with Islands = parseIslands v }

            | "A" ->
                { m with UseAbsolute = (v = "1") }

            | "E" ->
                match parseCoords v with
                | [| pt |] -> { m with EntryPoint = pt }
                | _ -> m

            | _ -> m
        ) baseModel

    FreshlyImported finalModel

let generateCxlArray (str: string) (seq: Sqn) (ouStr: string) (ilStr: string) (initialOcc : Hxl[]) = 
    let toBase36 (value: int64) =
        let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let rec convert v acc =
            match v = 0L with
            | true -> match acc = "" with | true -> "0" | false -> acc
            | false -> convert (v / 36L) (string chars.[int (v % 36L)] + acc)
        convert (abs value) ""

    let finalBatch, _ = generateCxlLayout str (Some seq) (Some ouStr) (Some ilStr) initialOcc

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
