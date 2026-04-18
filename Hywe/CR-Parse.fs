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

let tryParseUnion<'T> (s: string) : 'T option =
    if not (FSharpType.IsUnion typeof<'T>) then None
    else
        FSharpType.GetUnionCases typeof<'T>
        |> Array.tryFind (fun c -> c.Name = s)
        |> Option.map (fun c -> FSharpValue.MakeUnion(c,[||]) :?> 'T)

let tryParseFloat (s: string) =
    match Double.TryParse s with
    | true, v -> Some v
    | _ -> None

let tryParseInt (s: string) =
    match Int32.TryParse s with
    | true, v -> Some v
    | _ -> None

let getAttr key f fallback (m: Map<string,string>) =
    m
    |> Map.tryFind key
    |> Option.bind (fun v -> try Some (f v) with _ -> None)
    |> Option.defaultValue fallback
///

/// <summary> Categorize constituent Hexels within a Coxel. 
///</summary>
/// <param name="spaceStr"> Properly formatted string (RefId,Count,Lablel) </param>
/// <returns> Array of string arrays (RefId as string * Count as int * Label as string)  </returns>
/// <summary>
/// Categorize constituent Hexels within a Coxel
/// </summary>
let spaceSeq 
    (spaceStr:string) = 
    // Each hexel is 4sq units in area
    let hxlAreaX = 4
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
                    |> Array.Parallel.map(fun x -> x.Remove(0,1)) 
                    |> Array.Parallel.map(fun x -> x.Remove(x.Length-1,1))
                    |> Array.Parallel.map (fun x -> x.Split "/")
    
    let spcMp2 = match ((spcMp1 |> Array.head |> Array.head) = "0") with
                    | true -> spcMp1
                    | false -> Array.append [|[|"0";"Q=22"|]|] spcMp1
    
    let spcAt1 = spcMp2 
                |> Array.head 
                |> Array.tail
                |> Array.Parallel.map (fun x -> x.Split("="))
                |> Array.Parallel.map (fun x -> x[0],x[1])
                |> Map.ofArray

    let spcCt1 = spcMp2 |> Array.tail |> Array.map(fun x -> x[1])
    let spcMp3 = spcMp2 |> Array.tail
    let spcMp4 = Array.map2 (fun x y -> Array.set x 1 y) spcMp3 spcCt1
    let spcMp5 = Array.append [|spcMp2 |> Array.head|] spcMp3
    let spcMp6 = spcMp5 
                |> Array.tail
                |> Array.Parallel.map (fun x -> (x[0],((int x[1])/hxlAreaX,x[2]))) 
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
        |> Array.Parallel.map(fun x -> x[0],[|x[1]|])
    
    let spcKy03 = 
        spcKy01 
        |> Array.tail 
        |> Array.partition (fun (x,_) -> x.Length = 1)
    
    let spcKy04 = 
        (Array.append spcKy02 (fst spcKy03)) 
        |> Array.groupBy (fun (x,_) -> x)
        |> Array.Parallel.map (fun x -> snd x)
        |> Array.Parallel.map (fun x 
                                -> (Array.Parallel.map(fun (y,z)
                                                        -> Array.append[|y|] z))x)
        |> Array.Parallel.map (fun x -> Array.concat x)
        |> Array.Parallel.map (fun x -> Array.distinct x)
        |> Array.Parallel.map (fun x -> Array.sort x)
    
    let spcKy05 = 
        (snd spcKy03)
        |> Array.Parallel.map (fun (x,y) 
                                -> Array.append [|x|] y)
        |> Array.append spcKy04
        |> Array.sortBy (fun x -> Array.head x)
    let spcKy06 = 
        let a = match (Array.isEmpty spcKy05) with 
                |  true -> [|[|"1"|]|]
                | false -> spcKy05
        a
        |> Array.Parallel.map(fun x 
                                    -> (Array.Parallel.map (fun y 
                                                                -> y, spcMp6 
                                                                |> Map.find y))x)
    let spcKey =
        spcKy06
        |> Array.Parallel.map (fun z 
                                -> (Array.Parallel.map (fun (x,y) 
                                                            -> x, max hxlAreaX (fst y), snd y))z)
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
let getNtArea (bdOu: (int*int)[]) (bdIs: (int*int)[][]) = 
    polygonWithHolesArea bdOu bdIs

/// Tree Mapping (Dataset Generation Version)
let getTree01 (ntArea: float) (cxlCnt: float) (spcTree: (string * int * string) array array) =
    let bdPr = if cxlCnt > 0.0 then (ntArea / cxlCnt) / 4.0 else 1.0
    spcTree |> Array.map (fun row -> 
        row |> Array.map (fun (id, cnt, lb) -> 
            (Refid id, Count (int (float cnt * bdPr)), Label lb)))

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
        let chBs = if (Array.length chHx) >= cnt then 
                       let divs = (Array.length chHx) / cnt 
                       Array.chunkBySize divs chHx |> Array.map Array.head |> Array.take cnt
                   else Array.append chHx (Array.replicate (cnt - Array.length chHx) (identity elv))
        
        let cxc1 = coxel seq elv (Array.map2 (fun a (_, c, d) -> a, hostId, c, d) chBs (Array.tail currentBatch)) newOcc
        let chOc1 = hxlUni 2 (Array.append newOcc (Array.concat (cxc1 |> Array.map (fun x -> x.Hxls))))
        let cxc2 = cxc1 |> Array.map (fun x -> { x with Hxls = hxlChk seq elv chOc1 x.Hxls; Base = hxlChk seq elv chOc1 [|x.Base|] |> Array.head })
        
        cxCxCx seq elv nextTre newOcc (Array.append acc cxc2)
///

let hxlAreaFactor = 4.0

/// Consolidates all reproportioning logic into one call.
let applyReproportioning (attributes: Map<string, string>) (ntArea: float) (rawTree: (string * int * string)[][]) =
    let hxlAreaFactor = 4.0
    
    // 1. Calculate requested totals
    let flatTree = rawTree |> Array.concat
    let totalRequested = flatTree |> Array.sumBy (fun (_, cnt, _) -> cnt) |> float
    
    // 2. Identify Unbound State
    // If area is 0 or Outer attribute is missing, we are unbound.
    let isUnbound = ntArea <= 0.0 || not (attributes.ContainsKey "O")

    // 3. Determine Scaling Ratio
    let ratio = 
        if isUnbound then 1.0
        else
            match attributes |> Map.tryFind "X" with
            | Some "0" -> if totalRequested > 0.0 then (ntArea / totalRequested) / hxlAreaFactor else 1.0
            | Some a -> match Double.TryParse a with | true, v -> v | _ -> 1.0
            | None -> 1.0

    // 4. Target hexel count
    let targetTotal = 
        if isUnbound then int totalRequested 
        else int (ntArea / hxlAreaFactor)
        
    // 5. Calculate floors and remainders
    let initialData = 
        flatTree |> Array.map (fun (id, cnt, lb) ->
            let ideal = float cnt * ratio
            // Ensure every space has at least 1 hexel if it was requested
            let flr = if cnt > 0 then max 1 (int (floor ideal)) else 0
            {| Id = id; Label = lb; Ideal = ideal; Floor = flr; Remainder = ideal - float flr |})

    // 6. Distribute difference (The Largest Remainder Method)
    let currentSum = initialData |> Array.sumBy (fun x -> x.Floor)
    let diff = targetTotal - currentSum

    let distributedCounts =
        if diff > 0 && not isUnbound then
            let bonusIndices = 
                initialData 
                |> Array.mapi (fun i x -> i, x.Remainder)
                |> Array.sortByDescending snd
                |> Array.truncate diff
                |> Array.map fst |> Set.ofArray
            initialData |> Array.mapi (fun i x -> if bonusIndices.Contains i then x.Floor + 1 else x.Floor)
        else
            initialData |> Array.map (fun x -> x.Floor)

    // 7. Reconstruct the nested array structure (Fixes the 'int' compatibility error)
    let mutable pointer = 0
    rawTree |> Array.map (fun row ->
        row |> Array.map (fun (id, _, lb) ->
            let finalCount = distributedCounts.[pointer]
            pointer <- pointer + 1
            Refid id, Count finalCount, Label lb))
///

let generateCoreLayout 
    (str: string) 
    (seqOverride: Sqn option) 
    (ouStrOverride: string option) 
    (ilStrOverride: string option) 
    (initialOcc : Hxl[]) : Cxl[] * (int*int)[][] =
    
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
        | Some ou when ou <> "" -> parsePolygon ou |> Array.map (fun (x,y) -> int x, int y)
        | _ ->
            match spcAt1 |> Map.tryFind "O" with 
            | Some a -> parsePolygon a |> Array.map (fun (x,y) -> int x, int y)
            | None ->
                match bdWd > 0 with 
                | true -> 
                    let a = $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                    parsePolygon a |> Array.map (fun (x,y) -> int x, int y)
                | false -> [||]

    // 6. Island Boundary
    let bdIs =
        match Array.isEmpty bdOu with
        | true -> [||]
        | false ->
            match ilStrOverride with
            | Some il when il <> "" -> parsePolyIslands il |> Array.map (fun seg -> seg |> Array.map (fun (x,y) -> int x, int y))
            | _ ->
                match spcAt1 |> Map.tryFind "I" with 
                | Some a -> parsePolyIslands a |> Array.map (fun seg -> seg |> Array.map (fun (x,y) -> int x, int y))
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

    // 8. Site Net Area
    let ntArea = polygonWithHolesArea bdOu bdIs

    // 9. Occupancy
    let ouHx = match bdWd = 0 with | true -> [||] | false -> hxlPgn seq elv bdOu 
    let ilHx = match bdWd = 0 with | true -> [||] | false -> bdIs |> Array.collect (hxlPgn seq elv)
    let occ = 
        let list = System.Collections.Generic.List<Hxl>(initialOcc.Length + ouHx.Length + ilHx.Length)
        list.AddRange(initialOcc)
        list.AddRange(ouHx)
        list.AddRange(ilHx)
        list.ToArray()

    // 10. Parse Space String
    let tree01 = applyReproportioning spcAt1 ntArea spcTree
    let flatEntries = tree01 |> Array.concat

    // 11. Core Recursive Logic
    let result =
        match Array.tryHead flatEntries with
        | None -> [||]
        | Some (id, ct, lb) ->
            let cti = match ct with | Count x when x > 0 -> Count (x - 1) | _ -> Count 0
            let firstPrpId = tree01 |> Array.concat |> Array.head |> fun (i,_,_) -> i
            let firstPrpLb = tree01 |> Array.concat |> Array.head |> fun (_,_,l) -> l
            let ac0 = coxel seq elv [| bsHx, firstPrpId, cti, firstPrpLb |] occ

            match Array.tryHead ac0 with
            | None -> [||]
            | Some firstCxl ->
                let ac1 = [| { firstCxl with Hxls = Array.except occ (Array.append [| firstCxl.Base |] firstCxl.Hxls) } |]
                
                let globalOcc = System.Collections.Generic.List<Hxl>()
                globalOcc.AddRange(occ)
                globalOcc.Add(bsHx)
                globalOcc.AddRange(ac1.[0].Hxls)
                
                let cxlCxl (tre : (Prp*Prp*Prp)[]) (currentAcc: Cxl[]) = 
                    let targetId = tre |> Array.map (fun (a,_,_) -> a) |> Array.head
                    let bsCx = currentAcc |> Array.find (fun x -> x.Rfid = targetId)
                    let chHx = bsCx.Hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
                    let cnt = (Array.length tre) - 1
                    let chBs = 
                        match (Array.length chHx) >= cnt with 
                        | true -> 
                            let divs = match cnt > 0 with | true -> (Array.length chHx) / cnt | false -> 1
                            chHx |> Array.chunkBySize divs |> Array.map Array.head |> Array.take cnt
                        | false -> 
                            Array.append chHx (Array.replicate (max 0 (cnt - (Array.length chHx))) (identity elv))
                    
                    let cxc1 = coxel seq elv (Array.map2 (fun a (b, c, d) -> a,b,c,d) chBs (Array.tail tre)) (globalOcc.ToArray())
                    
                    let localOcc = System.Collections.Generic.List<Hxl>()
                    localOcc.AddRange(globalOcc)
                    for cx in cxc1 do localOcc.AddRange(cx.Hxls)
                    let chOc1 = hxlUni 2 (localOcc.ToArray())
                    
                    cxc1 |> Array.map (fun x -> 
                        let h2 = hxlChk seq elv chOc1 x.Hxls
                        let b2 = hxlChk seq elv chOc1 [|x.Base|] |> Array.tryHead |> Option.defaultValue x.Base
                        { x with Hxls = h2; Base = b2 })
                
                let finalAcc = System.Collections.Generic.List<Cxl>()
                finalAcc.AddRange(ac1)

                let rec cxCxCx (tre : (Prp*Prp*Prp)[][]) =
                    match Array.tryHead tre with 
                    | Some a -> 
                        let newCxls = cxlCxl a (finalAcc.ToArray())
                        finalAcc.AddRange(newCxls)
                        for cx in newCxls do globalOcc.AddRange(cx.Hxls)
                        cxCxCx (Array.tail tre)
                    | None -> ()

                if (Array.length flatEntries < 2) then ()
                else cxCxCx tree01

                finalAcc.ToArray()

    result, Array.append [|bdOu|] bdIs
///

/// <summary> Generate coxels based on string data. </summary>
/// <param name="seq"> Sequence. </param>
/// <param name="bas"> Base hexel. </param>
/// <param name="occ"> Unavailable hexels. </param>
/// <returns> Coxel array </returns>    
let spaceCxl (occ : Hxl[]) (str : string) = 
    generateCoreLayout str None None None occ

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

    let finalBatch, _ = generateCoreLayout str (Some seq) (Some ouStr) (Some ilStr) initialOcc

    let sb = System.Text.StringBuilder()
    finalBatch |> Array.iteri (fun j cxl ->
        match j > 0 with | true -> sb.Append(";") |> ignore | false -> ()
        cxl.Hxls |> Array.iteri (fun k h ->
            let (ax, ay, _) = hxlCrd h
            match k > 0 with | true -> sb.Append(" ") |> ignore | false -> ()
            sb.Append(toBase36 (int64 ax)).Append(".").Append(toBase36 (int64 ay)) |> ignore
        )
    )
    sb.ToString()
