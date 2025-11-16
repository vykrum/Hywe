module Parse

open Hexel
open Coxel
open Geometry
open System

// Sample Space Program Input Format
let spaceStr =
     "(1/15/Foyer),(2/20/Living),(3/20/Dining),
    (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"
///

/// <summary> Categorize constituent Hexels within a Coxel. 
///</summary>
/// <param name="spaceStr"> Properly formatted string (RefId,Count,Lablel) </param>
/// <returns> Array of string arrays (RefId as string * Count as int * Label as string)  </returns>
/// <summary>
/// Categorize constituent Hexels within a Coxel (safe + pattern matching version)
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

/// <summary> Generate coxels based on string data. </summary>
/// <param name="seq"> Sequence. </param>
/// <param name="bas"> Base hexel. </param>
/// <param name="occ"> Unavailable hexels. </param>
/// <returns> Coxel array </returns>    
let spaceCxl
    (occ : Hxl[])
    (str : string) = 
    
    // Attributes
    let spcAt1 = fst (spaceSeq str)
    // Attribute Q for Sequence
    let seq = match spcAt1 |> Map.tryFind "Q" with 
                | Some a -> match a with 
                            | "VRCWEE" -> VRCWEE
                            | "VRCCEE" -> VRCCEE
                            | "VRCWSE" -> VRCWSE
                            | "VRCCSE" -> VRCCSE
                            | "VRCWSW" -> VRCWSW
                            | "VRCCSW" -> VRCCSW
                            | "VRCWWW" -> VRCWWW
                            | "VRCCWW" -> VRCCWW
                            | "VRCWNW" -> VRCWNW
                            | "VRCCNW" -> VRCCNW
                            | "VRCWNE" -> VRCWNE
                            | "VRCCNE" -> VRCCNE
                            | "HRCWNN" -> HRCWNN
                            | "HRCCNN" -> HRCCNN
                            | "HRCWNE" -> HRCWNE
                            | "HRCCNE" -> HRCCNE
                            | "HRCWSE" -> HRCWSE
                            | "HRCCSE" -> HRCCSE
                            | "HRCWSS" -> HRCWSS
                            | "HRCCSS" -> HRCCSS
                            | "HRCWSW" -> HRCWSW
                            | "HRCCSW" -> HRCCSW
                            | "HRCWNW" -> HRCWNW
                            | "HRCCNW" -> HRCCNW
                            | _        -> VRCWEE
                | None -> VRCWEE

    // Attribute L for Elevation
    let elv = match spcAt1 |> Map.tryFind "L" with 
                | Some a -> a |> int
                | None -> 0

    // Attribute W for Width
    let bdWd = match spcAt1 |> Map.tryFind "W" with 
                | Some a -> match a |> int > 0 with
                            | true -> a |> int
                            | false -> 0
                | None -> 0

    // Attribute H for Height
    let bdHt = match spcAt1 |> Map.tryFind "H" with 
                | Some a -> match bdWd > 0 with 
                            | true -> a |> int
                            | false -> bdWd
                    | None -> bdWd

    let parsePolygonString (s: string) : (int * int)[][] =
        let parseSegment (segment: string) =
            segment.Split(',', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun x -> x.Trim())
            |> Array.choose (fun x ->
                match System.Int32.TryParse(x) with
                | true, v -> Some v
                | false, _ -> None
            )
            |> fun numbers ->
                match numbers with
                | [||] -> [||]
                | ns when ns.Length % 2 <> 0 -> [||]
                | ns ->
                    ns
                    |> Array.chunkBySize 2
                    |> Array.map (function
                        | [|a; b|] -> a, b
                        | _ -> failwith "Unexpected chunk length"
                    )

        match String.IsNullOrWhiteSpace(s) with
        | true -> [||]
        | false ->
            s.Split('-', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parseSegment

    // Attribute O for Outer Boundary Vertices
    let bdOu = match spcAt1 |> Map.tryFind "O" with 
                | Some a -> Array.concat (parsePolygonString a)
                | None -> match bdWd = 0 with 
                            | true -> [||]
                            | false -> 
                                    let b = $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                                    Array.concat (parsePolygonString b)

    // Attribute I for Island Boundary Vertices
    let bdIs = match spcAt1 |> Map.tryFind "I" with 
                | Some a -> parsePolygonString a
                | None -> [||]

    // Attribute E for Entry Hexel
    let bsHx =
        match spcAt1 |> Map.tryFind "E" with
        | Some a ->
            let parts = a.Split ',' |> Array.choose (fun s ->
                match System.Int32.TryParse(s.Trim()) with
                | true, v -> Some v
                | _ -> None
            )
            match parts with
            | [| x; y |] -> hxlLin seq elv (identity elv) (AV(x, y, elv))
                            |> hxlUni 1
                            |> Array.last
            | _ -> identity elv
        | None ->
            // Use an AV base when width is zero so adjacency/increment works.
            match bdWd = 0 with
            | true -> AV(0, 0, elv)
            | false ->  hxlLin seq elv (identity elv) (AV(bdWd/2+2, bdHt/2+2, elv))
                        |> hxlUni 1
                        |> Array.last
    
    // Total Count
    let cxlCnt = spaceSeq str 
                |> snd 
                |> Array.concat
                |> Array.Parallel.map (fun (_,x,_) -> x)
                |> Array.sum |> float

    // Site Net Area
    let ntArea = polygonWithHolesArea bdOu bdIs

    // Attribute X for Count Proportion
    let bdPr = match spcAt1 |> Map.tryFind "X" with 
                | Some a -> match a with
                            | "0" -> match cxlCnt > 0 with 
                                        | true -> (ntArea / cxlCnt) / 4.0
                                        | false -> 1.0
                            | _ -> a |> float
                | None -> 1.0

    // Outer Hexels 
    let ouHx = match bdWd = 0 with 
                | true -> [||]
                | false -> hxlPgn seq elv bdOu 

    // Island Hexels
    let ilHx = match bdWd = 0 with 
                | true -> [||]
                | false -> bdIs |> Array.map (fun x -> hxlPgn seq elv x )
        
    let occ = Array.concat [|occ;ouHx;Array.concat ilHx|]

    // Parse Space String
    let tree01 = 
        spaceSeq str
        |> snd
        |> Array.Parallel.map (fun x -> Array.Parallel.map (fun (a,b,c) -> Refid a, Count (int (float b * bdPr)), Label c) x)

    // Flatten and guard against empty parse results
    let flatEntries = tree01 |> Array.concat

    let result =
        match Array.tryHead flatEntries with
        | None ->
            [||]
        | Some (id, ct, lb) ->
            let cti =
                match ct with
                | Count x when x > 0 -> Count (x - 1)
                | _ -> Count 0
            let ac0 =
                match cti with
                | Count a when a < 1 -> coxel seq elv ([| bsHx, (tree01 |> Array.concat |> Array.head |> fun (id,_,_) -> id), cti, (tree01 |> Array.concat |> Array.head |> fun (_,_,lb) -> lb) |]) occ
                | _ -> coxel seq elv ([| bsHx, (tree01 |> Array.concat |> Array.head |> fun (id,_,_) -> id), cti, (tree01 |> Array.concat |> Array.head |> fun (_,_,lb) -> lb) |]) occ
            let ac1 = [| { ac0[0] with Hxls = Array.except occ (Array.append [| ac0[0].Base |] ac0[0].Hxls) } |]
            let oc1 = Array.concat [| occ; [| bsHx |]; (Array.head ac1).Hxls |]

            let cxlCxl 
                (seq : Sqn)
                (tre : (Prp*Prp*Prp)[])
                (occ : Hxl[])
                (acc : Cxl[]) = 
                let bsCx = 
                            acc 
                            |> Array.Parallel.map(fun x -> x.Rfid,x) 
                            |> Map.ofArray
                            |> Map.find (tre |> Array.Parallel.map (fun (a,_,_) -> a) |> Array.head)
                                
                // Available Hexels
                let chHx = bsCx.Hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
                // Required host Hexel count
                let cnt = (Array.length tre) - 1
                // Seperated host hexels
                let chBs = match (Array.length chHx) >= cnt with 
                            | true -> 
                                        let divs =  ((Array.length chHx) / cnt)
                                        let chnk = Array.chunkBySize divs chHx
                                        let fsHx = chnk |> Array.Parallel.map (fun x -> Array.head x)
                                        Array.take cnt fsHx
                            | false -> Array.append 
                                        chHx 
                                        (Array.replicate (cnt - (Array.length chHx)) (identity elv))
                let chPr = Array.tail tre
                let cxc1 = coxel 
                            seq
                            elv
                            (Array.map2 (fun a (b, c, d) -> a,b,c,d) chBs chPr)
                            occ
                // Reassigning Hexel types
                let chHx1 = Array.Parallel.map (fun x -> x.Hxls) cxc1
                let chOc1 = hxlUni 2 (Array.append occ (Array.concat chHx1))
                let chHx2 = Array.Parallel.map (fun x -> hxlChk seq elv chOc1 x) chHx1
                let chHx3 = hxlChk seq elv chOc1 (Array.map (fun x -> x.Base) cxc1)
                let cxc2 = Array.map3 (fun x y z -> {x with Cxl.Hxls = y; Cxl.Base = z}) cxc1 chHx2 chHx3
                cxc2
                
            let rec cxCxCx
                (seq : Sqn)
                (tre : (Prp*Prp*Prp)[][])
                (occ : Hxl[])
                (acc : Cxl[]) =
                    
                let a = match Array.tryHead tre with 
                            | Some a 
                                -> 
                                    let occ = Array.append occ (Array.concat (Array.Parallel.map(fun x -> x.Hxls)acc))
                                    let tre = Array.tail tre
                                    let acc = Array.append 
                                                acc 
                                                (cxlCxl seq a occ acc)
                                    cxCxCx seq tre occ acc
                            | None -> acc
                a

            match (Array.length (Array.concat tree01) < 2) with 
            | true -> ac1
            | false -> cxCxCx seq tree01 oc1 ac1

    result,Array.append [|bdOu|] bdIs
