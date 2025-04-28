module Parse

open Hexel
open Coxel
open Shape
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
let spaceSeq 
    (spaceStr:string) = 
    let spcMp1 = ((spaceStr.Replace ("\n",""))
                    .Replace("\t","")
                    .Replace(" ",""))
                    .Split ","
                    |> Array.Parallel.map(fun x -> x.Remove(0,1)) 
                    |> Array.Parallel.map(fun x -> x.Remove(x.Length-1,1))
                    |> Array.Parallel.map (fun x -> x.Split "/")
    let spcMp2 = match ((spcMp1 |> Array.head |> Array.head) = "0") with
                    | true -> spcMp1
                    | false -> Array.append [|[|"0";"W=0";"H=0";"I=0";"S=0";"Q=22"|]|] spcMp1   
    let spcAt1 = spcMp2 
                |> Array.head 
                |> Array.tail
                |> Array.Parallel.map (fun x -> x.Split("="))
                |> Array.Parallel.map (fun x -> x[0],x[1])
                |> Map.ofArray

    // Reproportion count based on Boundary Extent
    let bdPr = match (spcAt1 |> Map.tryFind "S") with 
                | Some a -> (a |> double)
                | None -> 1.0
    let bdWd = match (spcAt1 |> Map.tryFind "W") with 
                | Some a -> (a |> int)
                | None -> 0
    let bdHt = match (spcAt1 |> Map.tryFind "H") with 
                | Some a -> (a |> int)
                | None -> 0


    let spcCt1 = spcMp2 |> Array.tail |> Array.map(fun x -> x[1] |> int)
    let spcPr1 = match (bdWd=0 || bdHt=0) with 
                    | true -> match (bdPr=0.0) with 
                                | true -> 1.0
                                | false -> bdPr
                    | false -> match (bdPr=0.0) with
                                | true -> 1.0
                                | false -> ((bdWd * bdHt)|> double)/((Array.sum spcCt1)|> double)*bdPr
    let spcCt2 = Array.Parallel.map (fun a -> (Math.Ceiling((a|>double)*spcPr1)|>string)) spcCt1
    let spcMp3 = spcMp2 |> Array.tail
    let spcMp4 = Array.map2 (fun x y -> Array.set x 1 y) spcMp3 spcCt2
    let spcMp5 = Array.append [|spcMp2 |> Array.head|] spcMp3
    let spcMp6 = spcMp5 
                |> Array.tail
                |> Array.Parallel.map (fun x -> (x[0],(int x[1],x[2]))) 
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
                                                         -> x, fst y, snd y))z)
    spcAt1,spcKey
///    

/// <summary> Generate coxels based on string data. </summary>
/// <param name="seq"> Sequence. </param>
/// <param name="bas"> Base hexel. </param>
/// <param name="occ"> Unavailable hexels. </param>
/// <returns> Coxel array </returns>    
[<TailCall>]
let spaceCxl
    (occ : Hxl[])
    (str : string) = 
    (*         
    let avlReq 
        (tr01 : (Prp*Prp*Prp)[][]) = 
        let chlMap = 
            tr01
            |> Array.map(fun x -> Array.head x,Array.length x)
            |> Array.map(fun ((a,_,_),b) ->  a, b-1) 
            |> Map.ofArray
            
        let chdCnt = 
            tr01
            |> Array.map(fun x -> 
                Array.map(fun (a,_,_) 
                            -> Map.tryFind a chlMap)x)
            |> Array.map (fun x 
                            -> Array.map(fun y 
                                            -> match y with 
                                                | Some y -> y
                                                | None -> 0)x)
        chdCnt
    *)
    let tree01 = 
        spaceSeq str
            |> snd
            |> Array.Parallel.map (fun x -> 

                Array.Parallel.map(fun (a,b,c) 
                                    -> Refid a, Count (b), Label c)x)
    
    // Attributes
    let spcAt1 = fst (spaceSeq str)
    // Attribute Q for Sequence
    let seq = match (spcAt1 |> Map.tryFind "Q") with 
                | Some a -> match a with 
                            | "1" -> VRCWEE
                            | "11" -> HRCWNN
                            | "111" -> VRCCEE
                            | "1111" -> HRCCNN
                            | "2" -> VRCWSE
                            | "22" -> HRCWNE
                            | "222" -> VRCCSE
                            | "2222" -> HRCCNE
                            | "3" -> VRCWSW
                            | "33" -> HRCWSE
                            | "333" -> VRCCSW
                            | "3333" -> HRCCSE
                            | "4" -> VRCWWW
                            | "44" -> HRCWSS
                            | "444" -> VRCCWW
                            | "4444" -> HRCCSS
                            | "5" -> VRCWNW
                            | "55" -> HRCWSW
                            | "555" -> VRCCNW
                            | "5555" -> HRCCSW
                            | "6" -> VRCWNE
                            | "66" -> HRCWNW
                            | "666" -> VRCCNE
                            | "6666" -> HRCCNW
                            | _ -> VRCWEE
                | None -> HRCWNE

    // Rectangular Boundary
    // Attribute W for Width
    let bdWd = match (spcAt1 |> Map.tryFind "W") with 
                | Some a -> (a |> int)
                | None -> 0
    // Attribute H for Height
    let bdHt = match (spcAt1 |> Map.tryFind "H") with 
                | Some a -> (a |> int)
                | None -> 0
    // Attribute I for Initial Base
    let boI1 = match (spcAt1 |> Map.tryFind "I") with 
                | Some a -> (a |> int)
                | None -> 0
    
    let bdR1 = match (bdWd=0 || bdHt=0) with 
                | true -> [||]
                | false -> [|hxlRct seq (bdWd) (bdHt) boI1|]      
    let bdRt = match (bdWd=0 || bdHt=0) with 
                | true -> [||]
                | false -> snd(Array.head bdR1)
    let bsHx = match (bdWd=0 || bdHt=0) with 
                | true -> AV(0,0,0)
                | false -> fst (Array.head bdR1)
    
    let occ = Array.concat [|occ;bdRt|]

    // Generate base coxel
    let id,ct,lb = tree01 |> Array.concat |> Array.head
    let cti  = match ct with 
                | Count x when x>0 -> Count (x-1) 
                | _ -> Count 0       
    let ac0 = match cti with 
                | Count a when a < 1 -> coxel seq ([|identity, id, cti, lb|]) occ
                | _ -> coxel seq ([|bsHx, id, cti, lb|]) occ
    let ac1 = [|{ac0[0] with Hxls = Array.except occ (Array.append [|ac0[0].Base|] ac0[0].Hxls)}|]
    let oc1 = (Array.concat [|occ; [|bsHx|]; (Array.head ac1).Hxls|])

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
                                (Array.replicate (cnt - (Array.length chHx)) identity)
        let chPr = Array.tail tre
        let cxc1 = coxel 
                    seq
                    (Array.map2 (fun a (b, c, d) -> a,b,c,d) chBs chPr)
                    occ
        // Reassigning Hexel types
        let chHx1 = Array.Parallel.map (fun x -> x.Hxls) cxc1
        let chOc1 = hxlUni 2 (Array.append occ (Array.concat chHx1))
        let chHx2 = Array.Parallel.map (fun x -> hxlChk seq chOc1 x) chHx1
        let chHx3 = hxlChk seq chOc1 (Array.map (fun x -> x.Base) cxc1)
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
///
