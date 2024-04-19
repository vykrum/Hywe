module Parse

open Hexel
open Coxel

// Sample Space Program Input Format
let spaceStr =
     "(1/15/Foyer),(2/20/Living),(3/20/Dining),
    (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"
///

/// <summary> Categorize constituent Hexels within a Coxel. </summary>
/// <param name="spaceStr"> Properly formatted string (RefId,Count,Lablel) </param>
/// <returns> Array of string arrays (RefId as string * Count as int * Label as string)
let spaceSeq 
    (spaceStr:string) = 
        
    let spaceMap = 
        ((spaceStr.Replace ("\n",""))
            .Replace("\t","")
            .Replace(" ",""))
            .Split ","
            |> Array.map(fun x -> x.Remove(0,1)) 
            |> Array.map(fun x -> x.Remove(x.Length-1,1))
            |> Array.map (fun x -> x.Split "/") 
            |> Array.map (fun x -> (x[0],(int x[1],x[2]))) 
            |> Array.sortBy (fun (x,y) -> x)
            |> Map.ofArray

    let spcKy01 = 
        spaceMap 
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
        |> Array.partition (fun (x,y) -> x.Length = 1)

    let spcKy04 = 
        (Array.append spcKy02 (fst spcKy03)) 
        |> Array.groupBy (fun (x,y) -> x)
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
        spcKy05 
        |> Array.map(fun x 
                        -> (Array.map (fun y 
                                        -> y, spaceMap 
                                        |> Map.find y))x)
        
    let spcKey =
        spcKy06
        |> Array.map (fun z 
                        -> (Array.map (fun (x,y) 
                                        -> x, fst y, snd y))z)
    spcKey    
///

/// <summary> Generate coxels based on string data. </summary>
/// <param name="seq"> Sequence. </param>
/// <param name="bas"> Base hexel. </param>
/// <param name="occ"> Unavailable hexels. </param>
/// <returns> Coxel array </returns>
let spaceCxl 
    (seq : Sqn)
    (bas : Hxl)
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
    let bas = hxlVld seq bas
    let tree01 = 
        spaceSeq str 
            |> Array.map (fun x -> 
                Array.map(fun (a,b,c) 
                            -> Refid a, Count b, Label c)x)

    // Generate base coxel
    let id,ct,lb = tree01 |> Array.concat |> Array.head
    let cti  = match ct with 
                | Count x when x>0 -> Count (x-1) 
                | _ -> Count 0 
    let ac0 = match cti with 
                | Count a when a < 1 -> coxel seq ([|identity, id, cti, lb|]) occ
                | _ -> coxel seq ([|bas, id, cti, lb|]) occ
    let ac1 = [|{ac0[0] with Hxls = Array.except occ (Array.append [|ac0[0].Base|] ac0[0].Hxls)}|]
    let oc1 = (Array.concat [|occ; [|bas|]; (Array.head ac1).Hxls|])

    let cxlCxl 
        (seq : Sqn)
        (tre : (Prp*Prp*Prp)[])
        (occ : Hxl[])
        (acc : Cxl[]) = 
        let bsCx = 
                    acc 
                    |> Array.map(fun x -> x.Rfid,x) 
                    |> Map.ofArray
                    |> Map.find (tre |> Array.map (fun (a,_,_) -> a) |> Array.head)
                        
        let chHx = bsCx.Hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
        let cnt = (Array.length tre) - 1
        let chBs = match (Array.length chHx) >= cnt with 
                    | true -> Array.take cnt chHx
                    | false -> Array.append 
                                chHx 
                                (Array.replicate (cnt - (Array.length chHx)) identity)
        let chPr = Array.tail tre
        let cxc1 = coxel 
                    seq
                    (Array.map2 (fun a (b, c, d) -> a,b,c,d) chBs chPr)
                    occ
        // Reassigning Hexel types
        let chHx1 = Array.map (fun x -> x.Hxls) cxc1
        let chOc1 = allAV true (Array.append occ (Array.concat chHx1))
        let chHx2 = Array.map (fun x -> hxlTyp seq chOc1 x) chHx1
        let chHx3 = hxlTyp seq chOc1 (Array.map (fun x -> x.Base) cxc1)
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
                            let occ = Array.append occ (Array.concat (Array.map(fun x -> x.Hxls)acc))
                            let tre = Array.tail tre
                            let acc = Array.append 
                                        acc 
                                        (cxlCxl seq a occ acc)
                            cxCxCx seq tre occ acc
                    | None -> acc
        a

    cxCxCx seq tree01 oc1 ac1