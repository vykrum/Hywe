module Coxel

open Hexel
///

/// <summary> Coxels are primarily a collections of unique hexels </summary>
/// <summary> Coxel type properties </summary>
/// <typeparam name="Label"> Name </typeparam>
/// <typeparam name="Refid"> Unique reference ID </typeparam>
/// <typeparam name="Count"> ANumber of hexels in Coxel </typeparam>
type Prp = 
    | Label of string
    | Refid of string
    | Count of int
///

/// <summary> Coxel type consists of hexels and properties. </summary>
/// <typeparam name="Name"> Coxel Name. </typeparam>
/// <typeparam name="Rfid"> Reference ID. </typeparam>
/// <typeparam name="Size"> Number of hexels. </typeparam>
/// <typeparam name="Seqn"> Sequence of hexel arrangement. </typeparam>
/// <typeparam name="Base"> Base hexel. </typeparam>
/// <typeparam name="Hxls"> Constituent Hexels. </typeparam>
type Cxl = 
    {
        Name : Prp
        Rfid : Prp
        Size : Prp
        Seqn : Sqn
        Base : Hxl
        Hxls : Hxl[]
    }  
///

/// <summary> Property value types </summary>
/// <typeparam name="Label">  Name of coxel. </typeparam>
/// <typeparam name="Refid">  Reference ID. </typeparam>
/// <typeparam name="Count">  Number of hexels as a string. </typeparam>
let prpVlu 
    (prp : Prp) = 
    match prp with 
    | Label prp -> prp
    | Refid prp -> prp
    | Count prp -> prp.ToString()
///

/// <summary> Creating an array of coxels. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="ini"> An array of tuples containing base hexel, Reference Id, Count/Size, Label. </param>
/// <param name="occ"> Hexels that are unavailable. </param>
/// <returns> An array of coxels. </returns>
let coxel 
    (sqn : Sqn)
    (ini : (Hxl*Prp*Prp*Prp)[])
    (occ : Hxl[]) = 
        
    let bas = Array.map(fun (x,_,y,_) -> x,int(prpVlu y)) ini
    let szn = Array.map(fun (_,_,y,z) -> y,z) ini
    let idn = Array.map (fun(x,y,_,_)->x,y) ini

    let cnt = 
            bas
            |> Array.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 bas
    let occ = (Array.append occ (getHxls bas)) |> allAV false 
        
    let rec clsts 
        (hxo: (Hxl*int)[])
        (occ : Hxl[])
        (acc:(Hxl*int)[][])
        (cnt : int) = 
            
        match cnt with 
        | c when c < 0x1 -> acc
        | _ -> 
                let occ = 
                    acc 
                    |> Array.concat 
                    |> getHxls
                    |> Array.append occ
                    |> Array.append (getHxls hxo)
                    |> Array.append [|identity|]
                    |> Array.distinct
                    |> allAV false

                let rpt = Array.map (fun x 
                                        -> (snd x) - 0x1) hxo
                let Hxl =  
                    acc
                    |> Array.map (fun x
                                    -> Array.filter (fun a 
                                                        -> (available sqn a occ) > 0x0) x)
                    |> Array.map (fun x 
                                    -> Array.tryHead x)
                    |> Array.map (fun x 
                                    -> match x with
                                        | Some a -> a 
                                        | None -> (identity,0xFFFFFFFF))                
                    |> Array.map2 (fun x y 
                                    -> fst y, x) rpt
                    
                let inc = increments sqn Hxl occ
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)

                let occ = Array.concat[|getHxls (Array.concat [|Array.concat acc; inc;Hxl|]);occ|] |> allAV false

                (clsts Hxl occ acc (cnt - 0x1))


    let cls = 
        clsts bas occ acc cnt
            |> Array.map(fun x 
                            -> Array.filter(fun (_,z) -> z >= 0) x)

    let cl1 = 
        cls
        |> Array.map(fun x -> getHxls x)
        
    let cxl = Array.map3 (fun x y z -> 
                                            {
                                                Name = snd x
                                                Rfid = snd y
                                                Size = fst x
                                                Seqn = sqn
                                                Base = fst y
                                                Hxls = z
                                            })szn idn cl1
    cxl
///

/// <summary> Categorize constituent Hexels within a Coxel. </summary>
/// <param name="cxl"> A coxel. </param>
/// <param name="occ"> Hexels that are unavailable. </param>
/// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
let cxlHxl
    (cxl : Cxl)     
    (occ : Hxl[]) = 

    /// Bounding Hexels
    let cl1 = cxl.Hxls
    let cl2 = match ((available cxl.Seqn cxl.Base cl1) > 0) with
                    | false -> cl1
                    | true -> Array.tail cl1
    let cl3 = cl2 |> Array.Parallel.partition
                (fun x-> (available cxl.Seqn x cl2) > 0) 
    let bd1 = cl3 |> fst |> bndSqn cxl.Seqn
        
    /// Core Hexels
    let cr1 = snd cl3
        
    let oc1 = Array.concat
                [|
                    occ 
                    cxl.Hxls
                |] |> allAV false
        
    let cl4 = bd1 |> Array.Parallel.partition
                (fun x-> (available cxl.Seqn x oc1) > 0)
        
    /// Available Hexels
    let av1= cl4 |> fst |> cntSqn cxl.Seqn
        
    /// Border Hexels
    let br1= snd cl4
 
    {|
        Base = cxl.Base
        Hxls = cl1
        Core = cr1
        Prph = bd1
        Brdr = br1
        Avbl = av1
    |}    