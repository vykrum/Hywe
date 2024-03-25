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
        
    let bas = Array.Parallel.map(fun (x,_,y,_) -> x,int(prpVlu y)) ini
    let szn = Array.Parallel.map(fun (_,_,y,z) -> y,z) ini
    let idn = Array.Parallel.map (fun(x,y,_,_)->x,y) ini

    let cnt = 
            bas
            |> Array.Parallel.map (fun x -> snd x)
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

                let rpt = Array.Parallel.map (fun x 
                                                -> (snd x) - 0x1) hxo
                let Hxl =  
                    acc
                    |> Array.Parallel.map (fun x
                                            -> Array.filter (fun a 
                                                                        -> (available sqn a occ) > 0x0) x)
                    |> Array.Parallel.map (fun x 
                                            -> Array.tryHead x)
                    |> Array.Parallel.map (fun x 
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
            |> Array.Parallel.map(fun x 
                                    -> Array.filter(fun (_,z) -> z >= 0) x)
    
    // Avoid single unclustered cell towards the end
    let cl00 = 
        cls
        |> Array.Parallel.map(fun x -> getHxls x)
    let cl01 = 
        cl00
        |> Array.Parallel.map(fun x -> Array.tail x)
        |> Array.Parallel.map(fun x -> Array.filter(fun y -> (available sqn y x) < 5)x)
    let cl1 = Array.map2 (fun x y -> Array.append [|Array.head x|] y) cl00 cl01
        
    let cxl = Array.map3 (fun x y z -> 
                                            let hx1 = z |> hxlTyp sqn (Array.append occ z)
                                            {
                                                Name = snd x
                                                Rfid = snd y
                                                Size = fst x
                                                Seqn = sqn
                                                Base = Array.head hx1
                                                Hxls = hx1
                                            })szn idn cl1
    cxl

///

/// <summary> Categorize constituent Hexels within a Coxel. </summary>
/// <param name="cxl"> A coxel. </param>
/// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
let cxlHxl
    (cxl : Cxl)  = 

    let avrv = cxl.Hxls 
            |> Array.Parallel.partition
                (fun x -> x = AV(hxlCrd x))
    let rv01 = (snd avrv) 
            |> Array.Parallel.partition
                (fun x-> (available 
                    cxl.Seqn 
                    (AV(hxlCrd x)) 
                    (allAV false (cxl.Hxls))) < 1)
    let av01 = match (snd rv01) with 
                | [||] -> avrv |> fst |> bndSqn cxl.Seqn
                | _ -> avrv |> fst |> cntSqn cxl.Seqn
    let br01 = match (fst rv01) with 
                | [||] -> rv01 |> snd |> bndSqn cxl.Seqn
                | _ -> rv01 |> snd |> cntSqn cxl.Seqn
         
    let pr01 = match av01 with 
                    | [||] -> br01
                    | _ -> match br01 with 
                            | [||] -> av01
                            | _ -> match adjacent 
                                    cxl.Seqn 
                                    (Array.last av01) 
                                    |> allAV true
                                    |> Array.contains (Array.head br01) with 
                                    | true -> Array.append av01 br01
                                    | false -> Array.append av01 (Array.rev br01)
    {|
        Base = cxl.Base
        Hxls = cxl.Hxls
        Core = rv01 |> fst 
        Prph = pr01
        Brdr = br01
        Avbl = av01 
    |}  