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
    let idn = Array.Parallel.map (fun(x,y,_,_)-> x,y) ini

    let cnt = 
            bas
            |> Array.Parallel.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 bas
    let oc1 = (Array.append occ (getHxls bas)) |> hxlUni 1 
        
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
                    |> hxlUni 1

                let rpt = Array.Parallel.map (fun x 
                                                -> (snd x) - 0x1) hxo
                let Hxl =  
                    acc
                    |> Array.Parallel.map (fun x
                                            -> Array.filter (fun a -> (available sqn a occ) > 0x0) x)
                    |> Array.Parallel.map (fun x 
                                            -> Array.tryHead x)
                    |> Array.Parallel.map (fun x 
                                            -> match x with
                                                | Some a -> a 
                                                | None ->  (hxlVld sqn identity,0xFFFFFFFF))                
                    |> Array.map2 (fun x y 
                                    -> fst y, x) rpt
                    
                let inc = increments sqn Hxl occ
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)

                let occ = Array.concat[|getHxls 
                    (Array.concat [|
                    Array.concat acc
                    inc
                    Hxl|]);occ|] 
                        |> hxlUni 1

                (clsts Hxl occ acc (cnt - 0x1))
         
    let cls = 
        clsts bas oc1 acc cnt
            |> Array.Parallel.map(fun x 
                                    -> Array.filter(fun (_,z) -> z >= 0) x)
        
    let cl1 = 
        cls
        |> Array.Parallel.map(fun x -> getHxls x)

(*    // Avoid single unclustered cell towards the end
    let cl01 = 
        cl00
        |> Array.Parallel.map(fun x 
                                -> Array.filter(fun y 
                                                    -> (available sqn y x) < 5)x)
         
    let cl1 = Array.map2 (fun x y 
                                -> Array.append [|Array.head x|] y) cl00 cl01*)

    let cxl = Array.map3 (fun x y z -> 
                                            let hx1 = z 
                                                    |> hxlChk sqn (Array.append occ z)
                                                
                                            {
                                                Name = snd x
                                                Rfid = snd y
                                                Size = fst x
                                                Seqn = sqn
                                                Base = Array.head hx1
                                                Hxls = match Array.length hx1 > 0 with 
                                                        | true -> Array.except  ([|Array.head hx1|]) hx1
                                                        | false -> [||]
                                            })szn idn cl1
    cxl
///

/// <summary> Count open/exposed Hexels. </summary>
/// <param name="cxl"> A coxel. </param>
/// <param name="sqn"> Sequence to follow. </param>
/// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
let cxlExp 
    (cxl : Cxl[])
    (sqn: Sqn) = 
    let occ = cxl |> Array.map (fun x -> x.Hxls) |> Array.concat |> hxlUni 1 
    let cxlAvl 
        (cx:Cxl)
        (sq:Sqn)
        (oc:Hxl[]) =
        let hx = cx.Hxls |> hxlUni 1 
        hx |> Array.filter(fun x -> (available sq x oc)>0) |> Array.length
    cxl |> Array.map (fun a -> cxlAvl a sqn occ)
///

/// <summary> Categorize constituent Hexels within a Coxel. </summary>
/// <param name="cxl"> A coxel. </param>
/// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
let cxlHxl
    (cxl : Cxl)  = 
    /// <summary> Hexel Ring Boundary Sequence. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Boundary/Peripheral hexels. </returns>
    let bndSqn
        (sqn : Sqn) 
        (hxo : Hxl[]) = 
        /// <summary> Arrange/sort hexels in continuous sequence. </summary>
        /// <param name="sqn"> Sequence to follow. </param>
        /// <param name="hxl"> Array of hexels. </param>
        /// <param name="acc"> Accumulator for recursive function. </param>
        /// <param name="cnt"> Counter. </param>
        /// <returns> Array of sorted hexels </returns>
        let rec arr 
            (sqn : Sqn) 
            (hxl : Hxl[]) 
            (acc : Hxl[]) 
            (cnt : int)
            (opt : bool) = 
            match cnt with 
            | a when cnt <= 0x1 -> acc
            | _ -> 
                let hxl = Array.except acc hxl
                let hx1 = ((Array.filter (fun x -> Array.contains x hxl) 
                                (adjacent sqn (Array.last acc))))                
                let hx2 = match opt with 
                                | false -> Array.tryHead hx1
                                | true -> Array.tryLast hx1
                let hx3 = match hx2 with 
                                | Some a -> [|a|]
                                | None -> [||]
                let acc = Array.append acc  hx3
                arr sqn hxl acc (cnt-1) opt

        let hxl = hxo|> Array.sortByDescending 
                    (fun x -> available sqn x hxo)
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) true

        let b1 = Array.length a1 = Array.length hxl
            
        let ar1 = match b1 with 
                    | true -> a1
                    | false -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) false
        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> Array.rev ar1
                | false -> Array.rev (hxlUni 1 ar1)

    /// <summary> Hexel Ring Segment Sequence. </summary>
    let cntSqn
        (sqn : Sqn)
        (hxo : Hxl[]) =      
        let hxl = hxlUni 1 hxo
        let rec ctSq 
            (sqn : Sqn)
            (hxl : Hxl[])
            (acc : Hxl[])
            (cnt : int) = 
            match cnt with 
            | x when x<=1 -> acc
            | _ -> 
                    let b = Array.last acc
                    let hxl = Array.except [|b|] hxl
                    let d = (adjacent sqn b) |> Array.tail
                    let e = d |> Array.filter
                                (fun x -> Array.contains x hxl) 
                                |> Array.tryHead
                    let f = match e with 
                                | Some a -> [|a|]
                                | None -> [||]
                    let acc = Array.append acc f
                    ctSq sqn hxl acc (cnt-1)

        let hxl = hxl |> Array.sortByDescending 
                    (fun x -> available sqn x hxl)
        let cnt = Array.length(hxl)
        let arr =  match hxl with 
                        | [||] -> [||]
                        | _ -> ctSq sqn hxl ([|Array.head hxl|]) cnt
        let bln = cnt = Array.length(arr)
        let ar1 = match bln with 
                    | true -> arr
                    | false -> ctSq sqn (Array.rev hxl) ([|Array.last hxl|]) cnt
        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> ar1
                | false -> hxlUni 2 ar1

    let avrv = cxl.Hxls 
            |> Array.Parallel.partition
                (fun x -> x = AV(hxlCrd x))
    let rv01 = (snd avrv) 
            |> Array.Parallel.partition
                (fun x-> (available 
                    cxl.Seqn 
                    (AV(hxlCrd x)) 
                    (hxlUni 1 (cxl.Hxls))) < 1)
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
                                    |> hxlUni 2
                                    |> Array.contains (Array.head br01) with 
                                    | true -> Array.append av01 br01
                                    | false -> Array.append av01 (Array.rev br01)
    // Clockwise sequence
    let pr02 = match Array.length pr01 > 2 with 
                    | false -> pr01
                    | true -> 
                            let x1,y1,_ = hxlCrd (Array.last pr01)
                            let x2,y2,_ = hxlCrd (Array.head pr01)
                            let x3, y3,_ = hxlCrd (pr01[1])
                            let gs = (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
                            match gs with 
                            | 0 when x2 > x1 -> pr01
                            | a when a < 0 -> pr01
                            | _ -> Array.rev pr01

    {|
        Base = cxl.Base
        Hxls = cxl.Hxls
        Core = rv01 |> fst 
        Prph = pr02 
        Brdr = br01
        Avbl = av01 
    |}  