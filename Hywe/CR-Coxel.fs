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
    (elv : int)
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
        (elv : int)
        (occ : Hxl[])
        (acc:(Hxl*int)[][])
        (cnt : int)= 
            
        match cnt with 
        | c when c < 0x1 -> acc
        | _ -> 
                let occ = 
                    acc 
                    |> Array.concat 
                    |> getHxls
                    |> Array.append occ
                    |> Array.append (getHxls hxo)
                    |> Array.distinct
                    |> hxlUni 1

                let rpt = Array.Parallel.map (fun x 
                                                -> (snd x) - 0x1) hxo
                let hx1 =  
                    acc
                    |> Array.Parallel.map (fun x
                                            -> Array.filter (fun a -> (available sqn elv a occ) > 0x0) x)
                    |> Array.Parallel.map (fun x 
                                            -> Array.tryHead x)
                    |> Array.Parallel.map (fun x 
                                            -> match x with
                                                | Some a -> a 
                                                | None ->  (hxlVld sqn (RV(0,0,elv)),0xFFFFFFFF))                
                    |> Array.map2 (fun x y 
                                    -> fst y, x) rpt
                    
                let inc = increments sqn elv hx1 occ
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)
                // Avoid bridge in Coxel
                //let acc = ac1 
                //            |> Array.Parallel.map (fun x -> match Array.length x > 3 with
                //                                            | false -> x
                //                                            | true -> 
                //                                                    let h1 = hxlUni 1 (getHxls x) 
                //                                                    let h2 = h1.[Array.length h1 - 2]
                //                                                    let b1 = available sqn h2 (Array.except [|h2; Array.last h1|] h1) = 5
                //                                                    let b2 = available sqn (Array.last h1) (h1 |> Array.rev |> Array.tail) = 5
                //                                                    match b1 && b2 with 
                //                                                    | false -> x
                //                                                    | true -> Array.removeManyAt (Array.length h1 - 2) 2 x)

                let occ = Array.concat[|getHxls 
                    (Array.concat [|
                    Array.concat acc
                    inc
                    hx1|]);occ|] 
                        |> hxlUni 1

                (clsts hx1 elv occ acc (cnt - 0x1))
         
    let cls = 
        clsts bas elv oc1 acc cnt
            |> Array.Parallel.map(fun x 
                                    -> Array.filter(fun (_,z) -> z >= 0) x)
        
    let cl1 = 
        cls
        |> Array.Parallel.map(fun x -> getHxls x)

 (*  // Avoid single unclustered cell towards the end
    let hxlElm (sqn:Sqn) (hxl:Hxl[])=
        let hxo = hxl
        let avl = 5
        let hxl = hxlUni 1 hxl
        let acc = hxl |> Array.filter (fun x -> (available sqn x hxl) < avl)
        let rec elm (sqn:Sqn) (hxl:Hxl[]) (acc: Hxl[]) = 
            let hx1 = hxl
            match (Array.length hx1 = Array.length acc) with
            | true -> acc
            | false -> 
                        let hx1 = acc
                        let acc = hx1|> Array.filter (fun x -> (available sqn x hx1) < avl)
                        elm sqn hx1 acc
        let hx1 = elm sqn hxl acc
        hxlRst hxo hx1

   let cl01 = 
        cl00 |> Array.Parallel.map(fun x -> hxlElm sqn x)
        //|> Array.Parallel.map(fun x -> x |> hxlFil sqn)

    //let cl01 = cl00 |> Array.Parallel.map(fun x -> Array.filter(fun y -> (available sqn y x) < 5)x)
 
    let cl1 = Array.map2 (fun x y 
                                -> Array.append [|Array.head x|] y) cl00 cl01*)

    let cxl =
        Array.map3 (fun x y z ->
            // z -> candidate hexel cluster (Hxl[])
            // Ensure we get a safe hx1 from hxlChk
            let hx1 = z |> hxlChk sqn elv (Array.append occ z)

            // pattern-match hx1 so we never call Array.head on empty array
            match Array.toList hx1 with
            | [] ->
                // fallback when hx1 is empty: choose defaults
                {
                    Name = snd x
                    Rfid = snd y
                    Size = fst x
                    Seqn = sqn
                    Base = identity elv           // fallback base
                    Hxls = [||]               // no hexels available
                }
            | head :: _ ->
                // hx1 is non-empty, safe to use head and build Hxls
                let rest =
                    match Array.length hx1 > 0 with
                    | true -> Array.except ([| head; identity elv |]) hx1
                    | false -> [||] // unreachable because of match, but kept for clarity

                {
                    Name = snd x
                    Rfid = snd y
                    Size = fst x
                    Seqn = sqn
                    Base = head
                    Hxls = rest
                }
        ) szn idn cl1

    cxl
///

/// <summary> Count open/exposed Hexels. </summary>
/// <param name="cxl"> A coxel. </param>
/// <param name="sqn"> Sequence to follow. </param>
/// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
let cxlExp 
    (cxl : Cxl[])
    (sqn : Sqn)
    (elv : int)= 
    let occ = cxl |> Array.map (fun x -> x.Hxls) |> Array.concat |> hxlUni 1 
    let cxlAvl 
        (cx:Cxl)
        (sq:Sqn)
        (oc:Hxl[]) =
        let hx = cx.Hxls |> hxlUni 1 
        hx |> Array.filter(fun x -> (available sq elv x oc)>0) |> Array.length
    cxl |> Array.map (fun a -> cxlAvl a sqn occ)
///

/// <summary> Categorize constituent Hexels within a Coxel. </summary>
/// <param name="cxl"> A coxel. </param>
/// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
let cxlHxl
    (cxl : Cxl)
    (elv : int) = 
    /// <summary> Hexel Ring Boundary Sequence. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Boundary/Peripheral hexels. </returns>
    let bndSqn
        (sqn : Sqn)
        (elv : int)
        (hxo : Hxl[]) = 
        /// <summary> Arrange/sort hexels in continuous sequence. </summary>
        /// <param name="sqn"> Sequence to follow. </param>
        /// <param name="hxl"> Array of hexels. </param>
        /// <param name="acc"> Accumulator for recursive function. </param>
        /// <param name="cnt"> Counter. </param>
        /// <returns> Array of sorted hexels </returns>
        let rec arr 
            (sqn : Sqn)
            (elv : int)
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
                arr sqn elv hxl acc (cnt-1) opt

        let hxl = hxo|> Array.sortByDescending 
                    (fun x -> available sqn elv x hxo)
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) true

        let b1 = Array.length a1 = Array.length hxl
            
        let ar1 = match b1 with 
                    | true -> a1
                    | false -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) false
        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> Array.rev ar1
                | false -> Array.rev (hxlUni 1 ar1)

    /// <summary> Hexel Ring Segment Sequence. </summary>
    let cntSqn
        (sqn : Sqn)
        (elv : int)
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
                    (fun x -> available sqn elv x hxl)
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
                    elv
                    (AV(hxlCrd x)) 
                    (hxlUni 1 (cxl.Hxls))) < 1)
    let av01 = match (snd rv01) with 
                | [||] -> avrv |> fst |> bndSqn cxl.Seqn elv
                | _ -> avrv |> fst |> cntSqn cxl.Seqn elv
    let br01 = match (fst rv01) with 
                | [||] -> rv01 |> snd |> bndSqn cxl.Seqn elv
                | _ -> rv01 |> snd |> cntSqn cxl.Seqn elv
         
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
///

/// <summary> Coxel Offseted Boundary Wrap </summary>
/// <param name="cxl"> Coxel. </param>
/// <returns> Boundary Wrap vertices. </returns>
let cxlPrm
    (cxl : Cxl)
    (elv : int) : (int*int)[] =
    let rec removeCollinear (points: (int * int)[]) : (int * int)[] =
        let len = points.Length
        match len with
        | 0 | 1 | 2 -> points
        | _ when len < 100 ->
            // Small arrays → do sequential recursive cleaning
            let (x1, y1) = points.[0]
            let (x2, y2) = points.[1]
            let (x3, y3) = points.[2]
            let rest = points.[3..]
            let cross = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
            match cross with
            | 0 -> removeCollinear (Array.append [| (x1, y1) |] (Array.append [| (x3, y3) |] rest))
            | _ -> Array.append [| (x1, y1) |] (removeCollinear points.[1..])
        | _ ->
            // Large arrays → split into halves and process in parallel
            let mid = len / 2
            let left = points.[..mid]
            let right = points.[mid..]

            let results =
                [| left; right |]
                |> Array.Parallel.map removeCollinear

            let leftCleaned, rightCleaned = results.[0], results.[1]

            // Merge boundaries carefully — ensure no duplicate collinear points at the join
            let merged =
                match Array.tryLast leftCleaned, Array.tryHead rightCleaned with
                | Some (x1, y1), Some (x2, y2) when leftCleaned.Length >= 2 && rightCleaned.Length >= 2 ->
                    let (x0, y0) = leftCleaned.[leftCleaned.Length - 2]
                    let (x3, y3) = rightCleaned.[1]
                    let cross = (y1 - y0) * (x3 - x2) - (y3 - y2) * (x2 - x1)
                    match cross with
                    | 0 -> Array.append leftCleaned.[..leftCleaned.Length-2] rightCleaned
                    | _ -> Array.append leftCleaned rightCleaned
                | _ -> Array.append leftCleaned rightCleaned

            merged

    let prm =
        hxlOfs cxl.Seqn elv cxl.Hxls 
        |> Array.map (fun x -> hxlCrd x) 
        |> Array.map (fun (x,y,_) -> (x,y))

    removeCollinear prm
///

/// <summary> Coxel Center </summary>
/// <param name="cxl"> Coxel. </param>
/// <returns> Coxel hexel closest to center </returns> 
let cxlCnt 
    (cxl : Cxl): int * int = 
    match cxl.Hxls with
    | [||] -> 
        (-10, -10) 
    | hxls ->
        let hxXY : (int * int)[] = 
            hxls 
            |> Array.map (fun a -> 
                let x, y, _ = hxlCrd a
                x, y
            )
        let numPoints = hxXY.Length
        let sumX = hxXY |> Array.sumBy fst
        let sumY = hxXY |> Array.sumBy snd
        
        let centerX = sumX / numPoints
        let centerY = sumY / numPoints
        let center = (centerX, centerY)
        let closestHxl = 
                    hxls 
                    |> Array.minBy (fun hxl ->
                        let x, y, _ = hxlCrd hxl
                        let centerX, centerY = center 
                        let dx = x - centerX
                        let dy = y - centerY
                        dx*dx + dy*dy
                    )
        let finalX, finalY, _ = hxlCrd closestHxl
        (finalX, finalY)