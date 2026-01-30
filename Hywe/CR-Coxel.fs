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
let prpVlu = function 
    | Label s | Refid s -> s
    | Count i -> string i
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
        
    let bas = ini |> Array.map (fun (h, _, p, _) -> h, int (prpVlu p))
    let szn = ini |> Array.map (fun (_, _, y, z) -> y, z)
    let idn = ini |> Array.map (fun (h, r, _, _) -> h, r)

    let cnt = bas |> Array.map snd |> function [||] -> 0 | x -> Array.max x
    let acc = bas |> Array.map (fun x -> [| x |])
    let oc1 = (Array.append occ (getHxls bas)) |> hxlUni 1
        
    let rec clsts 
        (hxo : (Hxl * int)[]) 
        (elv : int) 
        (occ : Hxl[]) 
        (acc : (Hxl * int)[][]) 
        (cnt : int) = 
    
        match cnt with 
        | c when c < 1 -> acc
        | _ -> 
            //Fuse transformation: Find new heads and decrement counts in one pass
            let hx1 = 
                acc |> Array.Parallel.mapi (fun i row ->
                    let (_, count) = hxo.[i]
                    row 
                    |> Array.tryFind (fun a -> (available sqn elv a occ) > 0)
                    |> function
                       | Some (h, _) -> (h, count - 1)
                       | None        -> (hxlVld sqn (RV(0,0,elv)), 0xFFFFFFFF)
                )

            let inc = increments sqn elv hx1 occ
                            
            //Efficiently grow the accumulator
            let nextAcc = 
                Array.map2 (fun current (newEl: Hxl * int) -> 
                    Array.append current [| newEl |]) 
                    acc 
                    inc

            //Rebuild occupancy: Extract Hxl from (Hxl * int) tuples explicitly
            let nextOcc = 
                [| 
                    yield! occ
                    yield! (hxo |> Array.map fst) // Extract Hxl from hxo
                    yield! (inc |> Array.map fst) // Extract Hxl from inc
                    yield! (hx1 |> Array.map fst) // Extract Hxl from hx1
                |] 
                |> Array.distinct
                |> hxlUni 1

            clsts hx1 elv nextOcc nextAcc (cnt - 1)

    let cls = 
        clsts bas elv oc1 acc cnt
        |> Array.Parallel.map (fun row -> 
            row |> Array.filter (fun (_, z) -> z >= 0))
        
    let cl1 = cls |> Array.Parallel.map getHxls

    Array.map3 (fun (y, z) (h, r) (cluster: Hxl[]) ->
            let hx1 = hxlChk sqn elv (Array.append occ cluster) cluster

            match hx1 with
            | [||] ->
                {
                    Name = z; Rfid = r; Size = y; Seqn = sqn
                    Base = identity elv; Hxls = [||]
                }
            | _ ->
                let head = hx1.[0]
                let rest = hx1 |> Array.filter (fun x -> x <> head && x <> identity elv)
                {
                    Name = z; Rfid = r; Size = y; Seqn = sqn
                    Base = head; Hxls = rest
                }
        ) szn idn cl1
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
    let pr02 = 
        match pr01 with
        // If array has 0, 1, or 2 elements, just return it
        | [| |] | [| _ |] | [| _; _ |] -> pr01 
        | _ ->
            let (x1, y1, _) = hxlCrd (Array.last pr01)
            let (x2, y2, _) = hxlCrd (Array.head pr01)
            let (x3, y3, _) = hxlCrd pr01.[1]
        
            // The cross product (signed area)
            let gs = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

            // Compare sign to determine orientation
            match sign gs with
            | -1 -> pr01                // Already correct orientation
            | 0  ->                     // Collinear case
                match x2 > x1 with 
                | true -> pr01 
                | false -> Array.rev pr01
            | _  -> Array.rev pr01      // Opposite orientation, flip it

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
let (|Collinear|Turning|) (p1, p2, p3) =
    let (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
    let crossProduct = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
    match crossProduct with
    | 0 -> Collinear
    | _ -> Turning

let cxlPrm 
    (cxl : Cxl) 
    (elv : int) =
    let rec clean points =
        match points with
        // Match 3 points at a time and apply our Active Pattern
        | p1 :: p2 :: p3 :: rest ->
            match (p1, p2, p3) with
            | Collinear -> clean (p1 :: p3 :: rest)      // Drop p2
            | Turning   -> p1 :: clean (p2 :: p3 :: rest) // Keep p1, move to p2
        | _ -> points

    hxlOfs cxl.Seqn elv cxl.Hxls 
    |> Array.map (hxlCrd >> (fun (x, y, _) -> x, y))
    |> Array.toList
    |> clean
    |> List.toArray
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