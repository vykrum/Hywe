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
    
    // Use ResizeArrays for efficient growth
    let acc = bas |> Array.map (fun x -> 
        let list = System.Collections.Generic.List<Hxl * int>()
        list.Add(x)
        list)
    
    // Use HashSet for O(1) occupancy checks
    let initialOcc = Array.append occ (getHxls bas)
    let occSet = hxlSet initialOcc
        
    let rec clsts 
        (hxo : (Hxl * int)[]) 
        (elv : int) 
        (occSet : System.Collections.Generic.HashSet<Hxl>) 
        (acc : System.Collections.Generic.List<Hxl * int>[]) 
        (cnt : int) = 
    
        match cnt with 
        | c when c < 1 -> acc
        | _ -> 
            // Find growth points (sequential for WASM stability)
            let hx1 = 
                acc |> Array.mapi (fun i row ->
                    let (_, count) = hxo.[i]
                    
                    // Scan forward to find the first hexel with available neighbors (restores original growth behavior)
                    let mutable foundPoint = None
                    let mutable j = 0
                    while j < row.Count && foundPoint.IsNone do
                        let h, _ = row.[j]
                        if (availableSet sqn elv h occSet) > 0 then
                            foundPoint <- Some h
                        j <- j + 1
                    
                    match foundPoint with
                    | Some h -> (h, count - 1)
                    | None   -> (hxlVld sqn (RV(0,0,elv)), 0xFFFFFFFF)
                )

            // Calculate increments for this step
            let inc = incrementsSet sqn elv hx1 occSet
                            
            // Update clusters and occupancy set
            for i = 0 to acc.Length - 1 do
                let newEl = inc.[i]
                acc.[i].Add(newEl)
                let h, _ = newEl
                let x, y, z = hxlCrd h
                occSet.Add(AV(x, y, z)) |> ignore
            
            clsts hx1 elv occSet acc (cnt - 1)

    let cls = 
        clsts bas elv occSet acc cnt
        |> Array.map (fun row -> 
            row.ToArray() |> Array.filter (fun (_, z) -> z >= 0))
        
    let cl1 = cls |> Array.map getHxls

    Array.map3 (fun (y, z) (h, r) (cluster: Hxl[]) ->
            let clusterOcc = hxlSet (Array.append occ cluster)
            let hx1 = hxlChkSet sqn elv clusterOcc cluster

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

/// <summary> Generate child coxels. </summary>
/// <param name="sqn"> Sequence. </param>
/// <param name="elv"> Elevation. </param>
/// <param name="bsCx"> Host/Base Coxel. </param>
/// <param name="tre"> Batch of target properties (includes Host ID as first element). </param>
/// <param name="occ"> Currently occupied hexels. </param>
/// <returns> A tuple of (Updated Host Coxel, Generated Child Coxels, Updated Occupied Hexels) </returns>
let coxelChildren
    (sqn : Sqn)
    (elv : int)
    (bsCx : Cxl)
    (tre : (Prp*Prp*Prp)[])
    (occ : Hxl[]) =
    
    let chHx = bsCx.Hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
    let cnt = (Array.length tre) - 1
    
    let chBs = 
        match (Array.length chHx) >= cnt with 
        | true -> 
            let divs = match cnt > 0 with | true -> (Array.length chHx) / cnt | false -> 1
            chHx |> Array.chunkBySize divs |> Array.map Array.head |> Array.take cnt
        | false -> 
            Array.append chHx (Array.replicate (max 0 (cnt - (Array.length chHx))) (identity elv))
            
    let ini = Array.map2 (fun a (b, c, d) -> a, b, c, d) chBs (Array.tail tre)
    let cxc1 = coxel sqn elv ini occ
    
    let chOc1 = 
        cxc1
        |> Array.collect (fun cx -> cx.Hxls)
        |> Array.append occ
        |> hxlUni 2
    
    let finalCxc =
        cxc1 |> Array.map (fun x -> 
            let clusterOcc = hxlSet (Array.append chOc1 x.Hxls)
            let h2 = hxlChkSet sqn elv clusterOcc x.Hxls
            let baseCheck = hxlChkSet sqn elv clusterOcc [|x.Base|]
            let b2 = if Array.isEmpty baseCheck then x.Base else baseCheck.[0]
            { x with Hxls = h2; Base = b2 })
            
    bsCx, finalCxc, chOc1
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
        let arr (hxl : Hxl[]) (opt : bool) = 
            let availableSet = System.Collections.Generic.HashSet<Hxl>(hxl)
            let startNode = Array.last hxl
            let acc = System.Collections.Generic.List<Hxl>()
            acc.Add(startNode)
            availableSet.Remove(startNode) |> ignore
            
            let rec loop current cnt =
                match cnt with
                | c when c <= 1 -> ()
                | _ ->
                    let adj = adjacent sqn current
                    let validAdj = adj |> Array.filter (fun x -> availableSet.Contains(x))
                    
                    let nextOpt = 
                        match opt with
                        | true -> Array.tryLast validAdj
                        | false -> Array.tryHead validAdj
                        
                    match nextOpt with
                    | Some nxt ->
                        acc.Add(nxt)
                        availableSet.Remove(nxt) |> ignore
                        loop nxt (cnt - 1)
                    | None -> ()

            loop startNode hxl.Length
            acc.ToArray()

        let hxl = hxo |> Array.sortByDescending (fun x -> available sqn elv x hxo)
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr hxl true

        let b1 = Array.length a1 = Array.length hxl
            
        let ar1 = match b1 with 
                    | true -> a1
                    | false -> arr hxl false

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
        
        let ctSq (hxlArr : Hxl[]) = 
            let availableSet = System.Collections.Generic.HashSet<Hxl>(hxlArr)
            let startNode = Array.head hxlArr
            let acc = System.Collections.Generic.List<Hxl>()
            acc.Add(startNode)
            availableSet.Remove(startNode) |> ignore
            
            let rec loop current cnt =
                match cnt with
                | c when c <= 1 -> ()
                | _ ->
                    let d = (adjacent sqn current) |> Array.tail
                    let e = d |> Array.tryFind (fun x -> availableSet.Contains(x))
                    match e with
                    | Some nxt ->
                        acc.Add(nxt)
                        availableSet.Remove(nxt) |> ignore
                        loop nxt (cnt - 1)
                    | None -> ()

            loop startNode hxlArr.Length
            acc.ToArray()

        let hxl = hxl |> Array.sortByDescending (fun x -> available sqn elv x hxl)
        let cnt = Array.length(hxl)
        let arr =  
            match hxl with 
            | [||] -> [||]
            | _ -> ctSq hxl
        
        let bln = cnt = Array.length(arr)
        let ar1 = match bln with 
                    | true -> arr
                    | false -> ctSq (Array.rev hxl)

        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> ar1
                | false -> hxlUni 1 ar1

    let avrv = cxl.Hxls 
            |> Array.partition
                (fun x -> x = AV(hxlCrd x))
    let rv01 = (snd avrv) 
            |> Array.partition
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
let (|Collinear|Turning|) (p1: float * float, p2: float * float, p3: float * float) =
    let (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
    let crossProduct = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
    if System.Math.Abs(crossProduct) < 0.0001 then Collinear else Turning

let cxlPrm 
    (cxl : Cxl) 
    (elv : int) =
    let rec clean points =
        match points with
        | p1 :: p2 :: p3 :: rest ->
            match (p1, p2, p3) with
            | Collinear -> clean (p1 :: p3 :: rest)
            | Turning   -> p1 :: clean (p2 :: p3 :: rest)
        | _ -> points

    // "Halfway" Boundary Logic:
    // Instead of using the centers of neighboring hexels (outset), 
    // we calculate the midpoint between each interior hexel and its exterior neighbor.
    // This aligns the boundary exactly with the shared edges of the hex grid.
    let outside = hxlOfs cxl.Seqn elv cxl.Hxls
    let insideSet = hxlSet cxl.Hxls
    
    outside 
    |> Array.collect (fun hout ->
        let (ox, oy, _) = hxlCrd hout
        // Check all neighbors of each 'outside' hexel
        adjacent cxl.Seqn hout 
        |> Array.choose (fun n -> 
            let (ix, iy, _) = hxlCrd n
            // If the neighbor is 'inside' the coxel, the boundary vertex is the midpoint
            if insideSet.Contains(AV(ix, iy, elv)) then
                Some ( (float ox + float ix) / 2.0, (float oy + float iy) / 2.0 )
            else None)
    )
    |> Array.distinct // Remove duplicate midpoints (shared by multiple edges)
    |> Array.toList
    |> clean         // Remove collinear points and clean the polygon winding
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
        
        let centerX = float sumX / float numPoints
        let centerY = float sumY / float numPoints
        
        let closestHxl = 
            hxls 
            |> Array.minBy (fun hxl ->
                let x, y, _ = hxlCrd hxl
                let dx = float x - centerX
                let dy = float y - centerY
                dx*dx + dy*dy
            )
        let finalX, finalY, _ = hxlCrd closestHxl
        (finalX, finalY)

/// <summary> Functional Adjacency Matrix Calculation. </summary>
let cxlAdj (cxls: Cxl[]) =
    match cxls with
    | [||] -> [||], [||]
    | _ ->
        let sqn = cxls.[0].Seqn
        let elv = 0 
        let coxelSets = cxls |> Array.map (fun c -> Hexel.hxlSet c.Hxls)
        let allNeighbors = 
            cxls |> Array.map (fun c ->
                c.Hxls 
                |> Array.collect (Hexel.adjacent sqn)
                |> Array.map (fun h -> Hexel.AV(Hexel.hxlCrd h))
                |> Array.distinct
            )
        let names = cxls |> Array.map (fun c -> prpVlu c.Name)
        let matrix = 
            allNeighbors |> Array.mapi (fun i neighbors ->
                coxelSets |> Array.mapi (fun j set ->
                    match i = j with
                    | true -> false
                    | false -> neighbors |> Array.exists (fun h -> set.Contains(h))
                )
            )
        names, matrix
