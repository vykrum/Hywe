namespace Hywe.Core

/// <summary> 
/// Coxel (Collection of Hexels) 
/// Defines the primary building blocks of the Hygrid Woven Ensemble. 
/// Orchestrates spatial configurations as the second stage in the hierarchy: Hexel-Coxel-Xyxel-Zaxel.
/// </summary>

module Coxel =
    open Hexel
    open System.Collections.Frozen

    /// <summary> Coxels are primarily a collections of unique hexels </summary>
    /// <summary> Coxel type properties </summary>
    type Prp = 
        | Label of string
        | Refid of string
        | Count of int

    /// <summary> Coxel type consists of hexels and properties. </summary>
    type Cxl = 
        {
            Name : Prp
            Rfid : Prp
            Size : Prp
            Seqn : Sqn
            Base : Hxl
            Hxls : Hxl[]
        }  

    /// <summary> Property value types </summary>
    let prpVlu = function 
        | Label s | Refid s -> s
        | Count i -> string i

    let getCxlCoordsString (cxl: Cxl) =
        Array.append [|cxl.Base|] cxl.Hxls 
        |> Array.map (fun h -> 
            let (x, y, _) = hxlCrd h
            sprintf "%s.%s" (toBase36 (int64 x)) (toBase36 (int64 y)))
        |> String.concat " "

    let getBaseCoordString (cxl: Cxl) =
        let (x, y, _) = hxlCrd cxl.Base
        sprintf "%s.%s" (toBase36 (int64 x)) (toBase36 (int64 y))

    /// <summary> Creating an array of coxels. </summary>
    let coxel (sqn : Sqn) (elv : int) (ini : (Hxl*Prp*Prp*Prp)[]) (occ : Hxl[]) = 
        let bas = ini |> Array.map (fun (h, _, p, _) -> h, int (prpVlu p))
        let szn = ini |> Array.map (fun (_, _, y, z) -> y, z)
        let idn = ini |> Array.map (fun (h, r, _, _) -> h, r)

        let cnt = match bas |> Array.map snd with | [||] -> 0 | x -> Array.max x
        
        let acc = bas |> Array.map (fun x -> [| x |])
        
        let initialOcc = Array.append occ (getHxls bas)
            
        let rec clsts (hxo : (Hxl * int)[]) (currentOcc : Hxl[]) (acc : (Hxl * int)[][]) (cnt : int) = 
            match cnt with 
            | c when c < 1 -> acc
            | _ -> 
                let hx1 = 
                    acc |> Array.mapi (fun i row ->
                        let (_, count) = hxo.[i]
                        let foundPoint = 
                            row |> Array.tryFind (fun (h, _) -> 
                                available sqn elv h currentOcc > 0
                            )
                        
                        match foundPoint with
                        | Some (h, _) -> (h, count - 1)
                        | None   -> (hxlVld sqn (RV(0,0,elv)), 0xFFFFFFFF)
                    )

                let inc = increments sqn elv hx1 currentOcc
                                
                let nextAcc = Array.map2 (fun row newEl -> Array.append row [|newEl|]) acc inc
                let newOcc = Array.append currentOcc (getHxls inc) |> hxlUni 1
                
                clsts hx1 newOcc nextAcc (cnt - 1)

        let cls = 
            clsts bas initialOcc acc cnt
            |> Array.map (fun row -> 
                row |> Array.filter (fun (_, z) -> z >= 0))
            
        let cl1 = cls |> Array.map getHxls

        Array.map3 (fun (y, z) (h, r) (cluster: Hxl[]) ->
                let hx1 = hxlChk sqn elv occ cluster

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

    /// <summary> Generate child coxels. </summary>
    let coxelChildren (sqn : Sqn) (elv : int) (bsCx : Cxl) (tre : (Prp*Prp*Prp)[]) (occ : Hxl[]) =
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
                let h2 = hxlChk sqn elv chOc1 x.Hxls
                let baseCheck = hxlChk sqn elv chOc1 [|x.Base|]
                let b2 = match Array.isEmpty baseCheck with | true -> x.Base | false -> baseCheck.[0]
                { x with Hxls = h2; Base = b2 })
                
        bsCx, finalCxc, chOc1

    /// <summary> Create the base/root coxel for a layout. </summary>
    let createBaseCoxel (sqn : Sqn) (elv : int) (bsAtr : string) (entryFallback : Hxl) (id : Prp) (ct : Prp) (lb : Prp) (occ : Hxl[]) (bsHxSetOverride : Cxl option) =
        match bsHxSetOverride with
        | Some parentCxl ->
            let updatedHxls = 
                parentCxl.Hxls 
                |> Array.map (fun h -> 
                    let (x, y, _) = hxlCrd h
                    AV(x, y, elv))
            
            let (bx, by, _) = hxlCrd parentCxl.Base
            let baseHx = AV(bx, by, elv)
            let hxlsWithStatus = hxlChk sqn elv occ updatedHxls

            let rootCxl = {
                Name = parentCxl.Name
                Rfid = id 
                Size = parentCxl.Size
                Seqn = sqn
                Base = baseHx
                Hxls = hxlsWithStatus
            }
            let updatedOcc = Array.append occ (Array.append [|baseHx|] updatedHxls) |> hxlUni 1
            rootCxl, updatedOcc
        | None ->
            let bsHx = 
                match bsAtr with
                | "0" -> entryFallback
                | _ ->
                    match bsAtr.Split ',' with
                    | [| xStr; yStr |] ->
                        match System.Int32.TryParse(xStr.Trim()), System.Int32.TryParse(yStr.Trim()) with
                        | (true, x), (true, y) -> AV(x, y, elv)
                        | _ -> entryFallback
                    | _ -> entryFallback
            
            let cti = match ct with | Count x when x > 0 -> Count (x - 1) | _ -> Count 0
            let ac0 = coxel sqn elv [| bsHx, id, cti, lb |] occ
            match Array.tryHead ac0 with
            | None -> 
                let emptyCxl = { Name = lb; Rfid = id; Size = ct; Seqn = sqn; Base = bsHx; Hxls = [||] }
                emptyCxl, occ
            | Some firstCxl ->
                let refinedHxls = Array.except occ (Array.append [| firstCxl.Base |] firstCxl.Hxls)
                let rootCxl = { firstCxl with Hxls = refinedHxls }
                let updatedOcc = Array.concat [| occ; [|bsHx|]; refinedHxls |] |> hxlUni 1
                rootCxl, updatedOcc

    /// <summary> Count open/exposed Hexels. </summary>
    let cxlExp (cxl : Cxl[]) (sqn : Sqn) (elv : int)= 
        let occ = cxl |> Array.map (fun x -> x.Hxls) |> Array.concat |> hxlUni 1 
        let occSet = hxlSet occ
        let cxlAvl (cx:Cxl) (sq:Sqn) (oc : System.Collections.Generic.HashSet<Hxl>) =
            let hx = cx.Hxls |> hxlUni 1 
            hx |> Array.filter(fun x -> (availableSet sq elv x oc) > 0) |> Array.length
        cxl |> Array.map (fun a -> cxlAvl a sqn occSet)

    /// <summary> Categorize constituent Hexels within a Coxel. </summary>
    let cxlHxl (cxl : Cxl) (elv : int) = 
        let bndSqn (sqn : Sqn) (elv : int) (hxo : Hxl[]) = 
            let arr (hxl : Hxl[]) (opt : bool) = 
                let startNode = Array.last hxl
                let initialAvail = Set.ofArray hxl |> Set.remove startNode
                
                let rec loop current avail acc cnt =
                    match cnt with
                    | c when c <= 1 -> List.rev acc |> List.toArray
                    | _ ->
                        let adj = adjacent sqn current
                        let validAdj = adj |> Array.filter (fun x -> Set.contains x avail)
                        let nextOpt = match opt with | true -> Array.tryLast validAdj | false -> Array.tryHead validAdj
                        match nextOpt with
                        | Some nxt ->
                            loop nxt (Set.remove nxt avail) (nxt :: acc) (cnt - 1)
                        | None -> List.rev acc |> List.toArray

                loop startNode initialAvail [startNode] hxl.Length

            let hxl = hxo |> Array.sortByDescending (fun x -> available sqn elv x hxo)
            let a1 = match Array.isEmpty hxl with | true -> [||] | false -> arr hxl true
            let ar1 = match Array.length a1 = Array.length hxl with | true -> a1 | false -> arr hxl false

            match Array.isEmpty hxo with
            | true -> [||]
            | false ->
                match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with
                | true -> Array.rev ar1
                | false -> Array.rev (hxlUni 1 ar1)

        let cntSqn (sqn : Sqn) (elv : int) (hxo : Hxl[]) =      
            let hxl = hxlUni 1 hxo
            let ctSq (hxlArr : Hxl[]) = 
                let startNode = Array.head hxlArr
                let initialAvail = Set.ofArray hxlArr |> Set.remove startNode
                
                let rec loop current avail acc cnt =
                    match cnt with
                    | c when c <= 1 -> List.rev acc |> List.toArray
                    | _ ->
                        let d = (adjacent sqn current) |> Array.tail
                        let e = d |> Array.tryFind (fun x -> Set.contains x avail)
                        match e with
                        | Some nxt ->
                            loop nxt (Set.remove nxt avail) (nxt :: acc) (cnt - 1)
                        | None -> List.rev acc |> List.toArray

                loop startNode initialAvail [startNode] hxlArr.Length

            let hxlSort = hxl |> Array.sortByDescending (fun x -> available sqn elv x hxl)
            let cnt = Array.length(hxlSort)
            let arr = match Array.isEmpty hxlSort with | true -> [||] | false -> ctSq hxlSort
            let ar1 = match cnt = Array.length(arr) with | true -> arr | false -> ctSq (Array.rev hxlSort)

            match Array.isEmpty hxo with
            | true -> [||]
            | false ->
                match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with
                | true -> ar1
                | false -> hxlUni 1 ar1

        let allHxlsAV = cxl.Hxls |> hxlUni 1
        let avrv = cxl.Hxls |> Array.partition (fun x -> x = AV(hxlCrd x))
        let rv01 = (snd avrv) |> Array.partition (fun x -> available cxl.Seqn elv (AV(hxlCrd x)) allHxlsAV < 1)
        let av01 = match Array.isEmpty (snd rv01) with | true -> avrv |> fst |> bndSqn cxl.Seqn elv | false -> avrv |> fst |> cntSqn cxl.Seqn elv
        let br01 = match Array.isEmpty (fst rv01) with | true -> rv01 |> snd |> bndSqn cxl.Seqn elv | false -> rv01 |> snd |> cntSqn cxl.Seqn elv
             
        let pr01 = 
            match Array.isEmpty av01, Array.isEmpty br01 with
            | true, _ -> br01
            | false, true -> av01
            | false, false -> 
                let isAdj = adjacent cxl.Seqn (Array.last av01) |> hxlUni 2 |> Array.contains (Array.head br01)
                match isAdj with
                | true -> Array.append av01 br01
                | false -> Array.append av01 (Array.rev br01)

        let pr02 = 
            match pr01 with
            | [| |] | [| _ |] | [| _; _ |] -> pr01 
            | _ ->
                let (x1, y1, _) = hxlCrd (Array.last pr01)
                let (x2, y2, _) = hxlCrd (Array.head pr01)
                let (x3, y3, _) = hxlCrd pr01.[1]
                let gs = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
                match sign gs with
                | -1 -> pr01
                | 0  -> match x2 > x1 with | true -> pr01 | false -> Array.rev pr01
                | _  -> Array.rev pr01

        {| Base = cxl.Base; Hxls = cxl.Hxls; Core = rv01 |> fst; Prph = pr02; Brdr = br01; Avbl = av01 |}  

    /// <summary> Coxel Offseted Boundary Wrap </summary>
    let (|Collinear|Turning|) (p1: int * int, p2: int * int, p3: int * int) =
        let (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
        let crossProduct = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
        if crossProduct = 0 then Collinear else Turning

    let cxlPrm (cxl : Cxl) (elv : int) =
        let rec clean points =
            match points with
            | p1 :: p2 :: p3 :: rest ->
                match (p1, p2, p3) with
                | Collinear -> clean (p1 :: p3 :: rest)
                | Turning   -> p1 :: clean (p2 :: p3 :: rest)
            | _ -> points

        let outside = hxlOfs cxl.Seqn elv cxl.Hxls
        let insideSet = (hxlSet cxl.Hxls).ToFrozenSet()
        
        outside 
        |> Array.collect (fun hout ->
            let (ox, oy, _) = hxlCrd hout
            adjacent cxl.Seqn hout 
            |> Array.choose (fun n -> 
                let (ix, iy, _) = hxlCrd n
                if insideSet.Contains(AV(ix, iy, elv)) then
                    Some ( (ox + ix) / 2, (oy + iy) / 2 )
                else None)
        )
        |> Array.distinct
        |> Array.toList
        |> clean
        |> List.toArray

    /// <summary> Coxel Center </summary>
    let cxlCnt (cxl : Cxl): int * int = 
        match cxl.Hxls with
        | [||] -> (-10, -10) 
        | hxls ->
            let hxXY = hxls |> Array.map (fun a -> let x, y, _ = hxlCrd a in x, y)
            let numPoints = float hxXY.Length
            let sumX = hxXY |> Array.sumBy (fst >> float)
            let sumY = hxXY |> Array.sumBy (snd >> float)
            let centerX = sumX / numPoints
            let centerY = sumY / numPoints
            
            let closestHxl = hxls |> Array.minBy (fun hxl ->
                let x, y, _ = hxlCrd hxl
                let dx = float x - centerX
                let dy = float y - centerY
                dx*dx + dy*dy)
            let finalX, finalY, _ = hxlCrd closestHxl
            (finalX, finalY)

    /// <summary> Functional Adjacency Matrix Calculation. </summary>
    let cxlAdj (cxls: Cxl[]) =
        match cxls with
        | [||] -> [||], [||]
        | _ ->
            let sqn = cxls.[0].Seqn
            let elv = 0 
            let getFullHxls (c: Cxl) = Array.append [|c.Base|] c.Hxls |> hxlUni 1
            let coxelSets = cxls |> Array.map (fun c -> (getFullHxls c).ToFrozenSet())
            let allHalos = cxls |> Array.map (fun c ->
                let roomHxls = getFullHxls c
                roomHxls 
                |> Array.collect (adjacent sqn)
                |> Array.map (fun h -> AV(hxlCrd h))
                |> Array.distinct
                |> Array.filter (fun h -> not (Array.contains h roomHxls)))

            let names = cxls |> Array.map (fun c -> prpVlu c.Name)
            let matrix = allHalos |> Array.mapi (fun i halo ->
                coxelSets |> Array.mapi (fun j otherSet ->
                    if i = j then false else halo |> Array.exists (fun h -> otherSet.Contains(h))))
            names, matrix
