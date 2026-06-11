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

        let cnt = bas |> Array.map snd |> function [||] -> 0 | x -> Array.max x
        
        let acc = bas |> Array.map (fun x -> 
            let list = System.Collections.Generic.List<Hxl * int>()
            list.Add(x)
            list)
        
        let initialOcc = Array.append occ (getHxls bas)
        let occSet = hxlSet initialOcc
            
        let rec clsts (hxo : (Hxl * int)[]) (elv : int) (occSet : System.Collections.Generic.HashSet<Hxl>) (acc : System.Collections.Generic.List<Hxl * int>[]) (cnt : int) = 
            match cnt with 
            | c when c < 1 -> acc
            | _ -> 
                let hx1 = 
                    acc |> Array.mapi (fun i row ->
                        let (_, count) = hxo.[i]
                        
                        let rec findPoint j =
                            match j < row.Count with
                            | false -> None
                            | true ->
                                let h, _ = row.[j]
                                match availableSet sqn elv h occSet > 0 with
                                | true -> Some h
                                | false -> findPoint (j + 1)
                        
                        match findPoint 0 with
                        | Some h -> (h, count - 1)
                        | None   -> (hxlVld sqn (RV(0,0,elv)), 0xFFFFFFFF)
                    )

                let inc = incrementsSet sqn elv hx1 occSet
                                
                let rec updateAcc i =
                    match i < acc.Length with
                    | false -> ()
                    | true ->
                        let newEl = inc.[i]
                        acc.[i].Add(newEl)
                        let h, _ = newEl
                        let x, y, z = hxlCrd h
                        occSet.Add(AV(x, y, z)) |> ignore
                        updateAcc (i + 1)
                updateAcc 0
                
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

    /// <summary> Generate child coxels. </summary>
    let coxelChildren (sqn : Sqn) (elv : int) (bsCx : Cxl) (tre : (Prp*Prp*Prp)[]) (occ : Hxl[]) =
        let cnt = (Array.length tre) - 1
        
        let rec growParent (hxls: Hxl[]) =
            let chHx = hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
            match chHx.Length >= cnt with
            | true -> hxls, chHx
            | false ->
                let nextRing = 
                    hxls 
                    |> hxlUni 1 
                    |> Array.collect (adjacent sqn)
                    |> Array.distinct
                    |> Array.except (Array.append occ hxls |> hxlUni 1)
                
                match nextRing.Length = 0 with
                | true -> hxls, chHx
                | false -> 
                    let newHxls = Array.append hxls nextRing |> hxlChk sqn elv occ
                    growParent newHxls

        let finalParentHxls, chHx = growParent bsCx.Hxls
        let updatedBsCx = { bsCx with Hxls = finalParentHxls }
        
        let chBs = 
            match (Array.length chHx) >= cnt with 
            | true -> 
                let divs = match cnt > 0 with | true -> (Array.length chHx) / cnt | false -> 1
                chHx |> Array.chunkBySize divs |> Array.map (fun chunk -> chunk.[chunk.Length / 2]) |> Array.take cnt
            | false -> chHx
                
        let ini = Array.map2 (fun a (b, c, d) -> a, b, c, d) chBs (Array.tail tre |> Array.take chBs.Length)
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
                let b2 = match baseCheck with | [||] -> x.Base | _ -> baseCheck.[0]
                { x with Hxls = h2; Base = b2 })
                
        updatedBsCx, finalCxc, chOc1

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
                    let parts = bsAtr.Split ','
                    match parts with
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
                        let nextOpt = match opt with | true -> Array.tryLast validAdj | false -> Array.tryHead validAdj
                        match nextOpt with
                        | Some nxt ->
                            acc.Add(nxt)
                            availableSet.Remove(nxt) |> ignore
                            loop nxt (cnt - 1)
                        | None -> ()

                loop startNode hxl.Length
                acc.ToArray()

            let hxl = hxo |> Array.sortByDescending (fun x -> available sqn elv x hxo)
            let a1 = match hxl with | [||] -> [||] | _ -> arr hxl true
            let ar1 = match Array.length a1 = Array.length hxl with | true -> a1 | false -> arr hxl false

            match hxo with
            | [||] -> [||]
            | _ ->
                match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with
                | true -> Array.rev ar1
                | false -> Array.rev (hxlUni 1 ar1)

        let cntSqn (sqn : Sqn) (elv : int) (hxo : Hxl[]) =      
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
            let arr = match hxl with | [||] -> [||] | _ -> ctSq hxl
            let ar1 = match cnt = Array.length(arr) with | true -> arr | false -> ctSq (Array.rev hxl)

            match hxo with
            | [||] -> [||]
            | _ ->
                match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with
                | true -> ar1
                | false -> hxlUni 1 ar1

        let allHxlsAV = cxl.Hxls |> hxlUni 1
        let innerOccSet = hxlSet allHxlsAV
        let avrv = cxl.Hxls |> Array.partition (fun x -> x = AV(hxlCrd x))
        let rv01 = (snd avrv) |> Array.partition (fun x -> availableSet cxl.Seqn elv (AV(hxlCrd x)) innerOccSet < 1)
        let av01 = match snd rv01 with | [||] -> avrv |> fst |> bndSqn cxl.Seqn elv | _ -> avrv |> fst |> cntSqn cxl.Seqn elv
        let br01 = match fst rv01 with | [||] -> rv01 |> snd |> bndSqn cxl.Seqn elv | _ -> rv01 |> snd |> cntSqn cxl.Seqn elv
             
        let pr01 = 
            match av01 with
            | [||] -> br01
            | _ ->
                match br01 with
                | [||] -> av01
                | _ -> 
                    let isAdj = adjacent cxl.Seqn (Array.last av01) |> hxlUni 2 |> Array.contains (Array.head br01)
                    match isAdj with | true -> Array.append av01 br01 | false -> Array.append av01 (Array.rev br01)

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
        match crossProduct = 0 with | true -> Collinear | false -> Turning

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
                match insideSet.Contains(AV(ix, iy, elv)) with
                | true -> Some ( (ox + ix) / 2, (oy + iy) / 2 )
                | false -> None)
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
                    match i = j with | true -> false | false -> halo |> Array.exists (fun h -> otherSet.Contains(h))))
            names, matrix
