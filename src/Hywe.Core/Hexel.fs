namespace Hywe.Core

module Hexel =
    open System

    [<Struct>]
    type Point = { X: int; Y: int }

    /// <summary> Hexel types: Categorization based on location availabity. </summary>
    [<Struct>]
    type Hxl = 
        | AV of x: int * y: int * z: int
        | RV of x: int * y: int * z: int
        | EX of x: int * y: int * z: int

    /// <summary> Sequence specifies the orientation of hexels, the direction of flow of 
    /// adjacent hexels and the position of the first of the six adjaent hexels. </summary>
    [<Struct>]
    type Sqn =  
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW

    let (|Vertical|Horizontal|) sqn =
        match sqn with
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW 
        | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> Vertical
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE 
        | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW -> Horizontal

    /// <summary> Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel. </summary>
    let sequence (sqn:Sqn) =  
        match sqn with 
        | VRCWEE -> [| 0,0;  2, 0;  1,-2; -1,-2; -2, 0; -1, 2;  1, 2|]
        | VRCCEE -> [| 0,0;  2, 0;  1, 2; -1, 2; -2, 0; -1,-2;  1,-2|]
        | VRCWSE -> [| 0,0;  1,-2; -1,-2; -2, 0; -1, 2;  1, 2;  2, 0|]
        | VRCCSE -> [| 0,0;  1,-2;  2, 0;  1, 2; -1, 2; -2, 0; -1,-2|]
        | VRCWSW -> [| 0,0; -1,-2; -2, 0; -1, 2;  1, 2;  2, 0;  1,-2|]
        | VRCCSW -> [| 0,0; -1,-2;  1,-2;  2, 0;  1, 2; -1, 2; -2, 0|]
        | VRCWWW -> [| 0,0; -2, 0; -1, 2;  1, 2;  2, 0;  1,-2; -1,-2|]
        | VRCCWW -> [| 0,0; -2, 0; -1,-2;  1,-2;  2, 0;  1, 2; -1, 2|]
        | VRCWNW -> [| 0,0; -1, 2;  1, 2;  2, 0;  1,-2; -1,-2; -2, 0|]
        | VRCCNW -> [| 0,0; -1, 2; -2, 0; -1,-2;  1,-2;  2, 0;  1, 2|]
        | VRCWNE -> [| 0,0;  1, 2;  2, 0;  1,-2; -1,-2; -2, 0; -1, 2|]
        | VRCCNE -> [| 0,0;  1, 2; -1, 2; -2, 0; -1,-2;  1,-2;  2, 0|]
        | HRCWNN -> [| 0,0;  0, 2;  2, 1;  2,-1;  0,-2; -2,-1; -2, 1|]
        | HRCCNN -> [| 0,0;  0, 2; -2, 1; -2,-1;  0,-2;  2,-1;  2, 1|]
        | HRCWNE -> [| 0,0;  2, 1;  2,-1;  0,-2; -2,-1; -2, 1;  0, 2|]
        | HRCCNE -> [| 0,0;  2, 1;  0, 2; -2, 1; -2,-1;  0,-2;  2,-1|]
        | HRCWSE -> [| 0,0;  2,-1;  0,-2; -2,-1; -2, 1;  0, 2;  2, 1|]
        | HRCCSE -> [| 0,0;  2,-1;  2, 1;  0, 2; -2, 1; -2,-1;  0,-2|]
        | HRCWSS -> [| 0,0;  0,-2; -2,-1; -2, 1;  0, 2;  2, 1;  2,-1|]
        | HRCCSS -> [| 0,0;  0,-2;  2,-1;  2, 1;  0, 2; -2, 1; -2,-1|]
        | HRCWSW -> [| 0,0; -2,-1; -2, 1;  0, 2;  2, 1;  2,-1;  0,-2|]
        | HRCCSW -> [| 0,0; -2,-1;  0,-2;  2,-1;  2, 1;  0, 2; -2, 1|]
        | HRCWNW -> [| 0,0; -2, 1;  0, 2;  2, 1;  2,-1;  0,-2; -2,-1|]
        | HRCCNW -> [| 0,0; -2, 1; -2,-1;  0,-2;  2,-1;  2, 1;  0, 2|]

    let sqnArray = [|
        VRCWEE; VRCCEE; VRCWSE; VRCCSE; VRCWSW; VRCCSW; VRCWWW; VRCCWW; VRCWNW; VRCCNW; VRCWNE; VRCCNE
        HRCWNN; HRCCNN; HRCWNE; HRCCNE; HRCWSE; HRCCSE; HRCWSS; HRCCSS; HRCWSW; HRCCSW; HRCWNW; HRCCNW
    |]

    /// <summary> Identity Hexel. </summary>
    let identity (elv:int) = RV(0,0, elv)

    /// <summary> Extract coordinates from hexel. </summary>
    let inline hxlCrd (h: Hxl) =
        match h with
        | AV(x,y,z) -> x,y,z
        | RV(x,y,z) -> x,y,z
        | EX(x,y,z) -> x,y,z

    /// <summary> Converts an integer to a Base36 string. </summary>
    let toBase36 (value: int64) =
        let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let rec convert v acc =
            match v = 0L with
            | true -> match acc = "" with | true -> "0" | false -> acc
            | false -> convert (v / 36L) (string chars.[int (v % 36L)] + acc)
        let prefix = if value < 0L then "-" else ""
        prefix + convert (abs value) ""

    /// <summary> Valid Hexels. </summary>
    let hxlVld (sqn : Sqn) (hxl : Hxl) = 
        let validate s x y z =
            match s, x, y with
            | Vertical, a, b when b % 4 = 0 -> (a + (a % 2)), b, z
            | Vertical, a, b when a % 2 = 0 -> a + 1, (b + (b % 2)), z
            | Vertical, _, b                -> x, (b + (b % 2)), z
            | Horizontal, a, b when a % 4 = 0 -> a, (b + (b % 2)), z
            | Horizontal, a, b when b % 2 = 0 -> (a + (a % 2)), b + 1, z
            | Horizontal, a, _                -> (a + (a % 2)), y, z

        let x, y, z = hxlCrd hxl
        let vld = validate sqn x y z |> fun (x1, y1, z1) -> validate sqn x1 y1 z1
        match hxl with
        | AV _ -> AV vld
        | RV _ -> RV vld
        | EX _ -> EX vld

    /// <summary> Change all hexel types to a uniform type.</summary>
    let hxlUni (opt : int) (hxl : Hxl[]) = 
        let constructor x y z = 
                match opt with
                | 1 -> AV(x,y,z)
                | 2 -> RV(x,y,z)
                | 3 -> EX(x,y,z)
                | _ -> AV(x,y,z)
        hxl |> Array.map (fun h -> let (x,y,z) = hxlCrd h in constructor x y z)

    /// <summary> Create a HashSet of AV hexels for O(1) lookup. </summary>
    let hxlSet (hxls: Hxl seq) = 
        let set = System.Collections.Generic.HashSet<Hxl>()
        for h in hxls do
            let x,y,z = hxlCrd h
            set.Add(AV(x,y,z)) |> ignore
        set

    /// <summary> Get Hexel from Tuple. </summary>
    let getHxls (hxo : (Hxl*int)[]) = hxo |> Array.map fst

    /// <summary> Adjacent Hexels. </summary>
    let adjacent (sqn : Sqn) (hxo : Hxl) =
        let x, y, z = hxlCrd hxo
        sequence sqn |> Array.map (fun (a, b) -> AV(x + a, y + b, z))

    /// <summary> Increment Hexel. </summary>
    let increment (sqn : Sqn) (elv : int) (hxo : Hxl * int) (occ : Hxl[]) = 
        let occ = Array.concat [| occ; [|(fst hxo)|]; [|identity elv|] |] |> hxlUni 1
        match hxo with 
        | x,y when y >= 0 -> 
            let inc1 = x |> adjacent sqn |> Array.except occ
            let inc2 = match (Array.tryItem  1 inc1) with 
                            | Some a -> 
                                            let bl1 = Array.contains (Array.head inc1) (adjacent sqn a)
                                            match bl1 with 
                                            | true -> Array.tryHead inc1
                                            | false -> x |> adjacent sqn |> Array.except occ |> Array.tryLast
                            | None -> Array.tryHead inc1
            match inc2 with 
            | Some a -> a, y
            | None -> (hxlVld sqn (identity elv),-1)
        | _ -> (hxlVld sqn (identity elv),-1)

    /// <summary> Increment Hexel using HashSet for performance. </summary>
    let incrementSet (sqn : Sqn) (elv : int) (hxo : Hxl * int) (occ : System.Collections.Generic.HashSet<Hxl>) = 
        match hxo with 
        | x, y when y >= 0 -> 
            let adj = adjacent sqn x
            let inc1 = ResizeArray<Hxl>()
            if adj.Length > 1 then
                for i = 1 to adj.Length - 1 do
                    let n = adj.[i]
                    if not (occ.Contains(n)) && n <> x && n <> (identity elv) then inc1.Add(n)
            let inc2 = 
                match inc1.Count >= 2 with
                | true ->
                    let head = inc1.[0]
                    let next = inc1.[1]
                    let adjNext = adjacent sqn next
                    let mutable isAdj = false
                    for k = 0 to 6 do if adjNext.[k] = head then isAdj <- true
                    match isAdj with
                    | true -> Some head
                    | false -> Some inc1.[inc1.Count - 1]
                | false -> if inc1.Count = 1 then Some inc1.[0] else None
            match inc2 with 
            | Some a -> a, y
            | None -> (hxlVld sqn (identity elv), -1)
        | _ -> (hxlVld sqn (identity elv), -1)

    /// <summary> Available Adjacent Hexels. </summary>
    let available (sqn : Sqn) (elv : int) (hxo : obj) (occ : Hxl[]) =  
        let occ = occ |> hxlUni 1
        let hx1 = match hxo with 
                    | :? (Hxl*int) as (a,_) -> a
                    | :? Hxl as b ->  b
                    | _ -> identity elv
        hx1 |> adjacent sqn |> Array.except (Array.append occ [|hx1|]) |> Array.length

    /// <summary> Available Adjacent Hexels using HashSet for performance. </summary>
    let availableSet (sqn : Sqn) (elv : int) (hxo : Hxl) (occ : System.Collections.Generic.HashSet<Hxl>) =  
        let adj = adjacent sqn hxo
        let mutable count = 0
        if adj.Length > 1 then
            for i = 1 to adj.Length - 1 do
                let n = adj.[i]
                if not (occ.Contains(n)) && n <> hxo then count <- count + 1
        count

    /// <summary> Optimized version of hxlChk using a HashSet for performance. </summary>
    let hxlChkSet (sqn : Sqn) (elv : int) (occSet : System.Collections.Generic.HashSet<Hxl>) (hxl : Hxl[]) = 
        hxl |> Array.map (fun x -> 
            let (x1,y1,z1) = hxlCrd x
            match (x = EX(x1,y1,z1)) with 
            | true -> x
            | false -> 
                if (availableSet sqn elv x occSet) < 1 then RV(x1,y1,z1) else AV(x1,y1,z1))

    let hxlChk (sqn : Sqn) (elv : int) (occ : Hxl[]) (hxl : Hxl[]) = 
        let occSet = hxlSet (Array.append occ hxl)
        hxlChkSet sqn elv occSet hxl

    ///<summary> Add Hexel at Narrow Bridge. </summary>
    let hxlFil (sqn : Sqn) (elv : int) (hxl : Hxl[]) = 
        let hxx = hxl |> hxlUni 1     
        let hx1 = hxx |> Array.map (fun x -> adjacent sqn x)
        let in1 = Array.map (fun z -> Array.map (fun y -> Array.tryFindIndex (fun x -> x = y) z) hxx) hx1
        let in2 = in1 |> Array.map (fun x -> x |> Array.choose id) |> Array.map Array.sort |> Array.map (fun x -> Array.except [|0|] x)
        let in3 = in2 |> Array.map (fun x -> match x |> Array.toList with | [] -> [||] | [a] -> [| a |] | a :: _ -> [| a .. Array.last x |])
        let in4 = match in2.Length = in3.Length with | true -> Array.map2 (fun x y -> Array.except x y) in2 in3 | false -> [||]
        let in5 = in4 |> Array.map (fun x -> match x |> Array.toList with | [] -> [||] | [_] -> x | _ -> Array.except x [| Array.head x .. Array.last x |])
        let hx2 = match hx1.Length = in5.Length with 
                    | true -> Array.map2 (fun x y -> match Array.toList x, Array.tryHead y with | _, Some yi when yi >= 0 && yi < x.Length -> [| Array.get x yi |] | _ -> [||]) hx1 in5 |> Array.concat
                    | false -> [||]
        Array.append hxx hx2 |> hxlChk sqn elv [||]

    /// <summary> Increment Hexels using a HashSet for occupancy. </summary>
    let incrementsSet (sqn : Sqn) (elv : int) (hxo : (Hxl*int)[]) (occSet : System.Collections.Generic.HashSet<Hxl>) = 
        let replaceDuplicateSet (sqn : Sqn) (elv : int) (hxo : (Hxl*int)[]) (inc : (Hxl*int)[]) (occSet : System.Collections.Generic.HashSet<Hxl>) =   
            let localBatchSet = System.Collections.Generic.HashSet<Hxl>()
            Array.map2 (fun (hxBase, weight) (hxInc, _) ->
                if hxInc <> (hxlVld sqn (identity elv)) && not (localBatchSet.Contains(hxInc)) then
                    localBatchSet.Add(hxInc) |> ignore
                    hxInc, weight
                else
                    let next = incrementSet sqn elv (hxBase, weight) occSet
                    if fst next <> (hxlVld sqn (identity elv)) && not (localBatchSet.Contains(fst next)) then
                        localBatchSet.Add(fst next) |> ignore
                        next
                    else (hxlVld sqn (identity elv)), -1
            ) hxo inc
        for (h, _) in hxo do let (x,y,z) = hxlCrd h in occSet.Add(AV(x,y,z)) |> ignore
        let inc = hxo |> Array.map (fun st -> 
                let next = incrementSet sqn elv st occSet
                if fst next <> (hxlVld sqn (identity elv)) then let (x,y,z) = hxlCrd (fst next) in occSet.Add(AV(x,y,z)) |> ignore
                next)
        replaceDuplicateSet sqn elv hxo inc occSet

    /// <summary> Increment Hexels. </summary>
    let increments (sqn : Sqn) (elv : int) (hxo : (Hxl*int)[]) (occ : Hxl[]) = 
        let occSet = hxlSet occ
        incrementsSet sqn elv hxo occSet

    /// <summary> Boundary Hexels Ring. </summary>
    let bndSqn (sqn : Sqn) (elv : int) (hxo : Hxl[]) = 
        let rec arr (sqn : Sqn) (elv : int) (hxl : Hxl[]) (acc : Hxl[]) (cnt : int) (opt : bool) = 
            match cnt with 
            | a when cnt <= 1 -> acc
            | _ -> 
                let hxl = Array.except acc hxl
                let hx1 = ((Array.filter (fun x -> Array.contains x hxl) (adjacent sqn (Array.last acc))))                
                let hx2 = match opt with | false -> Array.tryHead hx1 | true -> Array.tryLast hx1
                let hx3 = match hx2 with | Some a -> [|a|] | None -> [||]
                arr sqn elv hxl (Array.append acc hx3) (cnt-1) opt
        let hxlOccSet = System.Collections.Generic.HashSet<Hxl>(hxo |> Array.map (fun x -> AV(hxlCrd x)))
        let hxl = hxo |> Array.sortByDescending (fun x -> availableSet sqn elv x hxlOccSet)
        let a1 = match hxl with | [||] -> [||] | _ -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) true
        let ar1 = match (Array.length a1) = Array.length hxl with | true -> a1 | false -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) false
        let ar2 = match hxo with | [||] -> [||] | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with | true -> ar1 | false -> hxlUni 2 ar1
        let ar3 = Array.windowed 2 ar2
        let bln = Array.map(fun x -> let (x1,y1,_) = hxlCrd (Array.head x) in let (x2,y2,_) = hxlCrd (Array.last x) in (x2 - x1 >= 0) && (y1 - y2 >= 0)) ar3
        match Array.contains false bln with | true -> Array.rev ar2 | false -> ar2

    /// <summary> Hexel Ring Segment Sequence. </summary>
    let cntSqn (sqn : Sqn) (elv : int) (hxo : Hxl[]) =      
        let hxl = hxlUni 1 hxo
        let rec ctSq (sqn : Sqn) (hxl : Hxl[]) (acc : Hxl[]) (cnt : int) = 
            match cnt with 
            | x when x<=1 -> acc
            | _ -> 
                    let b = Array.last acc
                    let hxl = Array.except [|b|] hxl
                    let d = (adjacent sqn b) |> Array.tail
                    let e = d |> Array.filter (fun x -> Array.contains x hxl) |> Array.tryHead
                    let f = match e with | Some a -> [|a|] | None -> [||]
                    ctSq sqn hxl (Array.append acc f) (cnt-1)
        let hxlOccSet = System.Collections.Generic.HashSet<Hxl>(hxl)
        let hxlSort = hxl |> Array.sortByDescending (fun x -> availableSet sqn elv x hxlOccSet)
        let cnt = Array.length(hxlSort)
        let arr =  match hxlSort with | [||] -> [||] | _ -> ctSq sqn hxlSort ([|Array.head hxlSort|]) cnt
        let ar1 = match cnt = Array.length(arr) with | true -> arr | false -> ctSq sqn (Array.rev hxlSort) ([|Array.last hxlSort|]) cnt
        match hxo with | [||] -> [||] | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with | true -> ar1 | false -> hxlUni 2 ar1

    /// <summary> Hexel Ring Offset. </summary>
    let hxlOfs (sqn : Sqn) (elv : int) (hxl : Hxl[]) = 
        hxl |> hxlUni 1 |> Array.map(fun x -> adjacent sqn x) |> Array.concat |> Array.distinct |> Array.except (hxlUni 1 hxl) |> cntSqn sqn elv

    /// <summary> Restored Hexel Types </summary>
    let hxlRst (org : Hxl[]) (hxl : Hxl[]) =
        let crd = Array.map hxlCrd hxl
        org |> Array.filter (fun x -> (crd |> Array.contains (hxlCrd x)))
