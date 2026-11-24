namespace Hywe.Core

/// <summary> 
/// Hexel (Hygrid Engine) 
/// Implements the base coordinate system and logic for the Hygrid Woven Ensemble. 
/// Acts as the fundamental unit (Hexel) in the architectural hierarchy: Hexel-Coxel-Xyxel-Zaxel.
/// </summary>

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

    let sqnToString (sqn: Sqn) =
        match sqn with
        | VRCWEE -> "VRCWEE" | VRCCEE -> "VRCCEE" | VRCWSE -> "VRCWSE" | VRCCSE -> "VRCCSE" 
        | VRCWSW -> "VRCWSW" | VRCCSW -> "VRCCSW" | VRCWWW -> "VRCWWW" | VRCCWW -> "VRCCWW" 
        | VRCWNW -> "VRCWNW" | VRCCNW -> "VRCCNW" | VRCWNE -> "VRCWNE" | VRCCNE -> "VRCCNE"
        | HRCWNN -> "HRCWNN" | HRCCNN -> "HRCCNN" | HRCWNE -> "HRCWNE" | HRCCNE -> "HRCCNE" 
        | HRCWSE -> "HRCWSE" | HRCCSE -> "HRCCSE" | HRCWSS -> "HRCWSS" | HRCCSS -> "HRCCSS" 
        | HRCWSW -> "HRCWSW" | HRCCSW -> "HRCCSW" | HRCWNW -> "HRCWNW" | HRCCNW -> "HRCCNW"


    /// <summary>
    /// Returns the lattice origin anchor cell at the specified elevation: RV(0, 0, elv).
    /// </summary>
    /// <example>
    /// <code>
    /// let origin = Hexel.identity 0 // RV(0, 0, 0)
    /// </code>
    /// </example>
    let identity (elv:int) = RV(0,0, elv)

    /// <summary>
    /// Deconstructs any Hxl instance and returns its raw (x, y, z) integer coordinate triple.
    /// </summary>
    /// <example>
    /// <code>
    /// let (x, y, z) = Hexel.hxlCrd (Hexel.Hxl.AV(2, 4, 0))
    /// </code>
    /// </example>
    let inline hxlCrd (h: Hxl) =
        match h with
        | AV(x,y,z) -> x,y,z
        | RV(x,y,z) -> x,y,z
        | EX(x,y,z) -> x,y,z

    /// <summary>
    /// Converts an integer coordinate or index to HYWE's compact Base34 string representation (excludes 'I' and 'O').
    /// </summary>
    /// <example>
    /// <code>
    /// let b34 = Hexel.toBase34 35L // "11"
    /// </code>
    /// </example>
    let toBase34 (value: int64) =
        let chars = "0123456789ABCDEFGHJKLMNPQRSTUVWXYZ"
        let rec convert v acc =
            match v = 0L with
            | true -> match acc = "" with | true -> "0" | false -> acc
            | false -> convert (v / 34L) (string chars.[int (v % 34L)] + acc)
        let prefix = match value < 0L with | true -> "-" | false -> ""
        prefix + convert (abs value) ""

    /// <summary>
    /// Validates and snaps an arbitrary coordinate to the nearest legal discrete lattice cell based on modulo parity rules.
    /// </summary>
    /// <example>
    /// <code>
    /// let snapped = Hexel.hxlVld Hexel.Sqn.VRCWEE (Hexel.Hxl.AV(1, 1, 0))
    /// </code>
    /// </example>
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

    /// <summary>
    /// Change all hexel types in an array to a uniform occupancy state (1 = AV, 2 = RV, 3 = EX).
    /// </summary>
    /// <example>
    /// <code>
    /// let occupiedCells = Hexel.hxlUni 2 [| Hexel.Hxl.AV(0, 0, 0); Hexel.Hxl.AV(2, 0, 0) |]
    /// </code>
    /// </example>
    let hxlUni (opt : int) (hxl : Hxl[]) = 
        let constructor x y z = 
                match opt with
                | 1 -> AV(x,y,z)
                | 2 -> RV(x,y,z)
                | 3 -> EX(x,y,z)
                | _ -> AV(x,y,z)
        hxl |> Array.map (fun h -> let (x,y,z) = hxlCrd h in constructor x y z)

    /// <summary>
    /// Creates a System.Collections.Generic.HashSet of AV hexels for high-performance O(1) membership lookups.
    /// </summary>
    /// <example>
    /// <code>
    /// let occSet = Hexel.hxlSet [| Hexel.Hxl.AV(0, 0, 0); Hexel.Hxl.AV(2, 0, 0) |]
    /// let isOccupied = occSet.Contains(Hexel.Hxl.AV(0, 0, 0)) // true
    /// </code>
    /// </example>
    let hxlSet (hxls: Hxl seq) = 
        System.Collections.Generic.HashSet<Hxl>(hxls |> Seq.map (fun h -> let x,y,z = hxlCrd h in AV(x,y,z)))

    /// <summary>
    /// Extracts the underlying Hxl array from an array of weighted (Hxl * int) tuples.
    /// </summary>
    /// <example>
    /// <code>
    /// let hexels = Hexel.getHxls [| (Hexel.Hxl.AV(0, 0, 0), 10); (Hexel.Hxl.AV(2, 0, 0), 5) |]
    /// </code>
    /// </example>
    let getHxls (hxo : (Hxl*int)[]) = hxo |> Array.map fst

    /// <summary>
    /// Computes the 7 neighboring hexel cells centered on the host hexel, ordered by sequence heading and chirality.
    /// </summary>
    /// <example>
    /// <code>
    /// let neighbors = Hexel.adjacent Hexel.Sqn.VRCWEE (Hexel.Hxl.AV(0, 0, 0))
    /// </code>
    /// </example>
    let adjacent (sqn : Sqn) (hxo : Hxl) =
        let x, y, z = hxlCrd hxo
        sequence sqn |> Array.map (fun (a, b) -> AV(x + a, y + b, z))

    /// <summary>
    /// Computes the next optimal expansion hexel for a growing spatial cluster while avoiding obstacles.
    /// </summary>
    /// <example>
    /// <code>
    /// let next = Hexel.increment Hexel.Sqn.VRCWEE 0 (Hexel.Hxl.AV(0, 0, 0), 1) [||]
    /// </code>
    /// </example>
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
            let rec getInc i (acc: Hxl list) =
                match i < 1 with
                | true -> acc
                | false ->
                    let n = adj.[i]
                    let nextAcc = match not (occ.Contains(n)) && n <> x && n <> identity elv with | true -> n :: acc | false -> acc
                    getInc (i - 1) nextAcc
            
            let inc1List = match adj.Length > 1 with | true -> getInc (adj.Length - 1) [] | false -> []
            let inc2 = 
                match inc1List with
                | head :: next :: _ ->
                    let isAdj = adjacent sqn next |> Array.contains head
                    match isAdj with | true -> Some head | false -> Some (List.last inc1List)
                | [head] -> Some head
                | [] -> None
            match inc2 with 
            | Some a -> a, y
            | None -> (hxlVld sqn (identity elv), -1)
        | _ -> (hxlVld sqn (identity elv), -1)

    /// <summary>
    /// Counts free neighbor cells around a candidate hexel given an array of occupied obstacle cells.
    /// </summary>
    /// <example>
    /// <code>
    /// let freeCount = Hexel.available Hexel.Sqn.VRCWEE 0 (Hexel.Hxl.AV(0, 0, 0)) [||]
    /// </code>
    /// </example>
    let available (sqn : Sqn) (elv : int) (hxo : obj) (occ : Hxl[]) =  
        let occ = occ |> hxlUni 1
        let hx1 = match hxo with 
                    | :? (Hxl*int) as (a,_) -> a
                    | :? Hxl as b ->  b
                    | _ -> identity elv
        hx1 |> adjacent sqn |> Array.except (Array.append occ [|hx1|]) |> Array.length

    /// <summary>
    /// High-performance O(1) variant of available using a HashSet of occupied cells.
    /// </summary>
    /// <example>
    /// <code>
    /// let occSet = Hexel.hxlSet [| Hexel.Hxl.AV(2, 0, 0) |]
    /// let freeCount = Hexel.availableSet Hexel.Sqn.VRCWEE 0 (Hexel.Hxl.AV(0, 0, 0)) occSet
    /// </code>
    /// </example>
    let availableSet (sqn : Sqn) (elv : int) (hxo : Hxl) (occ : System.Collections.Generic.HashSet<Hxl>) =  
        let adj = adjacent sqn hxo
        let rec countFree i acc =
            match i >= adj.Length with
            | true -> acc
            | false ->
                let n = adj.[i]
                countFree (i + 1) (match not (occ.Contains(n)) && n <> hxo with | true -> acc + 1 | false -> acc)
        match adj.Length > 1 with | true -> countFree 1 0 | false -> 0

    /// <summary> Optimized version of hxlChk using a HashSet for performance. </summary>
    let hxlChkSet (sqn : Sqn) (elv : int) (occSet : System.Collections.Generic.HashSet<Hxl>) (hxl : Hxl[]) = 
        hxl |> Array.map (fun x -> 
            let (x1,y1,z1) = hxlCrd x
            match (x = EX(x1,y1,z1)) with 
            | true -> x
            | false -> 
                match (availableSet sqn elv x occSet) < 1 with | true -> RV(x1,y1,z1) | false -> AV(x1,y1,z1))

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
                match hxInc <> hxlVld sqn (identity elv) && localBatchSet.Add(hxInc) with
                | true -> hxInc, weight
                | false ->
                    let next = incrementSet sqn elv (hxBase, weight) occSet
                    match fst next <> hxlVld sqn (identity elv) && localBatchSet.Add(fst next) with
                    | true -> next
                    | false -> (hxlVld sqn (identity elv)), -1
            ) hxo inc
        hxo |> Array.iter (fun (h, _) -> let x,y,z = hxlCrd h in occSet.Add(AV(x,y,z)) |> ignore)
        let inc = hxo |> Array.map (fun st -> 
                let next = incrementSet sqn elv st occSet
                match fst next <> hxlVld sqn (identity elv) with
                | true -> let x,y,z = hxlCrd (fst next) in occSet.Add(AV(x,y,z)) |> ignore
                | false -> ()
                next)
        replaceDuplicateSet sqn elv hxo inc occSet

    /// <summary>
    /// Advances multiple active cluster seeds simultaneously while preventing mutual collision.
    /// </summary>
    /// <example>
    /// <code>
    /// let activeSeeds = [| (Hexel.Hxl.AV(0, 0, 0), 10); (Hexel.Hxl.AV(8, 0, 0), 5) |]
    /// let nextSeeds = Hexel.increments Hexel.Sqn.VRCWEE 0 activeSeeds [||]
    /// </code>
    /// </example>
    let increments (sqn : Sqn) (elv : int) (hxo : (Hxl*int)[]) (occ : Hxl[]) = 
        let occSet = hxlSet occ
        incrementsSet sqn elv hxo occSet

    /// <summary>
    /// Traces and returns the ordered continuous outer boundary perimeter hexels of a cluster.
    /// </summary>
    /// <example>
    /// <code>
    /// let boundary = Hexel.bndSqn Hexel.Sqn.VRCWEE 0 [| Hexel.Hxl.AV(0, 0, 0); Hexel.Hxl.AV(2, 0, 0) |]
    /// </code>
    /// </example>
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

    /// <summary>
    /// Sorts a collection of hexels into a continuous sequence path.
    /// </summary>
    /// <example>
    /// <code>
    /// let path = Hexel.cntSqn Hexel.Sqn.VRCWEE 0 [| Hexel.Hxl.AV(0, 0, 0); Hexel.Hxl.AV(2, 0, 0) |]
    /// </code>
    /// </example>
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

    /// <summary>
    /// Calculates a single-ring exterior offset/buffer surrounding a collection of hexels.
    /// </summary>
    /// <example>
    /// <code>
    /// let buffer = Hexel.hxlOfs Hexel.Sqn.VRCWEE 0 [| Hexel.Hxl.AV(0, 0, 0) |]
    /// </code>
    /// </example>
    let hxlOfs (sqn : Sqn) (elv : int) (hxl : Hxl[]) = 
        hxl |> hxlUni 1 |> Array.map(fun x -> adjacent sqn x) |> Array.concat |> Array.distinct |> Array.except (hxlUni 1 hxl) |> cntSqn sqn elv

    /// <summary>
    /// Filters original hexels, retaining only those whose coordinates match any in the target array.
    /// </summary>
    /// <example>
    /// <code>
    /// let filtered = Hexel.hxlRst originalHexels candidateHexels
    /// </code>
    /// </example>
    let hxlRst (org : Hxl[]) (hxl : Hxl[]) =
        let crd = Array.map hxlCrd hxl
        org |> Array.filter (fun x -> (crd |> Array.contains (hxlCrd x)))
