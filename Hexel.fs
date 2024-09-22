/// <summary> Hexel is a location representing an irregular hexagonal module.
/// Collections of hexels in a hexagonal grid form Coxels.
/// A hexel can have a maximum of six neighbouring/adjacent hexels.
/// All neighbouring hexels share at least one common edge </summary>
module Hexel
///

/// <summary> Hexel types: Categorization based on location availabity. </summary>
///<typeparam name="AV"> AvaiIable Hexels. </typeparam>
///<typeparam name="RV"> Reserved Hexels. </typeparam>
type Hxl = 
    | AV of x:int * y:int * z:int
    | RV of x:int * y:int * z:int
    | EX of x:int * y:int * z:int
///

/// <summary> Sequence specifies the orientation of hexels, the direction of flow of 
/// adjacent hexels and the position of the first of the six adjaent hexels. </summary>
/// <remarks> 
/// <para>
/// Horizontal refers to a Flat Top hexagonal grid.
/// 
///  ___ N N ___     ___     ___     ___     ___     ___
/// /N W\___/N E\___/   \___/   \___/   \___/   \___/   \
/// \___/   \___/   \___/   \___/   \___/   \___/   \___/
/// /S W\___/S E\___/   \___/   \___/   \___/   \___/   \
/// \___/S S\___/   \___/   \___/   \___/   \___/   \___/
/// 
/// </para>
/// <para>
/// Vertical refers to a Pointy Top hexagonal grid. 
/// 
///   |NW |NE|   |   |   |   |   |   |   |   |   |   |
///  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\ 
/// /  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \
/// |WW |   |EE |   |   |   |   |   |   |   |   |   |  |
/// \  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /
///  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/ 
///   |SW |SE|   |   |   |   |   |   |   |   |   |   |
/// 
/// </para>
/// </remarks>
/// <typeparam name="VRCWEE"> Orientation:Vertical, Flow:Clockwise, Start:East </typeparam>
/// <typeparam name="VRCCEE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:East </typeparam>
/// <typeparam name="VRCWSE"> Orientation:Vertical, Flow:Clockwise, Start:South-East </typeparam>
/// <typeparam name="VRCCSE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-East </typeparam>
/// <typeparam name="VRCWSW"> Orientation:Vertical, Flow:Clockwise, Start:South-West </typeparam>
/// <typeparam name="VRCCSW"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-West </typeparam>
/// <typeparam name="VRCWWW"> Orientation:Vertical, Flow:Clockwise, Start:West </typeparam>
/// <typeparam name="VRCCWW"> Orientation:Vertical, Flow:Anti-Clockwise, Start:West </typeparam>
/// <typeparam name="VRCWNW"> Orientation:Vertical, Flow:Clockwise, Start:North-West </typeparam>
/// <typeparam name="VRCCNW"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-West </typeparam>
/// <typeparam name="VRCWNE"> Orientation:Vertical, Flow:Clockwise, Start:North-East </typeparam>
/// <typeparam name="VRCCNE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-East </typeparam>
/// <typeparam name="HRCWNN"> Orientation:Horizontal, Flow:Clockwise, Start:North </typeparam>
/// <typeparam name="HRCCNN"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North </typeparam>
/// <typeparam name="HRCWNE"> Orientation:Horizontal, Flow:Clockwise, Start:North-East </typeparam>
/// <typeparam name="HRCCNE"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-East </typeparam>
/// <typeparam name="HRCWSE"> Orientation:Horizontal, Flow:Clockwise, Start:South-East </typeparam>
/// <typeparam name="HRCCSE"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-East </typeparam>
/// <typeparam name="HRCWSS"> Orientation:Horizontal, Flow:Clockwise, Start:South </typeparam>
/// <typeparam name="HRCCSS"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South </typeparam>
/// <typeparam name="HRCWSW"> Orientation:Horizontal, Flow:Clockwise, Start:South-West </typeparam>
/// <typeparam name="HRCCSW"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-West </typeparam>
/// <typeparam name="HRCWNW"> Orientation:Horizontal, Flow:Clockwise, Start:North-West </typeparam>
/// <typeparam name="HRCCNW"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-West </typeparam>
type Sqn =  
    | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
///

/// <summary> Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel.
/// Each array begins with the location of Host hexel followed by the rest in a particular order.
/// Hexadecimal number system - 0x0:0, 0x1:1, 0x2:2, 0xFFFFFFFF:-1, 0xFFFFFFFE:-2 </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <returns> An array of two dimensional surrounding locations. </returns>
let sequence 
    (sqn:Sqn) =  
    match sqn with 
    | VRCWEE -> [|0x0,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2|]
    | VRCCEE -> [|0x0,0x0; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE|]
    | VRCWSE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0|]
    | VRCCSE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE|]
    | VRCWSW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE|]
    | VRCCSW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0|]
    | VRCWWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
    | VRCCWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2|]
    | VRCWNW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0|]
    | VRCCNW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2|]
    | VRCWNE -> [|0x0,0x0; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2|]
    | VRCCNE -> [|0x0,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0|]
    | HRCWNN -> [|0x0,0x0; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1|]
    | HRCCNN -> [|0x0,0x0; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1|]
    | HRCWNE -> [|0x0,0x0; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2|]
    | HRCCNE -> [|0x0,0x0; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF|]
    | HRCWSE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1|]
    | HRCCSE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
    | HRCWSS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF|]
    | HRCCSS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF|]
    | HRCWSW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
    | HRCCSW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1|]
    | HRCWNW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF|]
    | HRCCNW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2|]
///

/// <summary> Identity Hexel. </summary>
/// <returns> Available (AV) Hexel at global origin. </returns>
let identity = 
    EX(0x0,0x0, 0x0)
///

/// <summary> Extract coordinates from hexel. </summary>
/// <param name="hexel"> Hexel of type AV/RV. </param>
/// <returns> Tuple of integers representing three dimensional coordinates. </returns>
let hxlCrd 
    (hxl : Hxl) = 
    match hxl with 
    | AV (a,b,c) -> (a,b,c)
    | RV (a,b,c) -> (a,b,c)
    | EX (a,b,c) -> (a,b,c)
///

/// <summary> Valid Hexels. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxl"> Hexel whose coordinates need to be validated. </param> 
/// <returns> Valid hexel coordinates. </returns>
let hxlVld 
    (sqn : Sqn)
    (hxl : Hxl) = 
        let validate 
            sqn 
            crx 
            cry 
            crz = 
                match sqn with
                | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> match crx,cry with 
                        | a,b when (b%4 = 0) -> (a + (a%2)), b, crz
                        | a,b when (a%2 = 0)-> a+1, (b + (b%2)), crz
                        | _,b-> crx, (b + (b%2)), crz
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> match crx,cry with 
                        | a,b when (a%4 = 0) -> a, (b + (b%2)), crz
                        | a,b when (b%2 = 0) -> (a + (a%2)), b+1, crz
                        | a , _->  (a + (a%2)), cry, crz
        // Get hexel coordinayes
        let crx,cry,crz = hxlCrd hxl
        // Validate coordinates
        let x1,y1,z1 = validate sqn crx cry crz
        // Revalidate changed coordinates
        let vld = validate sqn x1 y1 z1
        // Hexels with validated coordinates
        match hxl with
        | AV(_) -> AV(vld)
        | RV(_) -> RV(vld)
        | EX(_) -> EX(vld)
///

/// <summary> Change all hexel types to a uniform type.</summary>
/// <param name="opt"> 1:AV, 2:RV, 3:EX. </param>
/// <param name="hxl"> An array of hexels. </param>
/// <returns> Converts all opted type </returns>
let hxlUni
    (opt : int)
    (hxl : Hxl[]) = 
    hxl
    |> Array.Parallel.map(fun x -> hxlCrd x)
    |> Array.Parallel.map(fun x -> match opt with 
                                                    | 1 -> AV x
                                                    | 2 -> RV x
                                                    | 3 -> EX x
                                                    | _ -> AV x)
///

/// <summary> Get Hexel from Tuple. </summary>
/// <param name="hxo"> Tuple containing Base hexel of collection and size. </param>
let getHxls 
    (hxo : (Hxl*int)[]) = 
    hxo
    |> Array.map(fun x 
                    -> fst x)
///

/// <summary> Adjacent Hexels. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxo"> Base hexel. </param> 
/// <returns> An array of six adjacent hexels. </returns>
let adjacent 
    (sqn : Sqn)
    (hxo : Hxl) =
    match hxo with 
    | AV (x,y,z) -> Array.map 
                        (fun (a,b) -> 
                        AV(x+a, y+b,z))(sequence sqn)
    | RV (x,y,z) -> [|RV(x,y,z)|]
    | EX (x,y,z) -> [|EX(x,y,z)|]
///

/// <summary> Increment Hexel. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxo"> Tuple containing Base hexel of collection and size. </param> 
/// <param name="occ"> Occupied/Unavailable hexels. </param>
/// <returns> Tuple containing the next hexel and size. </returns>
let increment 
    (sqn : Sqn)
    (hxo : Hxl * int) 
    (occ : Hxl[]) = 
    let occ = Array.concat 
                [|
                    occ
                    [|(fst hxo)|]
                    [|identity|]
                |] |> hxlUni 1
    match hxo with 
    | x,y when y >= 0x0 -> 
        let inc1 = x 
                |> adjacent sqn
                |> Array.except occ
        let inc2 = match (Array.tryItem  1 inc1) with 
                        | Some a -> 
                                        let bl1 = Array.contains (Array.head inc1) (adjacent sqn a)
                                        match bl1 with 
                                        | true -> Array.tryHead inc1
                                        | false -> x
                                                |>  adjacent sqn 
                                                |> Array.except occ
                                                |> Array.tryLast

                        | None -> Array.tryHead inc1
        match inc2 with 
        | Some a -> a, y
        | None -> (hxlVld sqn identity,0xFFFFFFFF)
    | _ -> (hxlVld sqn identity,0xFFFFFFFF)
///

/// <summary> Available Adjacent Hexels. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxo"> Hexel or Tuple containing Base hexel of collection and size. </param> 
/// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
/// <returns> The count of unoccupied surrounding hexels. </returns>
let available 
    (sqn : Sqn)
    (hxo : obj)
    (occ : Hxl[]) =  
    let occ = occ |> hxlUni 1
    let hx1 = match hxo with 
                | :? (Hxl*int) as (a,_) -> a
                | :? Hxl as b ->  b
                | _ -> identity
    hx1 
    |> adjacent sqn
    |> Array.except 
        (Array.append occ [|hx1|])
    |> Array.length
///

///<summary> Assign Hexel type. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
/// <param name="hxl"> All constituent hexels. </param>
/// <returns> Reassigned Hexel Types </returns>
let hxlChk
    (sqn : Sqn)
    (occ : Hxl[])
    (hxl : Hxl[]) = 
    hxl |> Array.map (fun x -> 
                                match (x = EX(hxlCrd x)) with 
                                | true -> x
                                | false -> match (available sqn x (Array.append occ hxl)) < 1 with 
                                            | true -> RV(hxlCrd x)
                                            | false -> AV(hxlCrd x))
///

/// <summary> Increment Hexels. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxo"> Array of Tuples containing Base hexel of collection and size. </param> 
/// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
/// <returns> Array of Tuples containing Base hexel of collection and reduced size. </returns>
let increments 
    (sqn : Sqn)
    (hxo : (Hxl*int)[]) 
    (occ : Hxl[]) = 
    let occ = (Array.append occ (getHxls hxo)) |> hxlUni 1
    let inc = 
        Array.scan (fun ac st -> 
        let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|]) |> hxlUni 1
        increment sqn st (Array.append[|fst ac|] occ )) 
            hxo[0] hxo
            |> Array.tail
    ///
    
    /// <summary> Generate alternate hexel in cases where there are overlapping hexels </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Array of Tuples containing Base hexel of collection and size. </param> 
    /// <param name="hxo"> Array of Tuples containing Incremental hexel of collection and reduced size. </param>
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <returns> Array of Tuples containing alternate incremental hexel of collection and reduced size. </returns>
    let replaceDuplicate 
        (sqn : Sqn)
        (hxo : (Hxl*int)[]) 
        (inc : (Hxl*int)[]) 
        (occ : Hxl[]) =   
        let in1 = Array.map (fun x -> snd x)inc
        let lc1 = getHxls hxo 
        let ic1 = getHxls inc 
        let oc1 = Array.concat[|occ;lc1;ic1|] |> hxlUni 1
        let id1 = Array.map(fun y -> Array.findIndex (fun x -> x = y)ic1)ic1
        let bl1 = Array.map2 (fun x y -> x=y) [|(0x0)..(Array.length ic1)-(0x1)|] id1   
        let tp1 = Array.zip3 bl1 ic1 hxo  
        tp1 |> Array.map2 (fun d (a,b,c) 
                            -> match a with 
                                | true -> b,d
                                | false -> 
                                        match ((available sqn c oc1) > 0x0) with 
                                        | false -> (fst c),0xFFFFFFFF
                                        | true -> fst(increment sqn c oc1),d) in1
        
    replaceDuplicate sqn hxo inc occ
///

/// <summary> Boundary Hexels Ring. </summary>
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

    let hxl = hxo
            |> Array.sortByDescending 
                (fun x -> available sqn x hxo)
    let a1 = 
        match hxl with 
        | [||] -> [||]
        | _ -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) true

    let b1 = (Array.length a1) = Array.length hxl
        
    let ar1 = match b1 with 
                | true -> a1
                | false -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) false
    let ar2 = 
        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> ar1
                | false -> hxlUni 2 ar1
        
    // Arrange clockwise
    let ar3 = Array.windowed 2 ar2
    let bln = Array.map(fun x 
                            ->  let cdx1,cdy1,_ = hxlCrd (Array.head x)
                                let cdx2,cdy2,_ = hxlCrd (Array.last x)
                                (cdx2 - cdx1 >= 0) && (cdy1 - cdy2 >= 0)) ar3
    match Array.contains false bln with
    | true -> Array.rev ar2
    | false -> ar2
///

/// <summary> Hexel Ring Segment Sequence. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxl"> All constituent hexels. </param>
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
///