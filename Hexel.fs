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
/// <typeparam name="SQ11"> Orientation:Vertical, Flow:Clockwise, Start:East </typeparam>
/// <typeparam name="SQ12"> Orientation:Vertical, Flow:Anti-Clockwise, Start:East </typeparam>
/// <typeparam name="SQ13"> Orientation:Vertical, Flow:Clockwise, Start:South-East </typeparam>
/// <typeparam name="SQ14"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-East </typeparam>
/// <typeparam name="SQ15"> Orientation:Vertical, Flow:Clockwise, Start:South-West </typeparam>
/// <typeparam name="SQ16"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-West </typeparam>
/// <typeparam name="SQ17"> Orientation:Vertical, Flow:Clockwise, Start:West </typeparam>
/// <typeparam name="SQ18"> Orientation:Vertical, Flow:Anti-Clockwise, Start:West </typeparam>
/// <typeparam name="SQ19"> Orientation:Vertical, Flow:Clockwise, Start:North-West </typeparam>
/// <typeparam name="SQ20"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-West </typeparam>
/// <typeparam name="SQ21"> Orientation:Vertical, Flow:Clockwise, Start:North-East </typeparam>
/// <typeparam name="SQ22"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-East </typeparam>
/// <typeparam name="SQ23"> Orientation:Horizontal, Flow:Clockwise, Start:North </typeparam>
/// <typeparam name="SQ24"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North </typeparam>
/// <typeparam name="SQ25"> Orientation:Horizontal, Flow:Clockwise, Start:North-East </typeparam>
/// <typeparam name="SQ26"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-East </typeparam>
/// <typeparam name="SQ27"> Orientation:Horizontal, Flow:Clockwise, Start:South-East </typeparam>
/// <typeparam name="SQ28"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-East </typeparam>
/// <typeparam name="SQ29"> Orientation:Horizontal, Flow:Clockwise, Start:South </typeparam>
/// <typeparam name="SQ30"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South </typeparam>
/// <typeparam name="SQ31"> Orientation:Horizontal, Flow:Clockwise, Start:South-West </typeparam>
/// <typeparam name="SQ32"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-West </typeparam>
/// <typeparam name="SQ33"> Orientation:Horizontal, Flow:Clockwise, Start:North-West </typeparam>
/// <typeparam name="SQ34"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-West </typeparam>
type Sqn =  
    | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22
    | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34
///

/// <summary> Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel.
/// Each array begins with the location of Host hexel followed by the rest in a particular order.
/// Hexadecimal number system - 0x0:0, 0x1:1, 0x2:2, 0xFFFFFFFF:-1, 0xFFFFFFFE:-2 </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <returns> An array of two dimensional surrounding locations. </returns>
let sequence 
    (sqn:Sqn) =  
    match sqn with 
    | SQ11 -> [|0x0,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2|]
    | SQ12 -> [|0x0,0x0; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE|]
    | SQ13 -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0|]
    | SQ14 -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE|]
    | SQ15 -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE|]
    | SQ16 -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0|]
    | SQ17 -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
    | SQ18 -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2|]
    | SQ19 -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0|]
    | SQ20 -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2|]
    | SQ21 -> [|0x0,0x0; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2|]
    | SQ22 -> [|0x0,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0|]
    | SQ23 -> [|0x0,0x0; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1|]
    | SQ24 -> [|0x0,0x0; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1|]
    | SQ25 -> [|0x0,0x0; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2|]
    | SQ26 -> [|0x0,0x0; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF|]
    | SQ27 -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1|]
    | SQ28 -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
    | SQ29 -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF|]
    | SQ30 -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF|]
    | SQ31 -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
    | SQ32 -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1|]
    | SQ33 -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF|]
    | SQ34 -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2|]
///

/// <summary> Identity Hexel. </summary>
/// <returns> Available (AV) Hexel at global origin. </returns>
let identity = 
    AV(0x0,0x0, 0x0)
///

/// <summary> Extract coordinates from hexel. </summary>
/// <param name="hexel"> Hexel of type AV/RV. </param>
/// <returns> Tuple of integers representing three dimensional coordinates. </returns>
let hxlCrd 
    (hxl : Hxl) = 
    match hxl with 
    | AV (a,b,c) -> (a,b,c)
    | RV (a,b,c) -> (a,b,c)
///

/// <summary> Standardize hexel type. </summary>
/// <param name="hxl"> An array of hexels. </param>
/// <returns> Converts all hexels to type AV </returns>
let allOG 
    (hxl:Hxl[]) = 
    hxl
    |> Array.map(fun x -> hxlCrd x)
    |> Array.map(fun x -> AV x)

/// <summary> Get Hexel from Tuple. </summary>
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
    (sqn: Sqn)
    (hxo: Hxl) =
    match hxo with 
    | AV (x,y,z) -> Array.map 
                        (fun (a,b) -> 
                        AV(x+a, y+b,z))(sequence sqn)
    | RV (x,y,z) -> [|RV(x,y,z)|]
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
                |] |> allOG
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
        | None -> (identity,0xFFFFFFFF)
    | _ -> (identity,0xFFFFFFFF)
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
    let occ = occ |> allOG
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

/// <summary> Increment Hexels. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="hxo"> Array of Tuples containing Base hexel of collection and size. </param> 
/// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
/// <returns> Array of Tuples containing Base hexel of collection and reduced size. </returns>
let increments 
    (sqn : Sqn)
    (hxo : (Hxl*int)[]) 
    (occ : Hxl[]) = 
    let occ = (Array.append occ (getHxls hxo)) |> allOG
    let inc = 
        Array.scan (fun ac st -> 
        let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|]) |> allOG
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
        let oc1 = Array.concat[|occ;lc1;ic1|] |> allOG
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