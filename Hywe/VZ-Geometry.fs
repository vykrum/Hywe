module Geometry

open Hexel
open System
///

/// <summary> Module shape in tessalated hexagonal grid. </summary>
/// <typeparam name="Hxg"> Hexagon. </typeparam>
/// <typeparam name="Sqr"> Square. </typeparam>
/// <typeparam name="Arw"> Arrow. </typeparam>
/// <typeparam name="Prl"> Parallelogram. </typeparam>
type Shp = 
    | Hxg | Sqr | Arw | Prl

// Hexel Vertices
let vertex
    (sqn : Sqn)
    (shp : Shp)
    (hxl : Hxl) = 
    let hxCr = 
        match shp with 
        | Hxg 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
                    -> [|0x0,0x0; 0x1,0x1; 0x2,0x0; 0x2,0xFFFFFFFF; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFF|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x1,0x0; 0x2,0xFFFFFFFF; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFF|]
        | Sqr 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> [|0x0,0x0; 0x1,0x0; 0x2,0x0; 0x2,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x2,0x0; 0x2,0xFFFFFFFF; 0x2,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0x0,0xFFFFFFFF|]
        | Arw 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> [|0x0,0x0; 0x1,0xFFFFFFFF; 0x2,0x0; 0x2,0xFFFFFFFD; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFD|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x3,0x0; 0x2,0xFFFFFFFF; 0x3,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0x1,0xFFFFFFFF|]
        | Prl 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> [|0x0,0x0; 0x1,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x2,0x1; 0x2,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x0,0xFFFFFFFF|] 
                   
    let x, y, _ = hxl |> hxlCrd 
    hxCr 
    |> Array.map(fun (a,b)-> a + x, b + y) 
    |> Array.map2 ( fun inx (vrx,vry) 
                        -> string(inx),vrx,vry) 
                        [|0..(Array.length hxCr)-1|]
///

/// <summary> Ortogonal Hexel Sequence </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="org"> Start Hexel. </param> 
/// <param name="lgt"> Sequence Length. </param> 
/// <param name="vrt"> Vertical or Horizontal Orientation. </param> 
/// <param name="rev"> Positive or Negative Direction. </param> 
/// <returns> Array of Sequential Reserved Hexels. </returns>
let hxlOrt
        (sqn : Sqn)
        (org : Hxl)
        (lgt : int)
        (vrt : bool)
        (rev : bool) =
        let hxx,hxy,hxz = org |> hxlVld sqn |> hxlCrd
        let lgt = lgt + (lgt%2)
        let sgn = match rev with 
                    | true -> -1
                    | false -> 1         
        match sqn with
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
            -> match vrt with 
                | true -> [|hxy..4*sgn..(hxy+lgt+4)*sgn|]
                        |> Array.map (fun y -> [|EX(hxx,y,hxz);EX(hxx+1,y+2*sgn,hxz)|])
                        |> Array.concat
                        |> Array.take ((lgt/2)+1)
                | false -> [|hxx..2*sgn..(hxx+lgt+4)*sgn|]
                        |> Array.map (fun x -> EX (x,hxy,hxz)) 
                        |> Array.take ((lgt/2)+1)
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
            -> match vrt with
                | true -> [|hxy..2*sgn..(hxy+lgt)*sgn|]
                        |> Array.map (fun y -> RV (hxx,y,hxz)) 
                        |> Array.take ((lgt/2)+1)
                | false -> [|hxx..4*sgn..(hxx+lgt)*sgn|] 
                        |> Array.map (fun x -> [|EX(x,hxy,hxz);EX(x+2*sgn,hxy+1,hxz)|])
                        |> Array.concat
                        |> Array.take ((lgt/2)+1)

let hxlOff
    (hxl : Hxl[])
    (rev : bool) = 
    let neg = match rev with 
                | true -> -1
                | false -> 1
    let crd = Array.map (fun a -> hxlCrd a) hxl
    let x1,y1,_ = Array.head crd
    let x2,y2,_ = Array.get crd 1
    match (x1 = x2) with 
    | true -> Array.map(fun (x,y,z) -> EX(x+(2*neg),y-1,z)) crd
    | false -> match (y1=y2) with 
                | true -> Array.map(fun (x,y,z) -> EX(x+1,y+(2*neg),z)) crd
                | false -> match (Math.Abs(x2-x1)=1) with 
                            | true -> Array.map(fun (x,y,z) -> EX(x+(2*neg),y,z)) crd
                            | false -> Array.map(fun (x,y,z) -> EX(x,y+(2*neg),z)) crd

let hxlRct
    (sqn : Sqn)
    (wdt : int)
    (hgt : int)
    (str : int) = 
    let sqn = match sqn with
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW-> HRCWNN
                | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> VRCWEE
    let org = hxlVld sqn (AV(0,0,0))
    let hrz1 = hxlOrt sqn org ((wdt+1)*2) false false
    let vrt1 = hxlOrt sqn (Array.last hrz1) ((hgt+1)*2) true false
    let hrz2 = hxlOrt sqn (Array.last vrt1) ((wdt+1)*2) false true
    let vrt2 = hxlOrt sqn (Array.last hrz2) ((hgt+1)*2) true true

    let bs1 = match str with 
                | 1 -> Array.get hrz1 ((Array.length hrz1)/2)
                | 2 -> vrt1 |> Array.tail |> Array.head
                | 3 -> Array.get vrt1 ((Array.length vrt1)/2)
                | 4 -> hrz2 |> Array.tail |> Array.head
                | 5 -> Array.get hrz2 ((Array.length hrz2)/2)
                | 6 -> vrt2 |> Array.tail |> Array.head
                | 7 -> Array.get vrt2 ((Array.length vrt2)/2)
                | _ -> hrz1 |> Array.tail |> Array.head

    let rct = Array.concat[|hrz1; vrt1; hrz2; vrt2; 
                    hxlOff hrz1 true; 
                    hxlOff vrt1 false; 
                    hxlOff hrz2 false; 
                    hxlOff vrt2 true|] 
                    |> Array.distinct
    let bas = AV(hxlCrd bs1) |> adjacent sqn |> hxlUni 3 |> Array.except rct |> Array.head
    AV(hxlCrd bas),rct

///

/// <summary> Hexel Line </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="stt"> Start Hexel. </param> 
/// <param name="enn"> End Hexel. </param> 
/// <returns> Array of Sequential Reserved Hexels. </returns>

let hxlLin 
    (sqn : Sqn)
    (elv : int)
    (stt : Hxl) 
    (enn : Hxl) =
    let safeHxlCrd (hOpt: Hxl option) =
        match hOpt with
        | Some h -> hxlCrd h   // valid input
        | None -> (0, 0, elv)    // fallback for empty/invalid input
    let sx, sy, sz = safeHxlCrd (Some stt)
    let ex, ey, _ = safeHxlCrd (Some enn)

    let splitOddChunks (n: int) (arr: 'T[]) : 'T[][] =
        let len = arr.Length

        let rec loop i start acc =
            match i with
            | i when i >= n -> acc |> List.rev |> Array.ofList
            | _ ->
                let remaining = len - start
                let remainingChunks = n - i

                // ideal fair size
                let fairSize = remaining / remainingChunks

                // adjust to odd (unless it's the last chunk)
                let size =
                    match i = n - 1 with
                    | true -> remaining
                    | false ->
                        match fairSize % 2 with
                        | 0 when remaining > remainingChunks -> fairSize + 1
                        | _ -> fairSize

                let chunk = arr.[start .. start + size - 1]
                loop (i + 1) (start + size) (chunk :: acc)

        loop 0 0 []

    let dropAlternate (arr) =
        arr
        |> Array.mapi (fun i x -> i, x)
        |> Array.choose (fun (i, x) -> if i % 2 = 0 then Some x else None)

    let bumpEveryOther (hr:bool) (arr: (int * int)[]) =
        arr
        |> Array.mapi (fun i (x, y) ->
            match i % 2 with
            | 0 -> (x, y)
            | _ -> match hr with
                    | true -> (x, y + 1)
                    | false -> (x + 1 , y)  
        )

    let seqH = 
        match sqn with
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> false
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW -> true

    let pty,flt =  
        let pth = match sx <= ex with
                    | true -> [|sx .. 2 .. ex|] 
                    | false -> [|sx .. -2 .. ex|]
        let flh = match sy <= ey with
                    | true -> [|sy .. 1 .. ey|]
                    | false -> [|sy .. -1 .. ey|]
        let ptv = match sy <= ey with
                    | true -> [|sy .. 2 .. ey|]
                    | false -> [|sy .. -2 .. ey|]
        let flv = match sx <= ex with
                    | true -> [|sx .. 1 .. ex|]
                    | false -> [|sx .. -1 .. ex|]  
                            
        match seqH with
        |false -> ptv,flv
        |true -> pth,flh
            
    let div = 
        match Array.length pty >= Array.length flt with  
        | true -> 
            let cnk1 = splitOddChunks (Array.length flt) pty
            let a =
                match seqH with
                | true -> // horizontal: (b, a)
                    Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (b, a))) flt
                | false -> // vertical: (a, b)
                    Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (a, b))) flt
            a |> Array.map (bumpEveryOther seqH) |> Array.concat
        | false -> 
            let cnk1 = splitOddChunks (Array.length pty) flt
            let a =
                match seqH with
                | true -> // horizontal: (a, b)
                    Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (a, b))) pty
                | false -> // vertical: (b, a)
                    Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (b, a))) pty
            a |> Array.map dropAlternate  |> Array.concat

    let result =
        div |> Array.map (fun (a,b) -> RV(a,b,sz))

    match result.Length = 0 with
    | true -> [| stt; enn |] |> Array.distinct 
    | false -> result

///

let removeSawtooth (sqn : Sqn) (arr : (int*int)[]) : (int*int)[] =
    // Determine primary axis
    let vert =
        match sqn with
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW
        | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> true
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS
        | HRCWSW | HRCCSW | HRCWNW | HRCCNW -> false

    let primary = if vert then snd else fst
    let secondary = if vert then fst else snd

    // Split array by Δ=2 along primary axis
    let splitByDelta2 (arr: (int*int)[]) : (int*int)[][] =
        match arr with
        | [||] -> [||]
        | _ ->
            let folder (groups, currGroup) p =
                match currGroup with
                | [||] -> (groups, [|p|])
                | _ ->
                    let last = currGroup.[currGroup.Length - 1]
                    match abs (primary p - primary last) with
                    | 2 -> (groups, Array.append currGroup [|p|])
                    | _ -> (Array.append groups [|currGroup|], [|p|])
            let groups, lastGroup = Array.fold folder ([||], [||]) arr
            Array.append groups [|lastGroup|]

    // Check secondary-axis oscillation using recursion + pattern matching
    let rec oscillates (values: int[]) idx =
        match idx >= values.Length - 2 with
        | true -> true
        | false ->
            match abs (values.[idx+1] - values.[idx]), abs (values.[idx+2] - values.[idx+1]) with
            | 1, 1 -> oscillates values (idx + 1)
            | _ -> false

    // Post-process subarrays for oscillation
    let processOscillation (subarrays: (int*int)[][]) : (int*int)[][] =
        subarrays
        |> Array.map (fun arr ->
            match arr.Length with
            | n when n <= 2 -> arr
            | _ ->
                let values = arr |> Array.map secondary
                match oscillates values 0 with
                | true ->
                    let first = arr.[0]
                    let last  = arr.[arr.Length - 1]
                    let low = min (secondary first) (secondary last)
                    match vert with
                    | true  -> [| (low, snd first); (low, snd last) |]
                    | false -> [| (fst first, low); (fst last, low) |]
                | false -> arr
        )

    // --- Pipeline ---
    arr
    |> splitByDelta2
    |> processOscillation
    |> Array.collect id  // Flatten

///

/// <summary> Hexel Polygon </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="vtx"> Integer coordinates of polygon vertices. </param> 
/// <param name="elv"> Elevation/Level/Z. </param> 
/// <returns> Array of Sequential Reserved Hexels. </returns>

let hxlPgn 
    (sqn: Sqn) 
    (elv: int)
    (vtx: (int * int)[]): Hxl[] =

    if vtx.Length < 2 then [||] else
    let stx, sty = Array.head vtx
    let xx, yy, _ =
        hxlLin sqn elv (RV(0,0,elv)) (RV(stx,sty,elv))
        |> Array.last
        |> hxlCrd

    // Replace start vertex with endpoint of (0,0) first vertex
    let vt1 = Array.concat [| [|xx,yy|]; Array.tail vtx |]

    // Ensure closure
    let verts =
        if vt1.[0] = vt1.[vt1.Length - 1] then vt1
        else Array.append vt1 [|vt1.[0]|]

    // Build polygon edges, chaining each segment
    let acc = ResizeArray<Hxl>()
    let mutable lastOpt : Hxl option = None

    for (x2, y2) in verts do
        match lastOpt with
        | None ->
            lastOpt <- Some (RV(x2, y2, elv))
        | Some last ->
            let seg = hxlLin sqn elv last (RV(x2, y2, elv))
            acc.AddRange(seg)
            lastOpt <- Some (Array.last seg)

    acc.ToArray()

// Polygon Area
/// Signed area of a polygon using pattern matching (shoelace formula)
let polygonArea (poly: (int * int)[]) =
    match poly with
    | [||] | [| _ |] | [| _; _ |] ->
        // Too few points to form a polygon
        0.0
    | pts ->
        let rec loop acc i =
            match i with
            | n when n = pts.Length ->
                // Close the loop (last to first)
                let (x1, y1) = pts.[n - 1]
                let (x2, y2) = pts.[0]
                acc + (x1 * y2 - x2 * y1)
            | _ ->
                let (x1, y1) = pts.[i]
                let (x2, y2) = pts.[(i + 1) % pts.Length]
                loop (acc + (x1 * y2 - x2 * y1)) (i + 1)
        float (abs (loop 0 0)) / 2.0


// Polygon Area with Holes
/// Net area of a polygon with holes
let polygonWithHolesArea (outer: (int * int)[]) (holes: (int * int)[][]) =
    match outer, holes with
    | [||], _ -> 0.0
    | _, [||] -> polygonArea outer
    | outerPts, holePolys ->
        let outerArea = polygonArea outerPts
        let holesArea =
            holePolys
            |> Array.sumBy (fun hole ->
                match hole with
                | [||] | [| _ |] | [| _; _ |] -> 0.0
                | _ -> polygonArea hole)
        outerArea - holesArea

