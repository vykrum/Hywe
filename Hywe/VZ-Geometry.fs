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
///

/// <summary> The integer vertex coordinates for a given shape and orientation. </summary>
/// <param name="sqn"> The grid sequence (Vertical or Horizontal). </param>
/// <param name="shp"> The geometric shape type. </param>
/// <param name="hxl"> The origin hexel for the shape. </param>
/// <returns> An array of tuples containing the vertex index as a string and the x, y coordinates. </returns>
let vertex
    (sqn : Sqn)
    (shp : Shp)
    (hxl : Hxl) = 
    let hxCr = 
        match shp, sqn with 
        | Hxg, Vertical   -> [| 0,0; 1, 1; 2, 0; 2,-1; 1,-2; 0,-1 |]
        | Hxg, Horizontal -> [| 0,0; 1, 0; 2,-1; 1,-2; 0,-2;-1,-1 |]

        | Sqr, Vertical   -> [| 0,0; 1, 0; 2, 0; 2,-2; 1,-2; 0,-2 |]
        | Sqr, Horizontal -> [| 0,0; 2, 0; 2,-1; 2,-2; 0,-2; 0,-1 |]

        | Arw, Vertical   -> [| 0,0; 1,-1; 2, 0; 2,-3; 1,-2; 0,-3 |]
        | Arw, Horizontal -> [| 0,0; 3, 0; 2,-1; 3,-2; 0,-2; 1,-1 |]

        | Prl, Vertical   -> [| 0,0; 1, 0; 2, 0; 1,-2; 0,-2;-1,-2 |]
        | Prl, Horizontal -> [| 0,0; 2, 1; 2, 0; 2,-1; 0,-2; 0,-1 |]    
    let x, y, _ = hxlCrd hxl 
    hxCr |> Array.mapi (fun i (a, b) -> string i, a + x, b + y)
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
    let hxx, hxy, hxz = org |> hxlVld sqn |> hxlCrd
    let lgt = lgt + (lgt % 2)
    let sgn = match rev with true -> -1 | false -> 1          
    
    match sqn, vrt with
    | Vertical, true -> 
        [| hxy .. 4*sgn .. (hxy+lgt+4)*sgn |]
        |> Array.map (fun y -> [| EX(hxx, y, hxz); EX(hxx+1, y+2*sgn, hxz) |])
        |> Array.concat |> Array.take ((lgt/2)+1)
    | Vertical, false -> 
        [| hxx .. 2*sgn .. (hxx+lgt+4)*sgn |]
        |> Array.map (fun x -> EX(x, hxy, hxz)) |> Array.take ((lgt/2)+1)
    | Horizontal, true -> 
        [| hxy .. 2*sgn .. (hxy+lgt)*sgn |]
        |> Array.map (fun y -> RV(hxx, y, hxz)) |> Array.take ((lgt/2)+1)
    | Horizontal, false -> 
        [| hxx .. 4*sgn .. (hxx+lgt)*sgn |] 
        |> Array.map (fun x -> [| EX(x, hxy, hxz); EX(x+2*sgn, hxy+1, hxz) |])
        |> Array.concat |> Array.take ((lgt/2)+1)
///

/// <summary> Offsets an array of hexels by a boundary distance based on relative direction. </summary>
/// <param name="hxl"> Array of Hexels to offset. </param>
/// <param name="rev"> Directional toggle for the offset. </param>
/// <returns> A new array of offset Hexels. </returns>
let hxlOff 
    (hxl : Hxl[]) 
    (rev : bool) =  
    let neg = match rev with true -> -1 | false -> 1
    let crd = Array.map hxlCrd hxl
    let x1, y1, _ = Array.head crd
    let x2, y2, _ = Array.get crd 1
    
    match (x1 = x2), (y1 = y2), (Math.Abs(x2 - x1) = 1) with
    | true, _, _ -> 
        crd |> Array.map (fun (x,y,z) -> EX(x + (2 * neg), y - 1, z))
    | _, true, _ -> 
        crd |> Array.map (fun (x,y,z) -> EX(x + 1, y + (2 * neg), z))
    | _, _, true -> 
        crd |> Array.map (fun (x,y,z) -> EX(x + (2 * neg), y, z))
    | _          -> 
        crd |> Array.map (fun (x,y,z) -> EX(x, y + (2 * neg), z))
///

/// <summary> Generates a hollow rectangular boundary of Hexels and identifies a base interior point. </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="wdt"> Width of the rectangle. </param>
/// <param name="hgt"> Height of the rectangle. </param>
/// <param name="str"> Start index/anchor position selector (1-7). </param>
/// <returns> A tuple of (Interior Base Hexel, Boundary Hexel Array). </returns>
let hxlRct 
    (sqn : Sqn) 
    (wdt : int) 
    (hgt : int) 
    (str : int) =  
    let sqnVal = match sqn with Horizontal -> HRCWNN | Vertical -> VRCWEE
    let org = hxlVld sqnVal (AV(0,0,0))
    let hrz1 = hxlOrt sqnVal org ((wdt + 1) * 2) false false
    let vrt1 = hxlOrt sqnVal (Array.last hrz1) ((hgt + 1) * 2) true false
    let hrz2 = hxlOrt sqnVal (Array.last vrt1) ((wdt + 1) * 2) false true
    let vrt2 = hxlOrt sqnVal (Array.last hrz2) ((hgt + 1) * 2) true true

    let bs1 = 
        match str with 
        | 1 -> Array.get hrz1 (hrz1.Length / 2)
        | 2 -> vrt1.[1]
        | 3 -> Array.get vrt1 (vrt1.Length / 2)
        | 4 -> hrz2.[1]
        | 5 -> Array.get hrz2 (hrz2.Length / 2)
        | 6 -> vrt2.[1]
        | 7 -> Array.get vrt2 (vrt2.Length / 2)
        | _ -> hrz1.[1]

    let rct = Array.concat [| hrz1; vrt1; hrz2; vrt2; 
                              hxlOff hrz1 true; hxlOff vrt1 false; 
                              hxlOff hrz2 false; hxlOff vrt2 true |] |> Array.distinct
    let bas = AV(hxlCrd bs1) |> adjacent sqnVal |> hxlUni 3 |> Array.except rct |> Array.head
    AV(hxlCrd bas), rct

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
        | Some h -> hxlCrd h
        | None -> (0, 0, elv)
    let sx, sy, sz = safeHxlCrd (Some stt)
    let ex, ey, _ = safeHxlCrd (Some enn)

    let splitOddChunks (n: int) (arr: 'T[]) : 'T[][] =
        let len = arr.Length
        let rec loop i start acc =
            match i >= n with
            | true -> acc |> List.rev |> Array.ofList
            | false ->
                let remaining = len - start
                let remainingChunks = n - i
                let fairSize = remaining / remainingChunks
                let size =
                    match i = n - 1 with
                    | true -> remaining
                    | false ->
                        match fairSize % 2, remaining > remainingChunks with
                        | 0, true -> fairSize + 1
                        | _ -> fairSize
                loop (i + 1) (start + size) (arr.[start .. start + size - 1] :: acc)
        loop 0 0 []

    let dropAlternate (arr) =
        arr |> Array.mapi (fun i x -> i, x)
            |> Array.choose (fun (i, x) -> match i % 2 with 0 -> Some x | _ -> None)

    let bumpEveryOther (hr: bool) (arr: (int * int)[]) =
        arr |> Array.mapi (fun i (x, y) ->
            match i % 2, hr with
            | 0, _ -> (x, y)
            | _, true -> (x, y + 1)
            | _, false -> (x + 1, y))

    let seqH = match sqn with Horizontal -> true | Vertical -> false
    let pty, flt =  
        let step p1 p2 d = match p1 <= p2 with true -> [|p1 .. d .. p2|] | false -> [|p1 .. -d .. p2|]
        match seqH with
        | true -> step sx ex 2, step sy ey 1
        | false -> step sy ey 2, step sx ex 1
            
    let div = 
        match Array.length pty >= Array.length flt with  
        | true -> 
            let cnk1 = splitOddChunks (Array.length flt) pty
            Array.mapi (fun i a -> 
                match seqH with
                | true -> cnk1.[i] |> Array.map (fun b -> (b, a))
                | false -> cnk1.[i] |> Array.map (fun b -> (a, b))
            ) flt |> Array.map (bumpEveryOther seqH) |> Array.concat
        | false -> 
            let cnk1 = splitOddChunks (Array.length pty) flt
            Array.mapi (fun i a -> 
                match seqH with
                | true -> cnk1.[i] |> Array.map (fun b -> (a, b))
                | false -> cnk1.[i] |> Array.map (fun b -> (b, a))
            ) pty |> Array.map dropAlternate |> Array.concat

    let result = div |> Array.map (fun (a,b) -> RV(a, b, sz))
    match result.Length with 0 -> [| stt; enn |] |> Array.distinct | _ -> result
///

/// <summary> Removes aliasing/sawtooth artifacts from a sequence of hex coordinates. </summary>
/// <param name="sqn"> The grid sequence orientation. </param>
/// <param name="arr"> Array of (x, y) coordinates. </param>
/// <returns> A cleaned array of (x, y) coordinates. </returns>
let removeSawtooth 
    (sqn : Sqn) 
    (arr : (int*int)[]) : (int*int)[] =
    // --- Determine primary axis functions ---
    let (primary, secondary) = 
        match sqn with
        | Vertical   -> (snd, fst)
        | Horizontal -> (fst, snd)

    // --- Split by Δ=2 along primary axis using Fold ---
    let splitByDelta2 (points: (int*int)[]) =
        match points.Length with
        | 0 -> [||]
        | _ -> 
            let folder (acc: (int*int) list list) point =
                match acc with
                | [] -> [[point]]
                | currentGroup :: rest ->
                    match abs(primary point - primary (List.head currentGroup)) with
                    | 2 -> (point :: currentGroup) :: rest
                    | _ -> [point] :: currentGroup :: rest
            
            points 
            |> Array.fold folder [] 
            |> List.map (List.rev >> Array.ofList)
            |> List.rev
            |> Array.ofList

    // --- Detect oscillation on secondary axis ---
    let rec oscillates values =
        match values with
        | [||] | [|_|] | [|_;_|] -> false
        | _ ->
            let rec loop i =
                match i >= values.Length - 2 with
                | true -> true
                | false ->
                    match abs(values.[i+1] - values.[i]), 
                          abs(values.[i+2] - values.[i+1]) with
                    | 1, 1 -> loop (i+1)
                    | _    -> false
            loop 0

    // --- Reduce oscillatory groups ---
    let processOscillation (groups: (int*int)[][]) =
        groups
        |> Array.map (fun g ->
            match g.Length <= 3 with
            | true -> g
            | false ->
                let secValues = g |> Array.map secondary
                match oscillates secValues with
                | false -> g
                | true ->
                    let f, l = g.[0], g.[g.Length-1]
                    let low = min (secondary f) (secondary l)
                    // Reconstruction based on orientation
                    match sqn with
                    | Vertical   -> [| (low, snd f); (low, snd l) |]
                    | Horizontal -> [| (fst f, low); (fst l, low) |]
        )

    // --- Pipeline ---
    arr
    |> splitByDelta2
    |> processOscillation
    |> Array.collect id
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
///

/// <summary> Calculates the signed area of a polygon using the Shoelace formula. </summary>
/// <param name="poly"> Array of (x, y) coordinates defining the polygon. </param>
/// <returns> The calculated area as a float. </returns>
let polygonArea 
    (poly: (int * int)[]) =
    match poly with
    | [||] | [|_|] | [|_; _|] -> 0.0
    | pts ->
        let n = pts.Length
        let area = 
            pts 
            |> Array.mapi (fun i (x, y) ->
                let (nx, ny) = pts.[(i + 1) % n]
                (x * ny) - (nx * y))
            |> Array.sum
        float (abs area) / 2.0
///

/// <summary> Calculates the net area of a polygon by subtracting the area of holes. </summary>
/// <param name="outer"> Coordinates of the outer boundary. </param>
/// <param name="holes"> Array of coordinate arrays defining interior holes. </param>
/// <returns> The net area as a float. </returns>
let polygonWithHolesArea 
    (outer: (int * int)[]) 
    (holes: (int * int)[][]) =
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

