module Geometry

open System
open Hexel
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
    (arr : (float*float)[]) : (float*float)[] =
    if arr.Length = 0 then [||] else
    let (primary, secondary) = 
        match sqn with
        | Vertical   -> (snd, fst)
        | Horizontal -> (fst, snd)

    let result = ResizeArray<float*float>()
    
    let mutable i = 0
    let n = arr.Length
    while i < n do
        let mutable j = i
        while j + 1 < n && abs(primary arr.[j+1] - primary arr.[j]) < 2.1 && abs(primary arr.[j+1] - primary arr.[j]) > 1.9 do
            j <- j + 1
            
        let groupLen = j - i + 1
        if groupLen > 3 then
            let mutable isOscillating = true
            for k = i to j - 1 do
                if abs(secondary arr.[k+1] - secondary arr.[k]) < 0.9 || abs(secondary arr.[k+1] - secondary arr.[k]) > 1.1 then
                    isOscillating <- false
            
            if isOscillating then
                let f = arr.[i]
                let l = arr.[j]
                let low = min (secondary f) (secondary l)
                match sqn with
                | Vertical   -> 
                    result.Add((low, snd f))
                    result.Add((low, snd l))
                | Horizontal -> 
                    result.Add((fst f, low))
                    result.Add((fst l, low))
            else
                for k = i to j do
                    result.Add(arr.[k])
        else
            for k = i to j do
                result.Add(arr.[k])
                
        i <- j + 1

    result.ToArray()

/// <summary> Calculates the signed area of a polygon using the Shoelace formula. </summary>
/// <param name="poly"> Array of (x, y) coordinates defining the polygon. </param>
/// <returns> The calculated area as a float. </returns>
let polygonArea 
    (poly: (float * float)[]) =
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
        area / 2.0

///

/// <summary> Calculates the net area of a polygon by subtracting the area of holes. </summary>
/// <param name="outer"> Coordinates of the outer boundary. </param>
/// <param name="holes"> Array of coordinate arrays defining interior holes. </param>
/// <returns> The net area as a float. </returns>
let polygonWithHolesArea 
    (outer: (float * float)[]) 
    (holes: (float * float)[][]) =

    match outer, holes with
    | [||], _ -> 0.0
    | _, [||] -> abs (polygonArea outer)
    | outerPts, holePolys ->
        let outerArea = abs (polygonArea outerPts)
        let holesArea =
            holePolys
            |> Array.sumBy (fun hole ->
                match hole with
                | [||] | [| _ |] | [| _; _ |] -> 0.0
                | _ -> abs (polygonArea hole))
        outerArea - holesArea

/// <summary> Ensures that a polygon vertex array is closed by appending the first vertex to the end if necessary. </summary>
let ensureClosed (pts: (float*float)[]) =
    match pts with
    | [||] -> pts
    | _ ->
        let first = pts.[0]
        let last = pts.[pts.Length-1]
        if first = last then pts
        else Array.append pts [| first |]

/// <summary> Removes consecutive duplicate points from a polygon vertex array. </summary>
let dedupeSequential (pts: (float*float)[]) =
    if pts.Length = 0 then pts
    else
        let res = ResizeArray<float*float>(pts.Length)
        res.Add(pts.[0])
        for i = 1 to pts.Length - 1 do
            if pts.[i] <> res.[res.Count - 1] then
                res.Add(pts.[i])
        res.ToArray()

/// <summary> Normalizes the winding order of a polygon to be either clockwise or counterclockwise. </summary>
let normalizeWinding (ccw: bool) (pts: (float*float)[]) =
    let pts = ensureClosed pts |> dedupeSequential
    if pts.Length < 4 then pts
    else
        let a = polygonArea pts
        match ccw, a > 0.0 with
        | true, false -> Array.rev pts
        | false, true -> Array.rev pts
        | _ -> pts

/// <summary> Computes the bounding box of a set of points. </summary>
let bounds (pts: (float*float)[]) =
    let xs = pts |> Array.map fst
    let ys = pts |> Array.map snd
    Array.min xs, Array.min ys, Array.max xs, Array.max ys

/// <summary> Computes the centroid (geometric center) of a set of points. </summary>
let centroid (pts: (float*float)[]) =
    let n = float pts.Length
    let sx = pts |> Array.sumBy fst
    let sy = pts |> Array.sumBy snd
    sx / n, sy / n

/// <summary> Determines if a point is inside a polygon using the ray-casting algorithm. </summary>
let pointInPolygon (px: float, py: float) (poly:(float*float)[]) =
    let rec loop i j inside =
        if i = poly.Length then inside else
        let xi, yi = poly.[i]
        let xj, yj = poly.[j]
        let intersect =
            ((yi > py) <> (yj > py)) &&
            (px < (xj-xi) * (py-yi) / (yj-yi+0.0001) + xi)
        loop (i+1) i (if intersect then not inside else inside)
    loop 0 (poly.Length-1) false

/// <summary> Determines if two line segments intersect. </summary>
let ccw (ax,ay) (bx,by) (cx,cy) =
    (cy-ay)*(bx-ax) > (by-ay)*(cx-ax)

/// <summary> Checks if the line segments AB and CD intersect. </summary>
let segmentsIntersect a b c d =
    ccw a c d <> ccw b c d && ccw a b c <> ccw a b d

/// <summary> Determines if a polygon has self-intersecting edges. </summary>
let hasSelfIntersections (pts:(float*float)[]) =
    let p = ensureClosed pts
    let n = p.Length-1
    let mutable found = false
    let mutable i = 0
    while i <= n - 2 && not found do
        let mutable j = i + 2
        while j <= n - 2 && not found do
            if i <> 0 || j <> n - 2 then
                if segmentsIntersect p.[i] p.[i+1] p.[j] p.[j+1] then
                    found <- true
            j <- j + 1
        i <- i + 1
    found

/// <summary> Determines if three points are collinear. </summary>
let isCollinear (ax: float, ay: float) (bx: float, by: float) (cx: float, cy: float) =
    abs((bx-ax)*(cy-ay) - (by-ay)*(cx-ax)) < 0.0001

let removeCollinear (pts:(float*float)[]) =
    let p = ensureClosed pts
    if p.Length < 3 then p
    else
        let res = ResizeArray<float*float>(p.Length)
        res.Add(p.[0])
        for i = 1 to p.Length - 2 do
            let a = p.[i-1]
            let b = p.[i]
            let c = p.[i+1]
            if not (isCollinear a b c) then 
                res.Add(b)
        res.Add(p.[p.Length-1])
        res.ToArray()

/// <summary> Cleans a polygon by deduplicating vertices, ensuring closure, removing collinear points, and eliminating sawtooth artifacts. </summary>
let cleanPolygon sqn pts =
    pts
    |> dedupeSequential
    |> removeSawtooth sqn
    |> dedupeSequential
    |> ensureClosed
    |> removeCollinear
    |> normalizeWinding true
///

/// <summary> Hexel Polygon </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="vtx"> Integer coordinates of polygon vertices. </param> 
/// <param name="elv"> Elevation/Level/Z. </param> 
/// <returns> Array of Sequential Reserved Hexels. </returns>
let hxlPgn 
    (sqn: Sqn) 
    (elv: int)
    (vtx: (float * float)[]): Hxl[] =

    if vtx.Length < 2 then [||] else
    let stx, sty = Array.head vtx
    let xx, yy, _ =
        hxlLin sqn elv (RV(0,0,elv)) (RV(int (Math.Round stx), int (Math.Round sty), elv))
        |> Array.last
        |> hxlCrd

    // Replace start vertex with endpoint of (0,0) first vertex
    let vt1 = Array.concat [| [|float xx, float yy|]; Array.tail vtx |]

    // Ensure closure
    let verts = cleanPolygon sqn vt1

    // Build polygon edges, chaining each segment
    let acc = ResizeArray<Hxl>()
    let mutable lastOpt : Hxl option = None

    for (x2, y2) in verts do
        let ix, iy = int (Math.Round x2), int (Math.Round y2)
        match lastOpt with
        | None ->
            lastOpt <- Some (RV(ix, iy, elv))
        | Some last ->
            let seg = hxlLin sqn elv last (RV(ix, iy, elv))
            acc.AddRange(seg)
            lastOpt <- Some (Array.last seg)

    acc.ToArray()

/// <summary> Generates a set of reserved hexels representing the area outside a polygon and inside islands. </summary>
let hxlBdrFill (sqn: Sqn) (elv: int) (outer: (float*float)[]) (islands: (float*float)[][]) =
    if Array.isEmpty outer then [||] else
    let minX, minY, maxX, maxY = bounds outer
    let buffer = 10.0 // Reduced buffer for performance
    
    let res = System.Collections.Generic.List<Hxl>()
    
    let bMinX = int (Math.Floor(minX - buffer))
    let bMaxX = int (Math.Ceiling(maxX + buffer))
    let bMinY = int (Math.Floor(minY - buffer))
    let bMaxY = int (Math.Ceiling(maxY + buffer))
    
    // Pre-calculate island bounds for faster skipping
    let islandBounds = islands |> Array.map bounds

    for x in bMinX .. bMaxX do
        let px = float x
        for y in bMinY .. bMaxY do
            let py = float y
            
            // Fast bounding box check for the outer boundary
            if px < minX || px > maxX || py < minY || py > maxY then
                res.Add(RV(x, y, elv))
            else
                let inside = pointInPolygon (px, py) outer
                if not inside then
                    res.Add(RV(x, y, elv))
                else
                    // Check islands with pre-calculated bounds
                    let mutable inIsland = false
                    let mutable j = 0
                    while j < islands.Length && not inIsland do
                        let iminX, iminY, imaxX, imaxY = islandBounds.[j]
                        if px >= iminX && px <= imaxX && py >= iminY && py <= imaxY then
                            if pointInPolygon (px, py) islands.[j] then
                                inIsland <- true
                        j <- j + 1
                    
                    if inIsland then
                        res.Add(RV(x, y, elv))
    res.ToArray()