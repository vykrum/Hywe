namespace Hywe.Core

module Geometry =
    open System
    open Hexel
    open Coxel

    /// <summary> Converts hexel indices to Cartesian coordinates for visual rendering. </summary>
    let toCartesian (sqn: Sqn) (x: int, y: int) =
        match sqn with
        | Vertical -> 
            let cartX = float x + (0.5 * float (y % 2))
            let cartY = float y * 0.866
            (cartX, cartY)
        | Horizontal ->
            let cartX = float x * 0.866
            let cartY = float y + (0.5 * float (x % 2))
            (cartX, cartY)

    /// <summary> Hexel Line generation using integer grid math. </summary>
    let hxlLin (sqn : Sqn) (elv : int) (stt : Hxl) (enn : Hxl) =
        let safeHxlCrd h = hxlCrd h
        let sx, sy, sz = safeHxlCrd stt
        let ex, ey, _ = safeHxlCrd enn

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
        match result.Length with 
        | 0 -> ([| stt; enn |] |> Array.distinct) 
        | _ -> result

    /// <summary> Removes aliasing/sawtooth artifacts from a sequence of hex coordinates. </summary>
    let removeSawtooth (sqn : Sqn) (arr : (int*int)[]) : (int*int)[] =
        if arr.Length = 0 then [||] else
        let (primary, secondary) = 
            match sqn with
            | Vertical   -> (snd, fst)
            | Horizontal -> (fst, snd)

        let result = ResizeArray<int*int>()
        let mutable i = 0
        let n = arr.Length
        while i < n do
            let mutable j = i
            while j + 1 < n && abs(primary arr.[j+1] - primary arr.[j]) = 2 do
                j <- j + 1
                
            let groupLen = j - i + 1
            if groupLen > 3 then
                let mutable isOscillating = true
                for k = i to j - 1 do
                    if abs(secondary arr.[k+1] - secondary arr.[k]) <> 1 then
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

    /// <summary> Deduplicates sequential points. </summary>
    let dedupeSequential (pts: (int * int)[]) =
        pts |> Array.fold (fun acc p -> 
            match acc with
            | [] -> [p]
            | head :: _ -> match head = p with true -> acc | false -> p :: acc
        ) [] |> List.rev |> Array.ofList

    /// <summary> Ensures a polygon is closed by repeating the first point if necessary. </summary>
    let ensureClosed (pts: (int * int)[]) =
        match pts.Length < 2 with
        | true -> pts
        | false ->
            match pts.[0] = pts.[pts.Length - 1] with
            | true -> pts
            | false -> Array.append pts [| pts.[0] |]

    /// <summary> Removes collinear points using integer cross product. </summary>
    let removeCollinear (pts: (int * int)[]) =
        match pts.Length < 3 with
        | true -> pts
        | false ->
            let midPoints = 
                pts |> Array.windowed 3 |> Array.choose (fun win ->
                    let (x1, y1), (x2, y2), (x3, y3) = win.[0], win.[1], win.[2]
                    let cross = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
                    match abs cross > 0 with true -> Some (x2, y2) | false -> None)
            Array.concat [| [|pts.[0]|]; midPoints; [|pts.[pts.Length-1]|] |]

    /// <summary> Normalizes the winding order of a polygon using integer area check. </summary>
    let normalizeWinding (clockwise: bool) (pts: (int * int)[]) =
        match pts.Length < 3 with
        | true -> pts
        | false ->
            let area = 
                pts |> Array.pairwise |> Array.fold (fun acc ((x1, y1), (x2, y2)) -> 
                    acc + int64 (x1 * y2 - x2 * y1)) 0L
            match (area < 0L) = clockwise with
            | true -> pts
            | false -> Array.rev pts

    /// <summary> Bounding box of a polygon in grid coordinates. </summary>
    let bounds (pts: (int * int)[]) =
        match pts.Length with
        | 0 -> (0, 0, 0, 0)
        | _ ->
            let xs = pts |> Array.map fst
            let ys = pts |> Array.map snd
            (Array.min xs, Array.min ys, Array.max xs, Array.max ys)

    /// <summary> Point-in-polygon check in grid coordinates. </summary>
    let pointInPolygon (p: int * int) (poly: (int * int)[]) =
        let px, py = p
        let n = poly.Length
        let pointsWithNext = Array.init n (fun i -> poly.[i], poly.[(i + 1) % n])
        
        pointsWithNext |> Array.fold (fun inside ((xi, yi), (xj, yj)) ->
            let intersect =
                ((yi > py) <> (yj > py)) &&
                (float px < float (xj - xi) * float (py - yi) / float (max 1 (abs (yj - yi))) * float (sign (yj - yi)) + float xi)
            match intersect with true -> not inside | false -> inside
        ) false

    /// <summary> Area of a polygon in logical grid units (integer). </summary>
    let polygonArea (pts: (int * int)[]) =
        match pts.Length < 3 with
        | true -> 0L
        | false ->
            let area2 = 
                pts |> Array.pairwise |> Array.sumBy (fun ((x1, y1), (x2, y2)) -> 
                    int64 (x1 * y2 - x2 * y1))
            abs area2 / 2L

    /// <summary> Calculates the net area of a polygon by subtracting the area of holes (integer). </summary>
    let polygonWithHolesArea (outer: (int * int)[]) (holes: (int * int)[][]) =
        let outerArea = polygonArea outer
        let holesArea = holes |> Array.sumBy polygonArea
        outerArea - holesArea

    /// <summary> Computes the logical centroid (average of grid indices). </summary>
    let centroid (pts: (int * int)[]) =
        match pts.Length with
        | 0 -> 0, 0
        | n ->
            let sx = pts |> Array.sumBy (fun (x, _) -> int64 x)
            let sy = pts |> Array.sumBy (fun (_, y) -> int64 y)
            int (sx / int64 n), int (sy / int64 n)

    /// <summary> Cleans a polygon using only integer operations. </summary>
    let cleanPolygon sqn pts =
        pts
        |> dedupeSequential
        |> removeSawtooth sqn
        |> dedupeSequential
        |> ensureClosed
        |> removeCollinear
        |> normalizeWinding true

    /// <summary> Hexel Polygon generation using integer grid indices. </summary>
    let hxlPgn (sqn: Sqn) (elv: int) (vtx: (int * int)[]): Hxl[] =
        match vtx.Length < 2 with
        | true -> [||]
        | false ->
            let stx, sty = Array.head vtx
            let xx, yy, _ =
                hxlLin sqn elv (RV(0,0,elv)) (RV(stx, sty, elv))
                |> Array.last
                |> hxlCrd
            let vt1 = Array.concat [| [|xx, yy|]; Array.tail vtx |]
            let verts = cleanPolygon sqn vt1
            
            verts |> Array.fold (fun (acc: ResizeArray<Hxl>, lastOpt) pt ->
                let (ix, iy) = pt
                match lastOpt with
                | None -> 
                    acc.Add(RV(ix, iy, elv))
                    (acc, Some (RV(ix, iy, elv)))
                | Some last ->
                    let seg = hxlLin sqn elv last (RV(ix, iy, elv))
                    acc.AddRange(seg)
                    (acc, Some (Array.last seg))
            ) (ResizeArray<Hxl>(), None) |> fst |> (fun ra -> ra.ToArray())
