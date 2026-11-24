namespace Hywe.Core

/// <summary> 
/// Goxel (Geometry Engine) 
/// Provides geometric primitives and utilities for Hygrid shapes. 
/// Includes line generation, polygon cleaning, area calculations, and winding normalization.
/// </summary>

module Goxel =
    open System
    open Hexel
    open Coxel


    /// <summary>
    /// Generates a discrete line of hexels connecting start and end hexels using integer grid mathematics.
    /// </summary>
    /// <param name="sqn">Traversal sequence.</param>
    /// <param name="elv">Elevation layer.</param>
    /// <param name="stt">Start hexel.</param>
    /// <param name="enn">End hexel.</param>
    /// <returns>Array of intermediate and endpoint Hexels forming the line.</returns>
    /// <example>
    /// <code>
    /// let startPt = Hexel.AV(0, 0, 0)
    /// let endPt = Hexel.AV(10, 10, 0)
    /// let line = Goxel.hxlLin Hexel.Sqn.VRCWEE 0 startPt endPt
    /// </code>
    /// </example>
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
            | true -> step sy ey 2, step sx ex 1
            | false -> step sx ex 2, step sy ey 1
                
        let div = 
            match Array.length pty >= Array.length flt with  
            | true -> 
                let cnk1 = splitOddChunks (Array.length flt) pty
                Array.mapi (fun i a -> 
                    match seqH with
                    | true -> cnk1.[i] |> Array.map (fun b -> (a, b))
                    | false -> cnk1.[i] |> Array.map (fun b -> (b, a))
                ) flt |> Array.map (bumpEveryOther seqH) |> Array.concat
            | false -> 
                let cnk1 = splitOddChunks (Array.length pty) flt
                Array.mapi (fun i a -> 
                    match seqH with
                    | true -> cnk1.[i] |> Array.map (fun b -> (b, a))
                    | false -> cnk1.[i] |> Array.map (fun b -> (a, b))
                ) pty |> Array.map dropAlternate |> Array.concat

        let result = div |> Array.map (fun (a,b) -> RV(a, b, sz))
        match result.Length with 
        | 0 -> ([| stt; enn |] |> Array.distinct) 
        | _ -> result

    /// <summary>
    /// Removes aliasing/sawtooth artifacts from a sequence of hex coordinates along the primary axis.
    /// </summary>
    /// <param name="sqn">Traversal sequence.</param>
    /// <param name="arr">Array of integer 2D coordinate points.</param>
    /// <returns>Filtered array of points with oscillating sawtooth artifacts removed.</returns>
    /// <example>
    /// <code>
    /// let smoothed = Goxel.removeSawtooth Hexel.Sqn.VRCWEE [| (0, 0); (2, 1); (4, 0); (6, 1); (8, 0) |]
    /// </code>
    /// </example>
    let removeSawtooth (sqn : Sqn) (arr : (int*int)[]) : (int*int)[] =
        match arr.Length with
        | 0 -> [||]
        | n ->
            let (primary, secondary) = 
                match sqn with
                | Vertical   -> (fst, snd)
                | Horizontal -> (snd, fst)

            let rec processGroups i acc =
                match i < n with
                | false -> acc |> List.rev |> Array.concat
                | true ->
                    let rec findGroupEnd j =
                        match j + 1 < n && abs(primary arr.[j+1] - primary arr.[j]) = 2 with
                        | true -> findGroupEnd (j + 1)
                        | false -> j
                    
                    let j = findGroupEnd i
                    let groupLen = j - i + 1
                    
                    match groupLen > 3 with
                    | false -> 
                        processGroups (j + 1) (arr.[i..j] :: acc)
                    | true ->
                        let rec checkOscillating k =
                            match k < j with
                            | false -> true
                            | true ->
                                match abs(secondary arr.[k+1] - secondary arr.[k]) <> 1 with
                                | true -> false
                                | false -> checkOscillating (k + 1)
                        
                        match checkOscillating i with
                        | false -> processGroups (j + 1) (arr.[i..j] :: acc)
                        | true ->
                            let f = arr.[i]
                            let l = arr.[j]
                            let low = min (secondary f) (secondary l)
                            let newPts =
                                match sqn with
                                | Vertical   -> [| (low, snd f); (low, snd l) |]
                                | Horizontal -> [| (fst f, low); (fst l, low) |]
                            processGroups (j + 1) (newPts :: acc)
            
            processGroups 0 []

    /// <summary>
    /// Deduplicates consecutive identical coordinate points in a sequence.
    /// </summary>
    /// <param name="pts">Array of 2D coordinate points.</param>
    /// <returns>Array of points without adjacent duplicates.</returns>
    /// <example>
    /// <code>
    /// let deduped = Goxel.dedupeSequential [| (0, 0); (0, 0); (1, 1); (2, 2); (2, 2) |]
    /// </code>
    /// </example>
    let dedupeSequential (pts: (int * int)[]) =
        pts |> Array.fold (fun acc p -> 
            match acc with
            | [] -> [p]
            | head :: _ -> match head = p with true -> acc | false -> p :: acc
        ) [] |> List.rev |> Array.ofList

    /// <summary>
    /// Ensures a polygon coordinate sequence is closed by appending the first point to the end if not already equal.
    /// </summary>
    /// <param name="pts">Array of 2D coordinate points.</param>
    /// <returns>Closed array of points where the last point matches the first.</returns>
    /// <example>
    /// <code>
    /// let closed = Goxel.ensureClosed [| (0, 0); (10, 0); (10, 10); (0, 10) |]
    /// </code>
    /// </example>
    let ensureClosed (pts: (int * int)[]) =
        match pts.Length < 2 with
        | true -> pts
        | false ->
            match pts.[0] = pts.[pts.Length - 1] with
            | true -> pts
            | false -> Array.append pts [| pts.[0] |]

    /// <summary>
    /// Removes collinear points from a polygon sequence using integer cross products.
    /// </summary>
    /// <param name="pts">Array of 2D polygon vertices.</param>
    /// <returns>Simplified array of vertices with intermediate collinear points removed.</returns>
    /// <example>
    /// <code>
    /// let simplified = Goxel.removeCollinear [| (0, 0); (5, 0); (10, 0); (10, 10) |]
    /// </code>
    /// </example>
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

    /// <summary>
    /// Normalizes the vertex winding order of a polygon to clockwise or counterclockwise using signed area calculation.
    /// </summary>
    /// <param name="clockwise">True to enforce clockwise winding; false for counterclockwise.</param>
    /// <param name="pts">Array of 2D polygon vertices.</param>
    /// <returns>Array of vertices with the specified winding order.</returns>
    /// <example>
    /// <code>
    /// let cwPolygon = Goxel.normalizeWinding true [| (0, 0); (10, 0); (10, 10); (0, 10); (0, 0) |]
    /// </code>
    /// </example>
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

    /// <summary>
    /// Computes the bounding box of a polygon in grid coordinates: (minX, minY, maxX, maxY).
    /// </summary>
    /// <param name="pts">Array of 2D coordinate points.</param>
    /// <returns>4-tuple of (minX, minY, maxX, maxY).</returns>
    /// <example>
    /// <code>
    /// let minX, minY, maxX, maxY = Goxel.bounds [| (0, 5); (12, 2); (6, 18) |]
    /// </code>
    /// </example>
    let bounds (pts: (int * int)[]) =
        match Array.isEmpty pts with
        | true -> (0, 0, 0, 0)
        | false ->
            let xs = pts |> Array.map fst
            let ys = pts |> Array.map snd
            (Array.min xs, Array.min ys, Array.max xs, Array.max ys)

    /// <summary>
    /// Tests whether a 2D integer point lies inside or on the boundary of a polygon using the ray casting algorithm.
    /// </summary>
    /// <param name="p">Test point (x, y).</param>
    /// <param name="poly">Polygon vertex array.</param>
    /// <returns>True if the point is inside the polygon; otherwise false.</returns>
    /// <example>
    /// <code>
    /// let isInside = Goxel.pointInPolygon (5, 5) [| (0, 0); (10, 0); (10, 10); (0, 10) |]
    /// </code>
    /// </example>
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

    /// <summary>
    /// Calculates the area of a simple polygon in logical grid units using Gauss's area formula (Shoelace algorithm).
    /// </summary>
    /// <param name="pts">Array of 2D polygon vertices.</param>
    /// <returns>Polygon area as a 64-bit integer.</returns>
    /// <example>
    /// <code>
    /// let area = Goxel.polygonArea [| (0, 0); (10, 0); (10, 10); (0, 10); (0, 0) |]
    /// </code>
    /// </example>
    let polygonArea (pts: (int * int)[]) =
        match pts.Length < 3 with
        | true -> 0L
        | false ->
            let area2 = 
                pts |> Array.pairwise |> Array.sumBy (fun ((x1, y1), (x2, y2)) -> 
                    int64 (x1 * y2 - x2 * y1))
            abs area2 / 2L

    /// <summary>
    /// Calculates the net area of a polygon by subtracting the total area of any interior holes (islands).
    /// </summary>
    /// <param name="outer">Outer boundary polygon vertices.</param>
    /// <param name="holes">Array of inner hole polygons.</param>
    /// <returns>Net usable area as a 64-bit integer.</returns>
    /// <example>
    /// <code>
    /// let outer = [| (0, 0); (20, 0); (20, 20); (0, 20); (0, 0) |]
    /// let hole = [| (5, 5); (15, 5); (15, 15); (5, 15); (5, 5) |]
    /// let net = Goxel.polygonWithHolesArea outer [| hole |]
    /// </code>
    /// </example>
    let polygonWithHolesArea (outer: (int * int)[]) (holes: (int * int)[][]) =
        let outerArea = polygonArea outer
        let holesArea = holes |> Array.sumBy polygonArea
        outerArea - holesArea

    /// <summary>
    /// Computes the arithmetic centroid (average of grid indices) of a collection of points.
    /// </summary>
    /// <param name="pts">Array of 2D coordinate points.</param>
    /// <returns>Tuple of (centerX, centerY).</returns>
    /// <example>
    /// <code>
    /// let cx, cy = Goxel.centroid [| (0, 0); (10, 0); (10, 10); (0, 10) |]
    /// </code>
    /// </example>
    let centroid (pts: (int * int)[]) =
        match pts.Length with
        | 0 -> 0, 0
        | n ->
            let sx = pts |> Array.sumBy (fun (x, _) -> int64 x)
            let sy = pts |> Array.sumBy (fun (_, y) -> int64 y)
            int (sx / int64 n), int (sy / int64 n)

    /// <summary>
    /// Cleans a polygon by deduplicating sequential points, removing sawtooth aliasing, closing the loop, removing collinear points, and normalizing winding to clockwise.
    /// </summary>
    /// <param name="sqn">Traversal sequence.</param>
    /// <param name="pts">Raw input polygon vertices.</param>
    /// <returns>Cleaned, validated, clockwise polygon vertices.</returns>
    /// <example>
    /// <code>
    /// let cleaned = Goxel.cleanPolygon Hexel.Sqn.VRCWEE [| (0, 0); (10, 0); (10, 10); (0, 10) |]
    /// </code>
    /// </example>
    let cleanPolygon sqn pts =
        pts
        |> dedupeSequential
        |> removeSawtooth sqn
        |> dedupeSequential
        |> ensureClosed
        |> removeCollinear
        |> normalizeWinding true

    /// <summary>
    /// Generates a filled or outlined hexel polygon covering the region defined by integer vertices.
    /// </summary>
    /// <param name="sqn">Traversal sequence.</param>
    /// <param name="elv">Elevation layer.</param>
    /// <param name="vtx">Array of 2D polygon vertices.</param>
    /// <returns>Array of Hexels forming the perimeter and segments of the polygon.</returns>
    /// <example>
    /// <code>
    /// let boundaryHxls = Goxel.hxlPgn Hexel.Sqn.VRCWEE 0 [| (0, 0); (12, 0); (12, 12); (0, 12) |]
    /// </code>
    /// </example>
    let hxlPgn (sqn: Sqn) (elv: int) (vtx: (int * int)[]): Hxl[] =
        match vtx.Length < 2 with
        | true -> [||]
        | false ->
            let stx, sty = Array.head vtx
            let snappedStart = hxlVld sqn (RV(stx, sty, elv))
            let xx, yy, _ = hxlCrd snappedStart
            
            let vt1 = Array.concat [| [|xx, yy|]; Array.tail vtx |]
            let verts = cleanPolygon sqn vt1
            
            verts |> Array.fold (fun (acc: Hxl list, lastOpt) pt ->
                let (ix, iy) = pt
                let current = hxlVld sqn (RV(ix, iy, elv))
                match lastOpt with
                | None -> 
                    (current :: acc, Some current)
                | Some last ->
                    let seg = hxlLin sqn elv last current
                    let newAcc = seg |> Array.fold (fun a c -> c :: a) acc
                    (newAcc, Some (Array.last seg))
            ) ([], None) |> fst |> List.rev |> List.toArray

    // --- Geometry Parsing ---

    /// <summary>
    /// Parses a comma-separated string of coordinate numbers (e.g. "0,0,10,0,10,10,0,10") into an array of 2D float points.
    /// </summary>
    /// <param name="s">Comma-separated string of coordinates.</param>
    /// <returns>Array of (float * float) points.</returns>
    /// <example>
    /// <code>
    /// let points = Goxel.parsePolygon "0,0, 10,0, 10,10, 0,10"
    /// </code>
    /// </example>
    let parsePolygon (s: string) : (float * float)[] =
        s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun x ->
            match Double.TryParse(x.Trim()) with
            | true, v -> Some v
            | _ -> None)
        |> fun nums ->
            match nums.Length % 2 with
            | 0 ->
                nums
                |> Array.chunkBySize 2
                |> Array.choose (function
                    | [| a; b |] -> Some (a, b)
                    | _ -> None)
            | _ -> [||]

    /// <summary>
    /// Parses a hyphen-separated string of polygons (e.g. "0,0,10,10 - 20,20,30,30") representing multiple islands into a 2D array of float points.
    /// </summary>
    /// <param name="s">Hyphen-separated string of polygon coordinates.</param>
    /// <returns>Array of float polygon vertex arrays.</returns>
    /// <example>
    /// <code>
    /// let islands = Goxel.parsePolyIslands "0,0,10,0,10,10 - 20,20,25,20,25,25"
    /// </code>
    /// </example>
    let parsePolyIslands (s: string) : (float * float)[][] =
        match String.IsNullOrWhiteSpace s with
        | true -> [||]
        | false ->
            s.Split('-', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parsePolygon

    /// <summary>
    /// Parses a comma-separated string of coordinates into rounded integer (int * int) grid vertices.
    /// </summary>
    /// <param name="value">Comma-separated coordinate string.</param>
    /// <returns>Array of integer (x, y) vertices.</returns>
    /// <example>
    /// <code>
    /// let coords = Goxel.parseCoords "0,0, 12,0, 12,8, 0,8"
    /// </code>
    /// </example>
    let parseCoords value =
        parsePolygon value
        |> Array.map (fun (x, y) -> int (System.Math.Round(x)), int (System.Math.Round(y)))

    /// <summary>
    /// Parses a hyphen-separated string of island polygons into rounded integer (int * int)[][] grid vertices.
    /// </summary>
    /// <param name="value">Hyphen-separated string of island polygon coordinates.</param>
    /// <returns>Array of integer island vertex arrays.</returns>
    /// <example>
    /// <code>
    /// let islandCoords = Goxel.parseIslands "2,2,4,2,4,4,2,4 - 6,6,8,6,8,8,6,8"
    /// </code>
    /// </example>
    let parseIslands value =
        parsePolyIslands value
        |> Array.map (Array.map (fun (x, y) -> int (System.Math.Round(x)), int (System.Math.Round(y))))
