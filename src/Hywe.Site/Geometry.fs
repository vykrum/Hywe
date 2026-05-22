namespace Hywe.Site

open System

module Geometry =

    let inline sqr x = x * x
    let distanceSq (a: Point) (b: Point) = sqr (a.X - b.X) + sqr (a.Y - b.Y)
    let withinRadiusSq (a: Point) (b: Point) (r: float) = distanceSq a b <= r*r

    let distancePointToSegmentSq p a b =
        let vx = b.X - a.X
        let vy = b.Y - a.Y
        let wx = p.X - a.X
        let wy = p.Y - a.Y
        let c1 = wx * vx + wy * vy
        match c1 <= 0.0 with
        | true -> distanceSq p a
        | false ->
            let c2 = vx * vx + vy * vy
            match c2 <= c1 with
            | true -> distanceSq p b
            | false ->
                let t = c1 / c2
                let proj = { X = a.X + t*vx; Y = a.Y + t*vy }
                distanceSq p proj

    /// <summary> 
    /// Ray-casting algorithm to determine if a point is inside a polygon.
    /// Optimized for performance by using manual loops to avoid array allocations and lambdas.
    /// </summary>
    let isInsidePolygon (poly: Point[]) (pt: Point) =
        let n = poly.Length
        let rec loop i j inside =
            match i < n with
            | false -> inside
            | true ->
                let vi = poly.[i]
                let vj = poly.[j]
                let newInside = 
                    match (vi.Y > pt.Y) <> (vj.Y > pt.Y) with
                    | true ->
                        let xIntersect = (vj.X - vi.X) * (pt.Y - vi.Y) / (vj.Y - vi.Y) + vi.X
                        match pt.X < xIntersect with
                        | true -> not inside
                        | false -> inside
                    | false -> inside
                loop (i + 1) i newInside
        loop 0 (n - 1) false

    let isPolygonInside outer inner =
        inner |> Array.forall (isInsidePolygon outer)

    let private eps = 1e-9

    let private orient (p: Point) (q: Point) (r: Point) =
        let v = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
        match abs v < eps with
        | true -> 0
        | false -> 
            match v > 0.0 with
            | true -> 1
            | false -> 2

    let private onSegment (p: Point) (q: Point) (r: Point) =
        q.X <= max p.X r.X + eps && q.X >= min p.X r.X - eps &&
        q.Y <= max p.Y r.Y + eps && q.Y >= min p.Y r.Y - eps

    let edgesIntersect (p1: Point) (q1: Point) (p2: Point) (q2: Point) =
        let o1 = orient p1 q1 p2
        let o2 = orient p1 q1 q2
        let o3 = orient p2 q2 p1
        let o4 = orient p2 q2 q1

        match o1 <> o2 && o3 <> o4 with
        | true -> true
        | false ->
            match o1 = 0 && onSegment p1 p2 q1 with
            | true -> true
            | false ->
                match o2 = 0 && onSegment p1 q2 q1 with
                | true -> true
                | false ->
                    match o3 = 0 && onSegment p2 p1 q2 with
                    | true -> true
                    | false ->
                        match o4 = 0 && onSegment p2 q1 q2 with
                        | true -> true
                        | false -> false

    let polygonSelfIntersects (points: Point[]) =
        let n = points.Length
        let rec loop i j =
            match i >= n with
            | true -> false
            | false ->
                match j >= n with
                | true -> loop (i + 1) (i + 2)
                | false ->
                    match abs (i - j) = 1 || abs (i - j) = n - 1 with
                    | true -> loop i (j + 1)
                    | false ->
                        match edgesIntersect points.[i] points.[(i+1)%n] points.[j] points.[(j+1)%n] with
                        | true -> true
                        | false -> loop i (j + 1)
        loop 0 2

    let polygonsIntersect (polyA: Point[]) (polyB: Point[]) : bool =
        let edgesA = [| for i in 0 .. polyA.Length - 1 -> (polyA.[i], polyA.[(i + 1) % polyA.Length]) |]
        let edgesB = [| for i in 0 .. polyB.Length - 1 -> (polyB.[i], polyB.[(i + 1) % polyB.Length]) |]

        edgesA |> Array.exists (fun (a1,a2) ->
            edgesB |> Array.exists (fun (b1,b2) ->
                edgesIntersect a1 a2 b1 b2))

    let isEntryPointValid (outer: Point[]) (islands: Point[][]) (pt: Point) =
        let clearanceSq = 40.0 * 40.0 // Increased clearance

        let tooClose (poly: Point[]) =
            let n = poly.Length
            let rec loop i j =
                match i < n with
                | false -> false
                | true ->
                    let a = poly.[j]
                    let b = poly.[i]
                    match distancePointToSegmentSq pt a b <= clearanceSq with
                    | true -> true
                    | false -> loop (i + 1) i
            loop 0 (n - 1)

        match not (isInsidePolygon outer pt) with
        | true -> false
        | false ->
            let rec checkIslands idx =
                match idx < islands.Length with
                | false -> false
                | true ->
                    match isInsidePolygon islands.[idx] pt with
                    | true -> true
                    | false -> checkIslands (idx + 1)
            
            match checkIslands 0 with
            | true -> false
            | false ->
                match tooClose outer with
                | true -> false
                | false ->
                    let rec checkIslandClose idx =
                        match idx < islands.Length with
                        | false -> false
                        | true ->
                            match tooClose islands.[idx] with
                            | true -> true
                            | false -> checkIslandClose (idx + 1)
                    not (checkIslandClose 0)

    let closestValidEntryPoint (outer: Point[]) (islands: Point[][]) =
        let centroid = 
            let sx, sy = outer |> Array.fold (fun (ax, ay) p -> (ax + p.X, ay + p.Y)) (0.0, 0.0)
            { X = sx / float outer.Length; Y = sy / float outer.Length }

        let step = 10.0
        let maxSearch = 400.0

        let rec searchR r =
            match r > maxSearch with
            | true -> centroid
            | false ->
                let steps = max 8 (int (r / 2.0))
                let rec searchAngle i =
                    match i >= steps with
                    | true -> None
                    | false ->
                        let angle = 2.0 * Math.PI * float i / float steps
                        let pt = { X = centroid.X + r * Math.Cos(angle); Y = centroid.Y + r * Math.Sin(angle) }
                        match isEntryPointValid outer islands pt with
                        | true -> Some pt
                        | false -> searchAngle (i + 1)
                
                match searchAngle 0 with
                | Some pt -> pt
                | None -> searchR (r + step)

        searchR 0.0

    let isConfigurationValid (outer: Point[]) (islands: Point[][]) =
        let allIslandsValid = 
            islands 
            |> Array.forall (fun isl -> 
                not (polygonSelfIntersects isl) && 
                isPolygonInside outer isl)
                
        let noIslandIntersections =
            islands 
            |> Array.mapi (fun i a -> 
                islands 
                |> Array.mapi (fun j b -> i, j, b) 
                |> Array.forall (fun (idxA, idxB, bPoly) -> 
                    match idxA = idxB with 
                    | true -> true 
                    | false -> not (polygonsIntersect a bPoly)))
            |> Array.forall id

        match not (polygonSelfIntersects outer) with
        | true -> allIslandsValid && noIslandIntersections
        | false -> false
