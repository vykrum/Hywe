module ThreeD

open Coxel
open Microsoft.JSInterop

/// Renders an extruded polygon on a canvas via JS WebGL
/// Simple ear-clipping triangulation for concave, non-self-intersecting polygons.
let triangulatePolygon 
    (points: (float * float)[]) 
    : (float * float)[][] =
    match points with
    | null -> [||]
    | p when p.Length < 3 -> [||]
    | _ ->
        let cross (ax, ay) (bx, by) (cx, cy) =
            (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

        let pointInTriangle (ax, ay) (bx, by) (cx, cy) (px, py) =
            let c1 = cross (ax, ay) (bx, by) (px, py)
            let c2 = cross (bx, by) (cx, cy) (px, py)
            let c3 = cross (cx, cy) (ax, ay) (px, py)
            (c1 >= 0.0 && c2 >= 0.0 && c3 >= 0.0) || (c1 <= 0.0 && c2 <= 0.0 && c3 <= 0.0)

        let area = 
            points 
            |> Array.indexed 
            |> Array.fold (fun acc (i, (x1, y1)) ->
                let (x2, y2) = points.[(i + 1) % points.Length]
                acc + (x1 * y2 - x2 * y1)) 0.0

        let initialPts = 
            match area < 0.0 with
            | true -> points |> Array.rev |> Array.toList
            | false -> points |> Array.toList

        // The 'acc' must be a list of arrays (triangles)
        let rec clip (remaining: (float * float) list) (acc: (float * float) array list) (attempts: int) =
            match remaining with
            // Case 1: Exactly 3 points left - this is the final triangle
            | [p1; p2; p3] -> 
                ([| p1; p2; p3 |] :: acc) |> List.rev |> List.toArray
            
            // Case 2: Safety exit to prevent infinite recursion on invalid polygons
            | _ when attempts > remaining.Length -> 
                acc |> List.rev |> List.toArray
            
            // Case 3: More than 3 points - try to find an ear
            | _ ->
                let n = remaining.Length
                // We pick 3 consecutive points: the last one, the first, and the second
                let prev = remaining.[n - 1]
                let curr = remaining.[0]
                let next = remaining.[1]
                
                let isEar = 
                    match cross prev curr next > 0.0 with
                    | false -> false
                    | true ->
                        remaining 
                        |> List.exists (fun p -> 
                            // A point is inside the triangle if it's not one of the vertices
                            if p = prev || p = curr || p = next then false
                            else pointInTriangle prev curr next p)
                        |> not

                match isEar with
                | true ->
                    // Remove 'curr' and add triangle to accumulator
                    clip (remaining.Tail) ([| prev; curr; next |] :: acc) 0
                | false ->
                    // Rotate list: move head to tail and increment attempts
                    let rotated = remaining.Tail @ [remaining.Head]
                    clip rotated acc (attempts + 1)

        clip initialPts [] 0

/// Extrudes a polygon
let polygonMesh 
    (poly2D: (float * float)[]) 
    (height: float) 
    : (float * float * float)[][] =

    let basePoly = 
        match poly2D.Length with
        | n when n < 3 -> [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |]
        | _ -> poly2D

    match triangulatePolygon basePoly with
    | [||] -> [||]
    | tris ->
        // 1. Recursive Top Faces
        let rec getTopFaces i acc =
            match i < tris.Length with
            | false -> acc
            | true ->
                let tri = tris.[i] |> Array.map (fun (x, y) -> (x, y, height))
                getTopFaces (i + 1) (tri :: acc)

        // 2. Recursive Side Walls (2 triangles per edge)
        let rec getSideWalls i acc =
            match i < basePoly.Length with
            | false -> acc
            | true ->
                let (x1, y1) = basePoly.[i]
                let (x2, y2) = basePoly.[(i + 1) % basePoly.Length]
                
                let wall1 = [| (x1, y1, 0.0); (x2, y2, 0.0); (x2, y2, height) |]
                let wall2 = [| (x1, y1, 0.0); (x2, y2, height); (x1, y1, height) |]
                
                getSideWalls (i + 1) (wall2 :: wall1 :: acc)

        // Combine lists and convert to final array
        let topList = getTopFaces 0 []
        let allFaces = getSideWalls 0 topList
        
        allFaces |> List.toArray

let extrudePolygons
    (js: IJSRuntime)
    (canvasId: string)
    (cxl: Cxl[])
    (colors: string[])
    (initHeight: float)
    (elv: int)
    : Async<unit> =

    // 1. Helper: Point conversion
    let toPoly (x: Cxl) =
        cxlPrm x elv
        |> Geometry.filterOddSecondary x.Seqn
        |> Array.map (fun (xi, yi) -> (float xi, float yi))

    // 2. Helper: Color normalization
    let normalizeColor (rgba: string) =
        let parts = 
            rgba.Replace("rgba(", "").Replace(")", "").Split(',')
            |> Array.choose (fun s -> 
                match System.Double.TryParse(s.Trim()) with
                | true, v -> Some v
                | _ -> None)
        match parts with
        | [| r; g; b; _ |] 
        | [| r; g; b |] -> [| r / 255.0; g / 255.0; b / 255.0 |]
        | _ -> [| 0.8; 0.8; 0.8 |]

    // 3. Process initial data
    let polygonsWithColor =
        Array.zip colors (Array.map toPoly cxl)
        |> Array.choose (function
            | (clr, poly) when poly.Length >= 3 -> Some (poly, clr)
            | _ -> None)

    let (polygonsFinal: (float * float)[][]), (colorsFinal: string[]) =
        match polygonsWithColor with
        | [||] -> 
            [| [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |] |],
            [| "rgba(200,200,200,1)" |]
        | items -> Array.unzip items

    // 4. Loop-free Mesh Assembler (Tail Recursive)
    let rec buildMeshes i accMeshes accEdges accHeights =
        match i < polygonsFinal.Length with
        | false -> 
            (List.rev accMeshes |> List.toArray, 
             List.rev accEdges |> List.toArray, 
             List.rev accHeights |> List.toArray)
        | true ->
            let h = initHeight - float i * 0.01
            let poly = polygonsFinal.[i]
            
            // Generate mesh and transform for WebGL (Flipping Y)
            let mesh = 
                polygonMesh poly h 
                |> Array.map (Array.map (fun (x, y, z) -> [| x; -y; z |]))
            
            let edge = poly |> Array.map (fun (x, y) -> [| x; -y |])
            
            buildMeshes (i + 1) (mesh :: accMeshes) (edge :: accEdges) (h :: accHeights)

    // 5. Final Async Execution
    async {
        do! Async.Sleep 30
        
        let colorsJs = colorsFinal |> Array.map normalizeColor
        let meshes, edges, heights = buildMeshes 0 [] [] []

        do! js.InvokeVoidAsync("initWebGLExtrudedPolygons", canvasId, meshes, colorsJs, heights, edges).AsTask()
            |> Async.AwaitTask
    }