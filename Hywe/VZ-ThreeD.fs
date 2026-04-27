module ThreeD

open Coxel
open Microsoft.JSInterop
open Shaders

module Mat4 =
    let create() = Array.zeroCreate<float> 16 |> fun a -> a.[0]<-1.0; a.[5]<-1.0; a.[10]<-1.0; a.[15]<-1.0; a
    
    let perspective (fovy: float) (aspect: float) (near: float) (far: float) =
        let out = Array.zeroCreate<float> 16
        let f = 1.0 / System.Math.Tan(fovy / 2.0)
        out.[0] <- f / aspect
        out.[5] <- f
        out.[10] <- (far + near) / (near - far)
        out.[11] <- -1.0
        out.[14] <- (2.0 * far * near) / (near - far)
        out
        
    let lookAt (eye: float[]) (target: float[]) (up: float[]) =
        let out = Array.zeroCreate<float> 16
        let ex, ey, ez = eye.[0], eye.[1], eye.[2]
        let tx, ty, tz = target.[0], target.[1], target.[2]
        let ux, uy, uz = up.[0], up.[1], up.[2]
        
        let mutable zx, zy, zz = ex - tx, ey - ty, ez - tz
        let lenZ = System.Math.Sqrt(zx*zx + zy*zy + zz*zz)
        zx <- zx / lenZ; zy <- zy / lenZ; zz <- zz / lenZ
        
        let mutable xx, xy, xz = uy * zz - uz * zy, uz * zx - ux * zz, ux * zy - uy * zx
        let lenX = System.Math.Sqrt(xx*xx + xy*xy + xz*xz)
        if lenX = 0.0 then xx <- 1.0; xy <- 0.0; xz <- 0.0
        else xx <- xx / lenX; xy <- xy / lenX; xz <- xz / lenX
        
        let yx, yy, yz = zy * xz - zz * xy, zz * xx - zx * xz, zx * xy - zy * xx
        
        out.[0] <- xx; out.[1] <- yx; out.[2] <- zx; out.[3] <- 0.0
        out.[4] <- xy; out.[5] <- yy; out.[6] <- zy; out.[7] <- 0.0
        out.[8] <- xz; out.[9] <- yz; out.[10] <- zz; out.[11] <- 0.0
        out.[12] <- -(xx * ex + xy * ey + xz * ez)
        out.[13] <- -(yx * ex + yy * ey + yz * ez)
        out.[14] <- -(zx * ex + zy * ey + zz * ez)
        out.[15] <- 1.0
        out

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
    : (float * float)[][] =

    let basePoly = 
        match poly2D.Length with
        | n when n < 3 -> [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |]
        | _ -> poly2D

    match triangulatePolygon basePoly with
    | [||] -> [||]
    | tris -> tris

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
        |> Geometry.cleanPolygon x.Seqn
        |> fun pts -> 
            if pts.Length > 0 && pts.[0] = pts.[pts.Length - 1] then 
                pts.[0 .. pts.Length - 2] 
            else pts

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
    let rec buildMeshes i accMeshes accEdges accHeights accCentroids =
        match i < polygonsFinal.Length with
        | false -> 
            (List.rev accMeshes |> List.toArray, 
             List.rev accEdges |> List.toArray, 
             List.rev accHeights |> List.toArray,
             List.rev accCentroids |> List.toArray)
        | true ->
            let h = initHeight - float i * 0.01
            let poly = polygonsFinal.[i]
            
            // Generate mesh and transform for WebGL (Flipping Y)
            let mesh = 
                polygonMesh poly 
                |> Array.map (Array.map (fun (x, y) -> [| x; -y |]))
            
            let edge = poly |> Array.map (fun (x, y) -> [| x; -y |])
            
            let cx = if poly.Length > 0 then poly |> Array.averageBy fst else 0.0
            let cy = if poly.Length > 0 then poly |> Array.averageBy snd else 0.0
            let centroid = [| cx; -cy; h / 2.0 |]
            
            buildMeshes (i + 1) (mesh :: accMeshes) (edge :: accEdges) (h :: accHeights) (centroid :: accCentroids)

    // 5. Final Async Execution
    async {
        do! Async.Sleep 30
        
        let colorsJs = colorsFinal |> Array.map normalizeColor
        let meshes, edges, heights, centroids = buildMeshes 0 [] [] [] []
        
        let projMatrix = Mat4.perspective (System.Math.PI / 4.0) (1.5) 0.1 100.0 // Aspect 3/2 matches container

        do! js.InvokeVoidAsync("initWebGLExtrudedPolygons", 
                               canvasId, meshes, colorsJs, heights, edges, centroids, 
                               vsSource, fsSource, projMatrix).AsTask()
            |> Async.AwaitTask
    }