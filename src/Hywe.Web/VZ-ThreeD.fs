module ThreeD

open Hywe.Core
open Hywe.Core.Coxel
open Microsoft.JSInterop
open Graphics

module Mat4 =
    let create() = 
        [|
            1.0; 0.0; 0.0; 0.0
            0.0; 1.0; 0.0; 0.0
            0.0; 0.0; 1.0; 0.0
            0.0; 0.0; 0.0; 1.0
        |]
    
    let perspective (fovy: float) (aspect: float) (near: float) (far: float) =
        let f = 1.0 / System.Math.Tan(fovy / 2.0)
        let nf = 1.0 / (near - far)
        [|
            f / aspect; 0.0; 0.0; 0.0
            0.0; f; 0.0; 0.0
            0.0; 0.0; (far + near) * nf; -1.0
            0.0; 0.0; (2.0 * far * near) * nf; 0.0
        |]
        
    let lookAt (eye: float[]) (target: float[]) (up: float[]) =
        let ex, ey, ez = eye.[0], eye.[1], eye.[2]
        let tx, ty, tz = target.[0], target.[1], target.[2]
        let ux, uy, uz = up.[0], up.[1], up.[2]
        
        let dx, dy, dz = ex - tx, ey - ty, ez - tz
        let lenZ = System.Math.Sqrt(dx * dx + dy * dy + dz * dz)
        let zx, zy, zz = dx / lenZ, dy / lenZ, dz / lenZ
        
        let cx, cy, cz = uy * zz - uz * zy, uz * zx - ux * zz, ux * zy - uy * zx
        let lenX = System.Math.Sqrt(cx * cx + cy * cy + cz * cz)
        let xx, xy, xz = 
            match lenX with
            | 0.0 -> 1.0, 0.0, 0.0
            | l -> cx / l, cy / l, cz / l
        
        let yx, yy, yz = zy * xz - zz * xy, zz * xx - zx * xz, zx * xy - zy * xx
        
        [|
            xx; yx; zx; 0.0
            xy; yy; zy; 0.0
            xz; yz; zz; 0.0
            -(xx * ex + xy * ey + xz * ez)
            -(yx * ex + yy * ey + yz * ez)
            -(zx * ex + zy * ey + zz * ez)
            1.0
        |]

/// <summary>
/// Simple ear-clipping triangulation for concave, non-self-intersecting polygons.
/// </summary>
/// <param name="points">The 2D points forming the polygon boundary.</param>
/// <returns>An array of triangles (each defined by 3 points).</returns>
let triangulatePolygon (points: (float * float)[]) : (float * float)[][] =
    match points with
    | null -> [||]
    | pts when pts.Length < 3 -> [||]
    | pts ->
        let n = pts.Length
        let cross (ax, ay) (bx, by) (cx, cy) =
            (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

        let area = 
            [ 0 .. n - 1 ]
            |> List.sumBy (fun i ->
                let (x1, y1) = pts.[i]
                let (x2, y2) = pts.[(i + 1) % n]
                x1 * y2 - x2 * y1)

        let initialIndices = 
            match area < 0.0 with
            | true -> [ n - 1 .. -1 .. 0 ]
            | false -> [ 0 .. n - 1 ]

        let pointInTriangle (ax, ay) (bx, by) (cx, cy) (px, py) =
            let c1 = cross (ax, ay) (bx, by) (px, py)
            let c2 = cross (bx, by) (cx, cy) (px, py)
            let c3 = cross (cx, cy) (ax, ay) (px, py)
            (c1 >= 0.0 && c2 >= 0.0 && c3 >= 0.0) || (c1 <= 0.0 && c2 <= 0.0 && c3 <= 0.0)

        let rec clip (ring: int list) (attempts: int) (acc: (float * float)[] list) : (float * float)[][] =
            match ring with
            | [ i0; i1; i2 ] ->
                [| pts.[i0]; pts.[i1]; pts.[i2] |] :: acc
                |> List.rev
                |> List.toArray
            | curr :: next :: rest when attempts < ring.Length ->
                let prev = List.last rest
                let p1, p2, p3 = pts.[prev], pts.[curr], pts.[next]
                let isConvex = cross p1 p2 p3 > 0.0
                let hasNoInternalPoints =
                    rest
                    |> List.take (rest.Length - 1)
                    |> List.forall (fun idx -> not (pointInTriangle p1 p2 p3 pts.[idx]))

                match isConvex && hasNoInternalPoints with
                | true ->
                    let triangle = [| p1; p2; p3 |]
                    clip (next :: rest) 0 (triangle :: acc)
                | false ->
                    let rotated = next :: rest @ [ curr ]
                    clip rotated (attempts + 1) acc
            | _ ->
                acc
                |> List.rev
                |> List.toArray

        clip initialIndices 0 []

/// <summary>
/// Triangulates a polygon, providing fallback coordinates for invalid input.
/// </summary>
/// <param name="poly2D">The polygon coordinates.</param>
/// <returns>A triangulated mesh.</returns>
let polygonMesh 
    (poly2D: (float * float)[]) 
    : (float * float)[][] =

    let basePoly = 
        match poly2D with
        | null -> [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |]
        | pts when pts.Length < 3 -> [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |]
        | pts -> pts

    triangulatePolygon basePoly

/// <summary>
/// Prepares coxel layout geometry and dispatches it to the WebGPU rendering pipeline.
/// Transforms 2D architectural coordinates into 3D extruded meshes with precise level elevations.
/// </summary>
/// <param name="js">The JavaScript runtime instance.</param>
/// <param name="canvasId">The target HTML canvas ID.</param>
/// <param name="cxl">Array of coxels to render.</param>
/// <param name="colors">Array of corresponding color assignments.</param>
/// <param name="levelElevations">The base Z-elevations for each architectural level.</param>
/// <param name="viewLocked">Determines if the camera is locked or interactive.</param>
let extrudePolygons
    (js: IJSRuntime)
    (canvasId: string)
    (cxl: Cxl[])
    (colors: string[])
    (levelElevations: float[])
    (viewLocked: bool)
    : Async<unit> =
    async {
        // Register shaders before initialization
        do! js.InvokeVoidAsync("registerWebGPUShaders", 
                                Hywe.Shaders.computeWgsl, 
                                Hywe.Shaders.renderWgsl, 
                                Hywe.Shaders.postProcessWgsl).AsTask()
            |> Async.AwaitTask

        // 1. Helper: Point conversion
        let toPoly (x: Cxl) =
            let (_, _, z) = Hexel.hxlCrd x.Base
            svgCxlPrm x z
            |> svgCleanPolygon x.Seqn
            |> Array.map (svgToCartesian x.Seqn)
            |> fun pts -> 
                match pts with
                | [||] -> [||]
                | _ when pts.[0] = pts.[pts.Length - 1] -> pts.[0 .. pts.Length - 2]
                | _ -> pts

        // 2. Helper: Color normalization
        let normalizeColor (rgba: string) =
            let parts = 
                rgba.Replace("rgba(", "").Replace("rgb(", "").Replace(")", "").Split(',')
                |> Array.choose (fun s -> 
                    match System.Double.TryParse(s.Trim()) with
                    | true, v -> Some v
                    | _ -> None)
            match parts with
            | [| r; g; b; a |] -> [| r / 255.0; g / 255.0; b / 255.0; a |]
            | [| r; g; b |] -> [| r / 255.0; g / 255.0; b / 255.0; 1.0 |]
            | _ -> [| 0.8; 0.8; 0.8; 1.0 |]

        // 3. Process initial data
        let processedData =
            cxl 
            |> Array.mapi (fun i c -> 
                let poly = toPoly c
                let clr = colors |> Array.tryItem i |> Option.defaultValue "rgba(200,200,200,1)"
                (c, poly, clr))
            |> Array.filter (fun (_, poly, _) -> poly.Length >= 3)

        // Calculate heights for each level
        let diffs = 
            match levelElevations with
            | null | [||] | [| _ |] -> [| 3.0 |]
            | elevations ->
                elevations
                |> Array.pairwise
                |> Array.map (fun (curr, next) -> next - curr)
        
        let avgHeight = 
            match diffs with
            | [||] -> 3.0
            | ds -> Array.average ds

        // 4. Functional Mesh Assembly
        let geometries =
            processedData
            |> Array.map (fun (c, (poly: (float * float)[]), _) ->
                let (_, _, z) = Hexel.hxlCrd c.Base
                let baseH = levelElevations |> Array.tryItem z |> Option.defaultValue (float z * avgHeight)
                let h = (diffs |> Array.tryItem z |> Option.defaultValue avgHeight) - 0.05

                let toCanvasPoint (x: float, y: float) =
                    let (cx, cy) = toCartesian c.Seqn (int (System.Math.Round(x)), int (System.Math.Round(y)))
                    [| cx; -cy |]

                let mesh =
                    polygonMesh poly
                    |> Array.map (Array.map toCanvasPoint)

                let edge = poly |> Array.map toCanvasPoint

                let rawCx = match poly with [||] -> 0.0 | pts -> Array.averageBy fst pts
                let rawCy = match poly with [||] -> 0.0 | pts -> Array.averageBy snd pts
                let (cx, cy) = toCartesian c.Seqn (int (System.Math.Round(rawCx)), int (System.Math.Round(rawCy)))
                let centroid = [| cx; -cy; baseH + h / 2.0 |]

                {| Mesh = mesh; Edge = edge; Height = h; BaseHeight = baseH; Centroid = centroid |})

        do! Async.Sleep 30

        let meshes = geometries |> Array.map (fun g -> g.Mesh)
        let edges = geometries |> Array.map (fun g -> g.Edge)
        let heights = geometries |> Array.map (fun g -> g.Height)
        let baseHeights = geometries |> Array.map (fun g -> g.BaseHeight)
        let centroids = geometries |> Array.map (fun g -> g.Centroid)
        let colorsJs = processedData |> Array.map (fun (_, _, clr) -> normalizeColor clr)
        
        let projMatrix = Mat4.perspective (System.Math.PI / 4.0) 1.5 0.1 100.0 // Aspect 3/2 matches container

        match meshes with
        | [||] -> ()
        | _ ->
            do! js.InvokeVoidAsync("initWebGPUExtrudedPolygons", 
                                    canvasId, meshes, colorsJs, heights, baseHeights, edges, centroids, 
                                    projMatrix, viewLocked).AsTask()
                |> Async.AwaitTask
    }
