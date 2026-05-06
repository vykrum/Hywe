module ThreeD

open Hywe.Core
open Hywe.Core.Coxel
open Microsoft.JSInterop
open Graphics

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
let triangulatePolygon (points: (float * float)[]) : (float * float)[][] =
    if points = null || points.Length < 3 then [||]
    else
        let n = points.Length
        let indices = Array.init n id
        
        let cross (ax, ay) (bx, by) (cx, cy) =
            (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

        let area = 
            let mutable a = 0.0
            for i = 0 to n - 1 do
                let (x1, y1) = points.[i]
                let (x2, y2) = points.[(i + 1) % n]
                a <- a + (x1 * y2 - x2 * y1)
            a

        let workingIndices = 
            if area < 0.0 then indices |> Array.rev |> ResizeArray
            else indices |> ResizeArray

        let pointInTriangle (ax, ay) (bx, by) (cx, cy) (px, py) =
            let c1 = cross (ax, ay) (bx, by) (px, py)
            let c2 = cross (bx, by) (cx, cy) (px, py)
            let c3 = cross (cx, cy) (ax, ay) (px, py)
            (c1 >= 0.0 && c2 >= 0.0 && c3 >= 0.0) || (c1 <= 0.0 && c2 <= 0.0 && c3 <= 0.0)

        let result = ResizeArray<(float * float)[]>()
        let mutable attempts = 0
        while workingIndices.Count > 3 && attempts < workingIndices.Count do
            let i = 0
            let prev = workingIndices.[(if i = 0 then workingIndices.Count - 1 else i - 1)]
            let curr = workingIndices.[i]
            let next = workingIndices.[(i + 1) % workingIndices.Count]
            
            let p1, p2, p3 = points.[prev], points.[curr], points.[next]
            
            let mutable isEar = cross p1 p2 p3 > 0.0
            if isEar then
                for j = 0 to workingIndices.Count - 1 do
                    let idx = workingIndices.[j]
                    if idx <> prev && idx <> curr && idx <> next then
                        if pointInTriangle p1 p2 p3 points.[idx] then
                            isEar <- false
            
            if isEar then
                result.Add([| p1; p2; p3 |])
                workingIndices.RemoveAt(i)
                attempts <- 0
            else
                // Rotate
                let head = workingIndices.[0]
                workingIndices.RemoveAt(0)
                workingIndices.Add(head)
                attempts <- attempts + 1
        
        if workingIndices.Count = 3 then
            result.Add([| points.[workingIndices.[0]]; points.[workingIndices.[1]]; points.[workingIndices.[2]] |])
        
        result.ToArray()

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
    (levelElevations: float[])
    (viewLocked: bool)
    : Async<unit> =

    // 1. Helper: Point conversion
    let toPoly (x: Cxl) =
        let (_, _, z) = Hexel.hxlCrd x.Base
        svgCxlPrm x z
        |> svgCleanPolygon x.Seqn
        |> Array.map (fun p -> svgToCartesian x.Seqn p)
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
    let processedData =
        cxl 
        |> Array.mapi (fun i c -> 
            let poly = toPoly c
            let clr = if i < colors.Length then colors.[i] else "rgba(200,200,200,1)"
            (c, poly, clr))
        |> Array.filter (fun (_, poly, _) -> poly.Length >= 3)

    let polygonsFinal = processedData |> Array.map (fun (_, p, _) -> p)
    let colorsFinal = processedData |> Array.map (fun (_, _, c) -> c)
    let cxlsFinal = processedData |> Array.map (fun (c, _, _) -> c)

    // Calculate heights for each level
    let diffs = 
        if levelElevations.Length < 2 then [| 3.0 |]
        else 
            [| 0 .. levelElevations.Length - 2 |]
            |> Array.map (fun i -> levelElevations.[i+1] - levelElevations.[i])
    
    let avgHeight = if Array.isEmpty diffs then 3.0 else Array.average diffs

    // 4. Loop-free Mesh Assembler (Tail Recursive)
    let rec buildMeshes i accMeshes accEdges accHeights accBaseHeights accCentroids =
        match i < polygonsFinal.Length with
        | false -> 
            (List.rev accMeshes |> List.toArray, 
             List.rev accEdges |> List.toArray, 
             List.rev accHeights |> List.toArray,
             List.rev accBaseHeights |> List.toArray,
             List.rev accCentroids |> List.toArray)
        | true ->
            let c = cxlsFinal.[i]
            let (_, _, z) = Hexel.hxlCrd c.Base
            
            let baseH = if z < levelElevations.Length then levelElevations.[z] else float z * avgHeight
            let h = 
                (if z < diffs.Length then diffs.[z]
                 else avgHeight) - 0.05
            
            let poly = polygonsFinal.[i]
            
            // Generate mesh and transform for WebGL (Flipping Y)
            let mesh = 
                polygonMesh poly 
                |> Array.map (Array.map (fun (x, y) -> 
                    let (cx, cy) = Geometry.toCartesian c.Seqn (int (System.Math.Round(x)), int (System.Math.Round(y)))
                    [| cx; -cy |]))
            
            let edge = 
                poly |> Array.map (fun (x, y) -> 
                    let (cx, cy) = Geometry.toCartesian c.Seqn (int (System.Math.Round(x)), int (System.Math.Round(y)))
                    [| cx; -cy |])
            
            let rawCx = if poly.Length > 0 then poly |> Array.averageBy fst else 0.0
            let rawCy = if poly.Length > 0 then poly |> Array.averageBy snd else 0.0
            let cx, cy = Geometry.toCartesian c.Seqn (int (System.Math.Round(rawCx)), int (System.Math.Round(rawCy)))
            let centroid = [| cx; -cy; baseH + h / 2.0 |]
            
            buildMeshes (i + 1) (mesh :: accMeshes) (edge :: accEdges) (h :: accHeights) (baseH :: accBaseHeights) (centroid :: accCentroids)

    // 5. Final Async Execution
    async {
        do! Async.Sleep 30
        
        let colorsJs = colorsFinal |> Array.map normalizeColor
        let meshes, edges, heights, baseHeights, centroids = buildMeshes 0 [] [] [] [] []
        
        let projMatrix = Mat4.perspective (System.Math.PI / 4.0) (1.5) 0.1 100.0 // Aspect 3/2 matches container

        do! js.InvokeVoidAsync("initWebGLExtrudedPolygons", 
                                canvasId, meshes, colorsJs, heights, baseHeights, edges, centroids, 
                                vsSource, fsSource, projMatrix, viewLocked).AsTask()
            |> Async.AwaitTask
    }
