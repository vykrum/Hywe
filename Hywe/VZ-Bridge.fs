module Bridge

open Bolero
open Bolero.Html
open Hexel
open Coxel
open Microsoft.JSInterop
///

type hxgn = Template<
      """ <polygon 
      points="${pt}" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      opacity = "0.75"
      stroke-opacity="0.175"
      >""">

type plgn = Template<
        """ <polygon 
        points="${pt}" 
        fill="${cl}"
        stroke="${st}"
        stroke-width="${sw}"
        opacity = "${op}"
        >""">

type svln = Template<
        """<line
        x1 = "${x1}"
        y1 = "${y1}"
        x2 = "${x2}"
        y2 = "${y2}"
        stroke = "${cl}"
        stroke-with = "2"
        >""">

type crPh = Template<
            """<path
            id = "${pathid}"
            fill = "none"
            d="M ${sx},${sy}
           A ${r},${r} 0 1,1 ${ex},${ey}
           A ${r},${r} 0 1,1 ${sx},${sy}"
            >""">

type crCl = Template<
    """<circle
        cx="${cx}" 
        cy="${cy}" 
        r="5" 
        fill="${cl}" />""">

type crTx = Template<
    """<text
        id="${pth}"
        font-weight="${fw}"
        fill="${fl}"
        text-decoration="${td}"
        font-size="10px"
        font-family="Verdana"
        text-anchor="middle"
        style="text-transform: lowercase">
        <textPath
            href="#${pth}"
            letter-spacing="0.5px"
            startOffset="50%">
            ${nm}
        </textPath>
    </text>""">

type svtx = Template<
        """<text 
        x="${xx}" 
        y="${yy}"
        width = "50px"
        font-size = "10px"
        font-family="Verdana"
        text-anchor="middle"
        dominant-baseline="middle"
        fill = "#808080"
        opacity = "1"
        >${nm}</text> """>

///

/// Integer-safe polygon centroid
let polygonCentroid (poly: (int * int)[]) =
    match poly with
    | [||] -> 0, 0
    | [|p|] -> p
    | _ ->
        let (sx, sy, a) =
            poly
            |> Array.pairwise
            |> Array.fold (fun (sx, sy, a) ((x1, y1), (x2, y2)) ->
                let cross = x1 * y2 - x2 * y1
                (sx + (x1 + x2) * cross,
                 sy + (y1 + y2) * cross,
                 a + cross)
            ) (0, 0, 0)

        match a with
        | 0 -> poly.[0]
        | _ -> (sx / (3 * a), sy / (3 * a))

/// Integer point-in-polygon
let pointInPolygon (px, py) (poly: (int * int)[]) =
    let rec loop i j acc =
        match i < poly.Length with
        | false -> acc
        | true ->
            let (xi, yi) = poly.[i]
            let (xj, yj) = poly.[j]
            let crosses =
                match (yi > py, yj > py) with
                | (true, false)
                | (false, true) ->
                    px < (xj - xi) * (py - yi) / ((yj - yi) + 1) + xi
                | _ -> false
            loop (i + 1) i (if crosses then not acc else acc)
    match poly with
    | [||] -> false
    | _ -> loop 0 (poly.Length - 1) false

/// Label position within coxel
let labelPosition (poly: (int * int)[]) =
    match poly with
    | [||] -> -10, -10
    | [|p|] -> p
    | _ ->
        let cx, cy = polygonCentroid poly
        let xs = poly |> Array.map fst
        let ys = poly |> Array.map snd
        let avgx = Array.sum xs / xs.Length
        let avgy = Array.sum ys / ys.Length

        // recursively shift centroid inward until it's inside
        let rec inward (x, y) step =
            match pointInPolygon (x, y) poly with
            | true -> (x, y)
            | false ->
                match step with
                | 0 -> (avgx, avgy)
                | _ ->
                    let nx = (x + avgx) / 2
                    let ny = (y + avgy) / 2
                    inward (nx, ny) (step - 1)

        inward (cx, cy) 3

///

/// <summary> Scale and Shift origin</summary>
/// <param name="scl"> Scale </param>
/// <param name="hxo"> Array of Hexels arrays </param>
/// <returns> Property array to feed into the Cluster function 
/// Hexel coordinates array * width * height </returns>
let crd 
    (scl : int) 
    (hxo : Hxl[][]) = 
    // Location to Coordinates
    let cdn 
        (hxo:Hxl[]) 
        (scl:int) =
        Array.map (fun a -> match a with 
                            |AV(x,y,z) -> ((x*scl),(y*scl),z)
                            |RV(x,y,z) -> ((x*scl),(y*scl),z)
                            |EX(x,y,z) -> ((x*scl),(y*scl),z)) hxo

    let hxXY01 = Array.map (fun x -> cdn x scl) hxo
    let (a,_,_) = (Array.concat hxXY01) |> Array.minBy(fun (x,_,_) -> x)
    let hxShfX = 0 - a + (4*scl)
    let (_,b,_) = (Array.concat hxXY01) |> Array.minBy(fun (_,x,_) -> x)
    let hxShfY = 0 - b + (4*scl)
    let (c,_,_) = (Array.concat hxXY01) |> Array.maxBy(fun (x,_,_) -> x)
    let hxMxmX = c + hxShfX + (4*scl)
    let (_,d,_) = (Array.concat hxXY01) |> Array.maxBy(fun (_,x,_) -> x)
    let hxMxmY = d + hxShfY + (4*scl)
    let hxXY02 = Array.map(fun aa -> Array.map (fun (x,y,z)-> (x + hxShfX), (y + hxShfY),z)aa) hxXY01
    (hxXY02,hxMxmX,hxMxmY)
///

/// <summary> Nested Coxels SVG </summary>
/// <param name="cxl"> Array of coxels </param>
/// <param name="clr"> Array of colors </param>
/// <returns> Polygon Vertices </returns>

let svgCoxels
    (cxl : Cxl[])
    (bdr : (int*int)[][])
    (elv : int)
    (clr : string[])
    (scl : int) = 

    // Vertices
    let sqn = cxl |> Array.map (fun x ->x.Seqn)
    let cr1 = cxl |> Array.map (fun x -> cxlPrm x elv) 
    let crd = Array.map2 (fun a b -> Geometry.removeSawtooth a b) sqn cr1
    // Log crd to console
    crd
    |> Array.iteri (fun i arr ->
        let line = arr |> Array.map (fun (x,y) -> $"({x},{y})") |> String.concat "; "
        printfn "Cxl %d: %s" i line
    )

    // Shift and Scale Vertices
    let padd = 5*scl
    let crd1 = Array.map (fun x -> Array.map(fun (a,b) -> a*scl,b*scl)x) crd
    let minX1 = fst (Array.minBy (fun (x,_) -> x) (Array.concat crd1))
    let maxX1 = fst (Array.maxBy (fun (x,_) -> x) (Array.concat crd1))
    let minY1 = snd (Array.minBy (fun (_,x) -> x) (Array.concat crd1))
    let maxY1 = snd (Array.maxBy (fun (_,x) -> x) (Array.concat crd1))
    let shfX = (-1 * minX1) + padd
    let shfY = (-1 * minY1) + padd
    let crd2 = Array.map (fun x -> Array.map(fun (a,b) -> a+shfX,b+shfY)x) crd1
    let wdt = (maxX1 - minX1)+(padd*2)+15
    let hgt = (maxY1 - minY1)+(padd*1)+0
    
    // Labels
    let lPs = Array.map(fun a -> 
                                let x,y = cxlCnt a
                                (x * scl) + shfX,(y * scl) + shfY) cxl
    let lbl = Array.map2 (fun a b -> (prpVlu a.Name),b) cxl lPs

    let svg = 
        svg {
            "viewBox" => $"0 0 {wdt} {hgt}"
            attr.``style`` (sprintf "display: block; width: 100%%; height: auto; max-width: %dpx;" wdt)

            let prp = Array.zip crd2 clr
            for (xxyy, color) in prp do
                let xy =
                    xxyy
                    |> Array.map (fun (x, y) -> [|x; y|])
                    |> Array.concat
                    |> Array.map string
                    |> String.concat ","

                plgn()
                    .pt(xy)
                    .cl(color)
                    .op("0.75")
                    .Elt()

            // Boundary Outlines
            for boundary in bdr do
            let xy =
                boundary
                |> Array.map (fun (x, y) -> [| (x * scl) + shfX; (y * scl) + shfY |])
                |> Array.concat
                |> Array.map string
                |> String.concat ","

            // Draw boundary outline in contrasting color
            plgn()
                .pt(xy)
                .st("#000000")
                .cl("none")
                .sw("2")
                .op("0.1")
                .Elt()

            let pth = Array.map (fun x -> $"path{x}") [|1..Array.length lbl|]
            let prp1 = Array.zip crd2 clr
            let prp2 = Array.zip lbl pth
            let prp = Array.map2 (fun x y -> fst x, fst y, snd x, snd y) prp1 prp2

            for i, (xxyy, label, color, path) in prp |> Array.indexed do
                let x, y =
                    match xxyy with
                    | [||] -> -10, -10
                    | _ -> snd label

                let r = 10
                crPh()
                    .pathid(path)
                    .sx($"{x}")
                    .sy($"{y + r}")
                    .r($"{r}")
                    .ex($"{x}")
                    .ey($"{y - r}")
                    .Elt()

                crTx()
                    .nm(fst label)
                    .pth(path)
                    .fw(if i = 0 then "700" else "400")
                    .fl(if i = 0 then "#333333" else "#666666")
                    .td("none")
                    .Elt()

                crCl()
                    .cx($"{x}")
                    .cy($"{y}")
                    .cl(color)
                    .Elt()
        
        }
    svg

///

/// Renders an extruded polygon on a canvas via JS WebGL
/// Simple ear-clipping triangulation for concave, non-self-intersecting polygons.
let triangulatePolygon 
    (points: (float * float)[]) : (float * float)[][] =
    if points = null || points.Length < 3 then [||]
    else
        let cross (ax, ay) (bx, by) (cx, cy) =
            (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

        let pointInTriangle (ax, ay) (bx, by) (cx, cy) (px, py) =
            let c1 = cross (ax, ay) (bx, by) (px, py)
            let c2 = cross (bx, by) (cx, cy) (px, py)
            let c3 = cross (cx, cy) (ax, ay) (px, py)
            (c1 >= 0.0 && c2 >= 0.0 && c3 >= 0.0)
            || (c1 <= 0.0 && c2 <= 0.0 && c3 <= 0.0)

        let area =
            points
            |> Array.mapi (fun i (x1, y1) ->
                let (x2, y2) = points.[(i + 1) % points.Length]
                x1 * y2 - x2 * y1)
            |> Array.sum

        let pts =
            if area < 0.0 then Array.rev points else Array.copy points

        let mutable remaining = [ for p in pts -> p ]
        let mutable triangles = []

        while remaining.Length > 3 do
            let n = remaining.Length
            let mutable earFound = false
            let mutable i = 0
            while i < n && not earFound do
                let prev = remaining.[(i + n - 1) % n]
                let curr = remaining.[i]
                let next = remaining.[(i + 1) % n]
                if cross prev curr next > 0.0 then
                    let others =
                        remaining
                        |> List.mapi (fun j p -> j, p)
                        |> List.filter (fun (j,_) -> j <> i && j <> (i + 1) % n && j <> (i + n - 1) % n)
                        |> List.map snd
                    let hasInside =
                        others |> List.exists (pointInTriangle prev curr next)
                    if not hasInside then
                        triangles <- [| prev; curr; next |] :: triangles
                        remaining <- remaining |> List.filter (fun x -> x <> curr)
                        earFound <- true
                i <- i + 1
            if not earFound then i <- n 

        if remaining.Length = 3 then
            triangles <- [| remaining.[0]; remaining.[1]; remaining.[2] |] :: triangles

        triangles |> List.rev |> List.toArray

/// Gracefully extrudes a polygon
let polygonMesh 
    (poly2D: (float * float)[]) 
    (height: float) : (float * float * float)[][] =
    // --- Default negative box polygon
    let defaultBox =
        [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |]

    // --- Safe triangulate using Option
    let safeTriangulate poly =
        match poly |> Array.length with
        | n when n < 3 -> None
        | _ ->
            match triangulatePolygon poly with
            | null | [||] -> None
            | tris -> Some tris

    // --- Base polygon selection
    let basePoly =
        match poly2D |> Array.length with
        | n when n < 3 -> defaultBox
        | _ -> poly2D

    // --- Top faces
    let topTris =
        match safeTriangulate basePoly with
        | Some tris ->
            tris |> Array.map (Array.map (fun (x, y) -> (x, y, height)))
        | None ->
            // fallback triangulate defaultBox (guaranteed valid)
            triangulatePolygon defaultBox
            |> Array.map (Array.map (fun (x, y) -> (x, y, height)))

    // --- Side walls (each edge -> 2 triangles)
    let walls =
        basePoly
        |> Array.mapi (fun i (x1, y1) ->
            let (x2, y2) = basePoly.[(i + 1) % basePoly.Length]
            let base1, base2 = (x1, y1, 0.0), (x2, y2, 0.0)
            let top1, top2 = (x1, y1, height), (x2, y2, height)
            [|
                [| base1; base2; top2 |]
                [| base1; top2; top1 |]
            |])
        |> Array.concat

    Array.concat [ topTris; walls ]

let extrudePolygons
    (js: IJSRuntime)
    (canvasId: string)
    (cxl: Cxl[])
    (colors: string[])
    (initHeight: float)
    (elv: int)
    : Async<unit> =

    // Convert Cxl to float polygon
    let toPoly (x: Cxl) =
        cxlPrm x elv
        |> Geometry.removeSawtooth x.Seqn
        |> Array.map (fun (xi, yi) -> (float xi, float yi))

    // Safe polygon + color sync
    let polygonsWithColor =
        cxl
        |> Array.map toPoly
        |> Array.zip colors
        |> Array.choose (fun (clr, poly) ->
            if poly.Length >= 3 then Some (poly, clr) else None
        )

    let polygonsFinal, colorsFinal =
        if polygonsWithColor.Length = 0 then
            [| [| (-2.0, -2.0); (-1.0, -2.0); (-1.0, -1.0); (-2.0, -1.0) |] |],
            [| "rgba(200,200,200,1)" |]
        else
            polygonsWithColor |> Array.map fst,
            polygonsWithColor |> Array.map snd

    // Safe color normalization
    let normalizeColor (rgba: string) =
        rgba.Replace("rgba(", "")
            .Replace(")", "")
            .Split(',')
        |> Array.map (fun s -> s.Trim() |> float)
        |> fun parts ->
            match parts with
            | [| r; g; b; _ |] -> [| r / 255.0; g / 255.0; b / 255.0 |]
            | [| r; g; b |] -> [| r / 255.0; g / 255.0; b / 255.0 |]
            | _ -> [| 0.8; 0.8; 0.8 |]

    async {
        do! Async.Sleep 30

        // Incremental heights
        let heights =
            polygonsFinal
            |> Array.mapi (fun i _ -> initHeight - float i * 0.01)

        // JS color array
        let colorsJs =
            colorsFinal |> Array.map normalizeColor

        // Mesh generation
        let meshes =
            polygonsFinal
            |> Array.mapi (fun i poly ->
                polygonMesh poly heights.[i]
            )

        // JS structure (triangles -> 3D coords)
        let meshesJs =
            meshes
            |> Array.map (Array.map (Array.map (fun (x, y, z) -> [| x; -y; z |])))

        // Edge polygons (same polygons used for top/bottom/vertical edges) ---
        let edgePolygonsJs =
            polygonsFinal
            |> Array.map (Array.map (fun (x, y) -> [| x; -y |]))  // match coordinate inversion in meshesJs

        // Invoke JS safely
        do!
            js.InvokeVoidAsync("initWebGLExtrudedPolygons", canvasId, meshesJs, colorsJs, heights,edgePolygonsJs).AsTask()
            |> Async.AwaitTask
    }

