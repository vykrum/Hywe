module Bridge

open Bolero
open Bolero.Html
open Hexel
open Coxel
open Microsoft.JSInterop
open System
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
let polygonCentroid 
    (poly: (int * int)[]) =
    match poly with
    | [||] -> 0, 0
    | [| p |] -> p
    | _ ->
        let n = poly.Length
        let (sx, sy, a) =
            [| 0 .. n - 1 |]
            |> Array.fold (fun (accSx, accSy, accA) i ->
                let (x1, y1) = poly.[i]
                let (x2, y2) = poly.[(i + 1) % n]
                let cross = x1 * y2 - x2 * y1
                (accSx + (x1 + x2) * cross,
                 accSy + (y1 + y2) * cross,
                 accA + cross)
            ) (0, 0, 0)

        // Safety check: a is 2 * Area. If Area < 1 unit, use simple average.
        // This prevents the sx / (3 * a) crash.
        if Math.Abs(a) < 1 then 
            let avgX = poly |> Array.averageBy (fun (x, _) -> float x) |> int
            let avgY = poly |> Array.averageBy (fun (_, y) -> float y) |> int
            (avgX, avgY)
        else 
            (sx / (3 * a), sy / (3 * a))

/// Integer point-in-polygon
let pointInPolygon 
    (px, py) 
    (poly: (int * int)[]) =
    let rec check i j acc =
        match i < poly.Length with
        | false -> acc
        | true ->
            let (xi, yi) = poly.[i]
            let (xj, yj) = poly.[j]
            
            let isIntersecting =
                // Check if the point's Y is between the edge's Ys
                match (yi > py, yj > py) with
                | (true, false) | (false, true) ->
                    let dy = yj - yi
                    // Denominator check: if dy is 0, it's a horizontal line. Skip.
                    if dy = 0 then false
                    else 
                        // Standard ray-casting formula
                        px < (xj - xi) * (py - yi) / dy + xi
                | _ -> false
            check (i + 1) i (if isIntersecting then not acc else acc)
    
    match poly with
    | [||] -> false
    | _ -> check 0 (poly.Length - 1) false

/// Label position within coxel
let labelPosition 
    (poly: (int * int)[]) =
    match poly with
    | [||] -> 0, 0
    | [|p|] -> p
    | _ ->
        let cx, cy = polygonCentroid poly
        let xs = poly |> Array.map fst
        let ys = poly |> Array.map snd
        
        // Prevent division by zero if poly.Length is somehow 0
        let len = Math.Max(1, poly.Length)
        let avgx = Array.sum xs / len
        let avgy = Array.sum ys / len

        let rec inward (x, y) step =
            if pointInPolygon (x, y) poly then (x, y)
            else
                match step with
                | 0 -> (avgx, avgy)
                | _ -> inward ((x + avgx) / 2, (y + avgy) / 2) (step - 1)

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

type PreviewShape = {| color: string; points: float[]; name: string; lx: float; ly: float |}
type PreviewConfig = {| sqnName: string; shapes: PreviewShape[]; w: float; h: float |}

/// Extracts high-fidelity coordinates for Geometry generation
let getStaticGeometry (cxl: Cxl[]) (colors: string[]) (elv: int) (scl: int) =
    let sqn = cxl |> Array.map (fun x -> x.Seqn)
    let cr1 = cxl |> Array.map (fun x -> cxlPrm x elv) 
    let coords = Array.map2 (fun a b -> Geometry.removeSawtooth a b) sqn cr1
    
    // Safety check for empty geometry to prevent crash in Array.minBy
    let flattened = Array.concat coords
    match flattened.Length with
    | 0 -> 
        {| shapes = [||]; w = 1.0; h = 1.0 |}
    | _ ->
        let minX = fst (Array.minBy fst flattened)
        let maxX = fst (Array.maxBy fst flattened)
        let minY = snd (Array.minBy snd flattened)
        let maxY = snd (Array.maxBy snd flattened)
        
        let currentWidth = float (maxX - minX)
        let currentHeight = float (maxY - minY)
        
        // Prevent index errors by taking the minimum length of all input arrays
        let len = 
            [| coords.Length; cxl.Length; colors.Length |] 
            |> Array.min
        
        let shapes = 
            [| 0 .. len - 1 |] 
            |> Array.map (fun i ->
                try
                    let pts = coords.[i]
                    let label = cxl.[i]
                    
                    // We still calculate lx/ly so the Legend can potentially 
                    // use them or for centering logic
                    let lx, ly = labelPosition pts
                    
                    {|
                        points = pts |> Array.collect (fun (px, py) -> [| float (px - minX); float (py - minY) |])
                        name = prpVlu label.Name
                        color = colors.[i]
                        lx = float (lx - minX)
                        ly = float (ly - minY)
                    |}
                with _ ->
                    // Fallback for a single broken shape within the variation
                    {| points = [||]; name = ""; color = "rgba(0,0,0,0)"; lx = 0.0; ly = 0.0 |}
            )

        {| shapes = shapes; w = currentWidth; h = currentHeight |}

let alternateConfigurations (configs: PreviewConfig[]) (onClose: unit -> unit) (js: IJSRuntime) : Node =
    // 1. GRID CONSTANTS (Define these first!)
    let totalItems = configs.Length
    let cols = 4 
    let rows = (totalItems + cols - 1) / cols
    let cellW, cellH = 200.0, 200.0 
    let svgPadding = 20.0 
    
    // 2. SCALE MATH
    let getMax getter =
        match Array.isEmpty configs with
        | true -> 1.0
        | false -> 
            let m = configs |> Array.map getter |> Array.max
            match m <= 0.0 with | true -> 1.0 | false -> m

    let maxW = getMax (fun c -> c.w)
    let maxH = getMax (fun c -> c.h)
    let scale = Math.Min((cellW * 0.85) / maxW, (cellH * 0.85) / maxH)

    // 3. LEGEND MATH (Needs uniqueShapes from configs)
    let uniqueShapes = 
        match configs.Length with
        | 0 -> [||]
        | _ -> configs.[0].shapes |> Array.distinctBy (fun s -> s.name)

    let legendItemsPerRow = 8 
    let legendItemHeight = 25.0
    let legendRows = ceil (float uniqueShapes.Length / float legendItemsPerRow)
    let legendTotalHeight = (max 1.0 legendRows) * legendItemHeight


    // 4. HEADER & TOTAL CANVAS MATH
    let headerHeight = 120.0 
    let totalWidth = (float cols * cellW)
    let totalHeight = (float rows * cellH) + headerHeight + legendTotalHeight + 40.0

    div {
        attr.id "pdf-export-container"
        attr.style "background: #ffffff; padding: 40px; width: 100%; display: flex; flex-direction: column; align-items: center;"
        
        svg {
            attr.id "variation-svg-output"
            "viewBox" => $"{ -svgPadding } { -headerHeight } { totalWidth + (svgPadding * 2.0) } { totalHeight }"
            attr.style "display: block; width: 100%; height: auto; background: #ffffff;"
            
            // --- PDF-ONLY BORDER ---
            let borderInset = 10.0
            let borderColor = "#444"
            elt "rect" {
                attr.id "pdf-border"
                // Move starting point inwards
                "x" => (-svgPadding + borderInset)
                "y" => (-headerHeight + borderInset)
                // Reduce total size to maintain the inset on all sides
                "width" => (totalWidth + (svgPadding * 2.0) - (borderInset * 2.0))
                "height" => (totalHeight - (borderInset * 2.0))
                "fill" => "none"
                "stroke" => borderColor
                "stroke-width" => "1.5"
                attr.style "visibility: hidden;"
            }

            // --- PDF-ONLY LOGO (Header Position) ---
            elt "g" {
                attr.id "pdf-logo"
                attr.style "visibility: hidden;" // Hidden on the web UI

                // lx: Pushes to the right side
                let lx = totalWidth - 40.0 - borderInset
                // ly: Moves into the negative header space
                // We start at the very top (-headerHeight) and add the inset + a small margin
                let ly = -headerHeight + borderInset + 5.0 
                "transform" => $"translate({lx}, {ly}) scale(0.04)"

                elt "path" {
                    "fill" => borderColor
                    "d" => "M 167 836 Q 167 850 179 857 L 279 915 Q 317 937 317 893 L 317 600 Q 317 575 342 575 L 500 575 Q 525 575 525 600 L 525 738 Q 525 788 575 788 L 748 788 Q 841 788 760 834 L 488 992 Q 450 1013 488 1035 L 588 1093 Q 600 1100 613 1093 L 1021 857 Q 1033 850 1033 836 L 1033 364 Q 1033 350 1021 343 L 921 285 Q 883 263 883 307 L 883 613 Q 883 638 858 638 L 700 638 Q 675 638 675 613 L 675 450 Q 675 425 650 425 L 430 425 Q 337 425 418 378 L 713 208 Q 750 187 713 165 L 613 104 Q 600 100 588 104 L 179 343 Q 167 350 167 364 L 167 836"
                }
            }

            // --- HEADER ---
            let labelPhrase = "alternATE◦CONFIGURATions"
            let restrictedWidth = totalWidth * 0.7 
            let horizontalOffset = (totalWidth - restrictedWidth) / 2.0
            let charSpacing = restrictedWidth / float (labelPhrase.Length + 1)

            for i in 0 .. labelPhrase.Length - 1 do
                elt "text" {
                    // We add the offset to the x position
                    "x" => (horizontalOffset + (charSpacing * float (i + 1)))
                    "y" => (-headerHeight / 1.5)
                    attr.style $"font-family: sans-serif; font-size: 22px; fill: {borderColor}; text-anchor: middle; font-weight: normal;"
                    text (labelPhrase.[i].ToString())
                }

            // --- THE GRID ---
            for i in 0 .. (configs.Length - 1) do
                let cfg = configs.[i]
                let col, row = i % cols, i / cols
                let ox = (float col * cellW) + (cellW / 2.0) - (maxW * scale / 2.0)
                let oy = (float row * cellH) + (cellH / 2.0) - (maxH * scale / 2.0)
            
                elt "g" {
                    for s in cfg.shapes do
                        let xy = s.points |> Array.chunkBySize 2 |> Array.map (fun p -> $"{ox + p.[0] * scale},{oy + p.[1] * scale}") |> String.concat " "
                        elt "g" { plgn().pt(xy).cl(s.color).op("0.8").Elt() }
                    
                    svtx().xx(string (ox + (maxW * scale / 2.0))).yy(string (oy + (maxH * scale / 2.0) + (maxH * scale / 2.0) + 12.0)).nm(cfg.sqnName).Elt()
                }

            // --- LEGEND ---
            let legendStartY = (float rows * cellH) + 40.0
            for i in 0 .. uniqueShapes.Length - 1 do
                let s = uniqueShapes.[i]
                let currR = floor (float i / float legendItemsPerRow)
                let currC = float (i % legendItemsPerRow)
                let lx = currC * (totalWidth / float legendItemsPerRow)
                let ly = legendStartY + (currR * legendItemHeight)
                
                elt "g" {
                    "transform" => $"translate({lx+30.0}, {ly})"
                    elt "rect" { "y" => -12.0; "width" => 14; "height" => 14; "fill" => s.color }
                    elt "text" { "x" => 20.0; text s.name }
                }
        }

        // Download Button
        div {
            attr.style "margin-top: 30px;"
            button {
                attr.``class`` "hywe-toggle-btn"
                on.click (fun _ -> 
                    async {
                        let now = System.DateTime.Now
                        let datePart = now.ToString("yyMMddmm")
                        let fileName = $"Hyw{datePart}.pdf"

                        // TARGET THE SVG ID: "variation-svg-output"
                        do! js.InvokeVoidAsync("alternateConfigurationsPdf", "variation-svg-output", fileName).AsTask() 
                            |> Async.AwaitTask
                    } |> Async.StartImmediate
                )
                text "Download PDF"
            }
        }
    }

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
        |> Geometry.removeSawtooth x.Seqn
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

