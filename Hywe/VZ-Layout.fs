module Layout

open Bolero
open Bolero.Html
open Hexel
open Coxel
open Microsoft.JSInterop
open System
open System.Text
open System.Text.Json
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

/// Polygon centroid
let polygonCentroid 
    (poly: (float * float)[]) =
    match poly with
    | [||] -> 0.0, 0.0
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
            ) (0.0, 0.0, 0.0)

        if Math.Abs(a) < 0.001 then 
            let avgX = poly |> Array.averageBy fst
            let avgY = poly |> Array.averageBy snd
            (avgX, avgY)
        else 
            (sx / (3.0 * a), sy / (3.0 * a))

/// Point-in-polygon
let pointInPolygon 
    (px: float, py: float) 
    (poly: (float * float)[]) =
    let rec check i j acc =
        match i < poly.Length with
        | false -> acc
        | true ->
            let (xi, yi) = poly.[i]
            let (xj, yj) = poly.[j]
            
            let isIntersecting =
                match (yi > py, yj > py) with
                | (true, false) | (false, true) ->
                    let dy = yj - yi
                    if Math.Abs(dy) < 0.0001 then false
                    else 
                        px < (xj - xi) * (py - yi) / dy + xi
                | _ -> false
            check (i + 1) i (if isIntersecting then not acc else acc)
    
    match poly with
    | [||] -> false
    | _ -> check 0 (poly.Length - 1) false

/// Label position within coxel
let labelPosition 
    (poly: (float * float)[]) =
    match poly with
    | [||] -> 0.0, 0.0
    | [|p|] -> p
    | _ ->
        let cx, cy = polygonCentroid poly
        let avgx = poly |> Array.averageBy fst
        let avgy = poly |> Array.averageBy snd

        let rec inward (x, y) step =
            if pointInPolygon (x, y) poly then (x, y)
            else
                match step with
                | 0 -> (avgx, avgy)
                | _ -> inward ((x + avgx) / 2.0, (y + avgy) / 2.0) (step - 1)

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
/// <param name="bdr"> Boundary polygons </param>
/// <param name="elv"> Elevation level </param>
/// <param name="clr"> Array of colors </param>
/// <param name="scl"> Scale factor </param>
/// <param name="svgId"> Optional id to assign to the SVG element </param>
/// <returns> Polygon Vertices </returns>

let svgCoxels
    (cxl : Cxl[])
    (bdr : (float*float)[][])
    (elv : int)
    (clr : string[])
    (scl : int)
    (svgId : string option) = 

    // Vertices
    let sqn = cxl |> Array.map (fun x ->x.Seqn)
    let cr1 = cxl |> Array.map (fun x -> cxlPrm x elv) 
    let crd = Array.map2 (fun a b -> Geometry.removeSawtooth a b) sqn cr1

    // Shift and Scale Vertices
    let padd = float (5*scl)
    let crd1 = Array.map (fun x -> Array.map(fun (a,b) -> a * float scl, b * float scl)x) crd
    // --- Coordinate Transformation & Offsetting ---
    // Calculate global shifts to normalize the layout within the SVG viewport
    let minX1 = fst (Array.minBy fst (Array.concat crd1))
    let maxX1 = fst (Array.maxBy fst (Array.concat crd1))
    let minY1 = snd (Array.minBy snd (Array.concat crd1))
    let maxY1 = snd (Array.maxBy snd (Array.concat crd1))
    
    // Shift points into the positive coordinate space with padding
    let shfX = (-1.0 * minX1) + padd
    let shfY = (-1.0 * minY1) + padd
    
    // Transform all vertices (already calculated as midpoints in cxlPrm)
    let crd2 = Array.map (fun x -> Array.map(fun (a,b) -> a+shfX,b+shfY)x) crd1
    
    let wdt = int ((maxX1 - minX1)+(padd*2.0)+15.0)
    let hgt = int ((maxY1 - minY1)+(padd*1.0)+0.0)
    
    // Labels
    let lPs = Array.map(fun a -> 
                                let x,y = cxlCnt a
                                (float x * float scl) + shfX,(float y * float scl) + shfY) cxl
    let lbl = Array.map2 (fun a b -> (prpVlu a.Name),b) cxl lPs

    let svg = 
        svg {
            "viewBox" => $"0 0 {wdt} {hgt}"
            attr.``style`` (sprintf "display: block; width: 100%%; height: auto; max-width: %dpx;" wdt)
            attr.id (svgId |> Option.defaultValue "")

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
                    |> Array.map (fun (x, y) -> [| (float x * float scl) + shfX; (float y * float scl) + shfY |])
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
                    | [||] -> -10.0, -10.0
                    | _ -> snd label

                let r = 10.0
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

// Batch Export Types
type BatchComponent = {| color: string; points: float[]; name: string; lx: float; ly: float |}
type BatchConfgrtns = {| sqnName: string; shapes: BatchComponent[]; w: float; h: float |}

/// Extracts high-fidelity coordinates for Geometry generation
let getBtchCrds (cxl: Cxl[]) =
    // Estimate capacity: 10 chars per int * 3 ints per hex * total hexes
    let totalHexes = cxl |> Array.sumBy (fun x -> x.Hxls.Length)
    let sb = StringBuilder(totalHexes * 30) 

    cxl |> Array.iteri (fun i item ->
        // Add comma separator for all but the first row
        match i with
        | 0 -> ()
        | _ -> sb.Append(" , ") |> ignore
        
        item.Hxls |> Array.iteri (fun j hxl ->
            let (a, b, c) = hxlCrd hxl
            // Add space for all but the first tuple in a row
            match j with
            | 0 -> ()
            | _ -> sb.Append(" ") |> ignore
            
            sb.Append(a).Append(" ")
              .Append(b).Append(" ")
              .Append(c) |> ignore
        )
    )
    sb.ToString()

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
                    
                    let lx, ly = labelPosition pts
                    
                    {|
                        points = pts |> Array.collect (fun (px, py) -> [| px - minX; py - minY |])
                        name = prpVlu label.Name
                        color = colors.[i]
                        lx = lx - minX
                        ly = ly - minY
                    |}
                with _ ->
                    {| points = [||]; name = ""; color = "rgba(0,0,0,0)"; lx = 0.0; ly = 0.0 |}
            )

        {| shapes = shapes; w = currentWidth; h = currentHeight |}

let getDatasetWithLabels (configs: BatchConfgrtns[]) : (string * float[])[][] =
    configs 
    |> Array.map (fun config ->
        config.shapes 
        |> Array.map (fun shape ->
            // Returns a tuple: ("Room Name", [| x1; y1; x2; y2... |])
            (shape.name, shape.points)
        )
    )

let alternateConfigurations 
    (configs: BatchConfgrtns[]) 
    (selectedIndex: int option) 
    (onTap: int -> 'msg)
    (dispatch: 'msg -> unit)
    (onClose: unit -> unit)
    (js: IJSRuntime): Node =
    
    // 1. GRID CONSTANTS
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

    // 3. LEGEND MATH
    let uniqueShapes = 
        match configs.Length with
        | 0 -> [||]
        | _ -> configs.[0].shapes |> Array.distinctBy (fun s -> s.name)

    let legendItemsPerRow = 8 
    let legendItemHeight = 25.0
    let legendRows = ceil (float uniqueShapes.Length / float legendItemsPerRow)
    let legendTotalHeight = (max 1.0 legendRows) * legendItemHeight

    // 4. HEADER & TOTAL CANVAS MATH
    let headerHeight = 60.0 
    let totalWidth = (float cols * cellW)
    let totalHeight = (float rows * cellH) + headerHeight + legendTotalHeight + 40.0
    let borderColor = "#444"

    div {
        attr.id "pdf-export-container"
        attr.style "background: #ffffff; padding: 0px 40px; width: 100%; display: flex; flex-direction: column; align-items: center;"
        
        svg {
            attr.id "variation-svg-output"
            "viewBox" => $"{ -svgPadding } { -headerHeight } { totalWidth + (svgPadding * 2.0) } { totalHeight }"
            attr.style "display: block; width: 100%; height: auto; background: #ffffff;"
            
            // --- BACKGROUND CATCHER (Click Outside) ---
            elt "rect" {
                "x" => -svgPadding; "y" => -headerHeight
                "width" => totalWidth + (svgPadding * 2.0); "height" => totalHeight
                "fill" => "white"; "fill-opacity" => "0.01"
                on.click (fun _ -> dispatch (onTap -1))
            }

            // --- HEADER (Fixed Interpolation) ---
            let labelPhrase = "alternATE◦CONFIGURATions"
            let restrictedWidth = totalWidth * 0.7 
            let horizontalOffset = (totalWidth - restrictedWidth) / 2.0
            let charSpacing = restrictedWidth / float (labelPhrase.Length + 1)

            for i in 0 .. labelPhrase.Length - 1 do
                elt "text" {
                    "x" => (horizontalOffset + (charSpacing * float (i + 1)))
                    "y" => (-headerHeight / 1.5)
    
                    // Correct way to pass the string as an Attr
                    attr.style $"font-family: sans-serif; font-size: 22px; fill: {borderColor}; text-anchor: middle; font-weight: normal; dominant-baseline: hanging;"
    
                    text (labelPhrase.[i].ToString())
                }

            // --- THE GRID ---
            for i in 0 .. (configs.Length - 1) do
                let cfg = configs.[i]
                let col, row = i % cols, i / cols
                let ox = (float col * cellW) + (cellW / 2.0) - (maxW * scale / 2.0)
                let oy = (float row * cellH) + (cellH / 2.0) - (maxH * scale / 2.0)
                let isSelected = selectedIndex = Some i

                elt "g" {
                    for j, s in cfg.shapes |> Array.indexed do
                        // Filter: Only show if polygon has points
                        if not (Array.isEmpty s.points) then
                            let xy = s.points 
                                     |> Array.chunkBySize 2 
                                     |> Array.map (fun p -> $"{ox + p.[0] * scale},{oy + p.[1] * scale}") 
                                     |> String.concat " "
            
                            elt "g" { 
                                on.click (fun _ -> dispatch (onTap i))
                                "onclick:stopPropagation" => true
                    
                                attr.style "cursor: pointer;"
                                plgn().pt(xy).cl(s.color).op("0.8").Elt() 

                                // Temporary Labels
                                if isSelected then
                                    let tx = ox + (s.lx * scale)
                                    let ty = oy + (s.ly * scale)
                                    let pathId = $"batch_path_{i}_{j}"
                                    let r = 10.0
                                    
                                    elt "path" {
                                        "id" => pathId
                                        "fill" => "none"
                                        "d" => $"M {tx},{ty + r} A {r},{r} 0 1,1 {tx},{ty - r} A {r},{r} 0 1,1 {tx},{ty + r}"
                                    }
                                    
                                    elt "text" {
                                        "font-weight" => if j = 0 then "700" else "400"
                                        "fill" => if j = 0 then "#333333" else "#666666"
                                        "font-size" => "10px"
                                        "font-family" => "Verdana"
                                        "text-anchor" => "middle"
                                        attr.style "text-transform: lowercase; pointer-events: none;"
                                        elt "textPath" {
                                            "href" => $"#{pathId}"
                                            "startOffset" => "50%"
                                            "letter-spacing" => "0.5px"
                                            text s.name
                                        }
                                    }
                                    
                                    elt "circle" {
                                        "cx" => tx
                                        "cy" => ty
                                        "r" => 5.0
                                        "fill" => s.color
                                    }
                            }

                    // Permanent label below
                    let labelX = ox + (maxW * scale / 2.0)
                    let labelY = oy + (maxH * scale) + 12.0
                    let letter = if i < labelPhrase.Length then string labelPhrase.[i] else ""
                    svtx().xx(string labelX).yy(string labelY).nm($"{letter} [{cfg.sqnName}]").Elt()
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
                    elt "text" { 
                        "x" => 20.0 
                        // Add explicit font-family here
                        attr.style "font-family: Arial, Helvetica, sans-serif; font-size: 12px; fill: #333;"
                        text s.name 
                    }
                }
        }
        // --- DOWNLOAD BUTTON ---
        button {
            attr.``class`` "layout-download-btn"
            on.click (fun _ -> 
                let datePart = System.DateTime.Now.ToString("yyMMddmm")
                let fileName = "HywVariations_" + datePart + ".svg"
        
                // Call our new SVG downloader
                js.InvokeVoidAsync("downloadSvgFile", "variation-svg-output", fileName).AsTask() 
                |> ignore
            )
            text "Download SVG"
        }
    }

let serializeConfiguration 
    (batchOption: BatchConfgrtns[] option) : string =
    batchOption
    |> Option.defaultValue [||]
    |> Array.map (fun config ->
        config.sqnName, 
        config.shapes |> Array.map (fun s -> 
            // Rounding points to 2 decimal places to save string space
            let roundedPoints = s.points |> Array.map (fun p -> System.Math.Round(p, 2))
            (s.name, roundedPoints)
        )
    )
    |> readOnlyDict
    |> JsonSerializer.Serialize