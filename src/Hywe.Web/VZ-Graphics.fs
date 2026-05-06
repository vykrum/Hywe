module Graphics

open Bolero
open System.Collections.Frozen
open Hywe.Core

let vsSource = """
    attribute vec3 a_position;
    attribute vec4 a_color;
    uniform mat4 u_projection;
    uniform mat4 u_view;
    varying vec4 v_color;
    void main() {
        gl_Position = u_projection * u_view * vec4(a_position, 1.0);
        v_color = a_color;
    }"""

let fsSource = """
    precision mediump float;
    varying vec4 v_color;
    void main() {
        gl_FragColor = v_color;
    }"""

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
        stroke-width = "1"
        opacity = "0.5"
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
        font-family="Outfit, system-ui, sans-serif"
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
        font-family="Outfit, system-ui, sans-serif"
        text-anchor="middle"
        dominant-baseline="middle"
        fill = "#808080"
        opacity = "1"
        >${nm}</text> """>

let (|SvgCollinear|SvgTurning|) (p1: float * float, p2: float * float, p3: float * float) =
    let (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
    let crossProduct = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
    if abs crossProduct < 0.0001 then SvgCollinear else SvgTurning

let svgCxlPrm (cxl : Coxel.Cxl) (elv : int) =
    let rec clean points =
        match points with
        | p1 :: p2 :: p3 :: rest ->
            match (p1, p2, p3) with
            | SvgCollinear -> clean (p1 :: p3 :: rest)
            | SvgTurning   -> p1 :: clean (p2 :: p3 :: rest)
        | _ -> points

    let outside = Hexel.hxlOfs cxl.Seqn elv cxl.Hxls
    let insideSet = (Hexel.hxlSet cxl.Hxls).ToFrozenSet()
    
    outside 
    |> Array.collect (fun hout ->
        let (ox, oy, _) = Hexel.hxlCrd hout
        Hexel.adjacent cxl.Seqn hout 
        |> Array.choose (fun n -> 
            let (ix, iy, _) = Hexel.hxlCrd n
            if insideSet.Contains(Hexel.AV(ix, iy, elv)) then
                Some ( (float ox + float ix) / 2.0, (float oy + float iy) / 2.0 )
            else None)
    )
    |> Array.distinct
    |> Array.toList
    |> clean
    |> List.toArray

let svgRemoveSawtooth (sqn : Hexel.Sqn) (arr : (float*float)[]) : (float*float)[] =
    if arr.Length = 0 then [||] else
    let (primary, secondary) = 
        match sqn with
        | Hexel.Vertical   -> (snd, fst)
        | Hexel.Horizontal -> (fst, snd)

    let result = ResizeArray<float*float>()
    let mutable i = 0
    let n = arr.Length
    while i < n do
        let mutable j = i
        while j + 1 < n && abs(primary arr.[j+1] - primary arr.[j]) < 2.1 && abs(primary arr.[j+1] - primary arr.[j]) > 1.9 do
            j <- j + 1
            
        let groupLen = j - i + 1
        if groupLen > 3 then
            let mutable isOscillating = true
            for k = i to j - 1 do
                if abs(secondary arr.[k+1] - secondary arr.[k]) < 0.9 || abs(secondary arr.[k+1] - secondary arr.[k]) > 1.1 then
                    isOscillating <- false
            
            if isOscillating then
                let f = arr.[i]
                let l = arr.[j]
                let low = min (secondary f) (secondary l)
                match sqn with
                | Hexel.Vertical   -> 
                    result.Add((low, snd f))
                    result.Add((low, snd l))
                | Hexel.Horizontal -> 
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

let svgToCartesian (sqn: Hexel.Sqn) (x: float, y: float) =
    match sqn with
    | Hexel.Vertical -> 
        let cartX = x + (0.5 * (y % 2.0))
        let cartY = y * 0.866
        (cartX, cartY)
    | Hexel.Horizontal ->
        let cartX = x * 0.866
        let cartY = y + (0.5 * (x % 2.0))
        (cartX, cartY)

let svgDedupeSequential (pts: (float * float)[]) =
    pts |> Array.fold (fun acc p -> 
        match acc with
        | [] -> [p]
        | head :: _ -> 
            let (hx, hy) = head
            let (px, py) = p
            if abs(hx - px) < 0.0001 && abs(hy - py) < 0.0001 then acc else p :: acc
    ) [] |> List.rev |> Array.ofList

let svgEnsureClosed (pts: (float * float)[]) =
    match pts.Length < 2 with
    | true -> pts
    | false ->
        let (fx, fy) = pts.[0]
        let (lx, ly) = pts.[pts.Length - 1]
        if abs(fx - lx) < 0.0001 && abs(fy - ly) < 0.0001 then pts
        else Array.append pts [| pts.[0] |]

let svgRemoveCollinear (pts: (float * float)[]) =
    match pts.Length < 3 with
    | true -> pts
    | false ->
        let midPoints = 
            pts |> Array.windowed 3 |> Array.choose (fun win ->
                let (x1, y1), (x2, y2), (x3, y3) = win.[0], win.[1], win.[2]
                let cross = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
                match abs cross > 0.0001 with true -> Some (x2, y2) | false -> None)
        Array.concat [| [|pts.[0]|]; midPoints; [|pts.[pts.Length-1]|] |]

let svgRemoveHooks (pts: (float * float)[]) =
    if pts.Length < 4 then pts
    else
        let dist (x1, y1) (x2, y2) = sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))
        let rec loop (current: (float * float)[]) =
            let n = current.Length
            if n < 4 then current
            else
                let res = ResizeArray<float * float>()
                let mutable hookFound = false
                
                for i = 0 to n - 1 do
                    let p1 = current.[(i + n - 1) % n]
                    let p2 = current.[i]
                    let p3 = current.[(i + 1) % n]
                    
                    let d12 = dist p1 p2
                    let d23 = dist p2 p3
                    let d13 = dist p1 p3
                    
                    if d13 < 0.1 then
                        hookFound <- true
                    else
                        if d12 > 0.001 && d23 > 0.001 then
                            let v1x, v1y = (fst p1 - fst p2)/d12, (snd p1 - snd p2)/d12
                            let v2x, v2y = (fst p3 - fst p2)/d23, (snd p3 - snd p2)/d23
                            let dot = v1x * v2x + v1y * v2y
                            if dot > 0.95 then
                                hookFound <- true
                            else
                                res.Add(p2)
                        else
                            res.Add(p2)
                            
                if hookFound && res.Count >= 3 then loop (res.ToArray())
                else current
        loop pts

let svgCleanPolygon (sqn: Hexel.Sqn) (pts: (float * float)[]) =
    pts
    |> svgDedupeSequential
    |> svgRemoveHooks
    |> svgRemoveSawtooth sqn
    |> svgDedupeSequential
    |> svgRemoveHooks
    |> svgEnsureClosed
    |> svgRemoveCollinear

let polygonCentroid (poly: (float * float)[]) =
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
        let area = a / 2.0
        if abs area < 0.0001 then poly.[0]
        else (sx / (6.0 * area), sy / (6.0 * area))
