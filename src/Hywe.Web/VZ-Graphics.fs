module Graphics

open Bolero
open Bolero.Html
open Hywe.Core

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
        r="${cr}" 
        fill="${cl}" />""">

type crTx = Template<
    """<text
        id="${pth}"
        font-weight="${fw}"
        fill="${fl}"
        text-decoration="${td}"
        font-size="20px"
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

let truncateLabel (maxLen: int) (name: string) : string =
    let trimmed =
        match Option.ofObj name with
        | None -> ""
        | Some n -> n.Trim()

    match trimmed.Length with
    | len when len <= maxLen -> trimmed
    | len ->
        let prefixLen = max 1 (maxLen - 2)
        trimmed.Substring(0, min prefixLen len) + "..."

let truncateName (name: string) : string =
    truncateLabel 8 name

type hzTx = Template<
    """<text 
        x="${x}" 
        y="${y}"
        font-weight="${fw}"
        fill="${fl}"
        font-size="${fs}"
        font-family="Outfit, system-ui, sans-serif"
        text-anchor="middle"
        style="text-transform: lowercase; pointer-events: none;"
    >${nm}</text>""">

type svtx = Template<
        """<text 
        x="${xx}" 
        y="${yy}"
        width="50px"
        font-size="${fs}"
        font-family="Outfit, system-ui, sans-serif"
        text-anchor="middle"
        dominant-baseline="middle"
        fill="#808080"
        opacity="1"
        >${nm}</text> """>

let viewLegend (items: (string * string) seq) : Node =
    let uniqueItems = 
        items 
        |> Seq.filter (fun (name, _) -> not (System.String.IsNullOrWhiteSpace name))
        |> Seq.distinctBy (fun (name, _) -> name.Trim())
        |> Seq.toArray

    match uniqueItems with
    | [||] -> empty()
    | _ ->
        div {
            attr.``class`` "layout-legend"
            forEach uniqueItems <| fun (name, clr) ->
                let trimmed = name.Trim()
                div {
                    attr.``class`` "layout-legend-item"
                    attr.title trimmed
                    span {
                        attr.``class`` "layout-legend-dot"
                        attr.style $"background-color: {clr}; border-color: {clr};"
                    }
                    span {
                        attr.``class`` "layout-legend-label"
                        text trimmed
                    }
                }
        }

let renderLegendHtml (items: (string * string) seq) : string =
    let uniqueItems = 
        items 
        |> Seq.filter (fun (name, _) -> not (System.String.IsNullOrWhiteSpace name))
        |> Seq.distinctBy (fun (name, _) -> name.Trim())
        |> Seq.toArray

    match uniqueItems with
    | [||] -> ""
    | _ ->
        let renderItem (name: string, clr: string) =
            let safeName =
                name.Trim()
                    .Replace("&", "&amp;")
                    .Replace("<", "&lt;")
                    .Replace(">", "&gt;")
                    .Replace("\"", "&quot;")
            $"<div class=\"layout-legend-item\" title=\"{safeName}\"><span class=\"layout-legend-dot\" style=\"background-color: {clr}; border-color: {clr};\"></span><span class=\"layout-legend-label\">{safeName}</span></div>"

        let innerHtml =
            uniqueItems
            |> Array.map renderItem
            |> String.concat ""

        $"<div class=\"layout-legend\">{innerHtml}</div>"

let (|SvgCollinear|SvgTurning|) (p1: float * float, p2: float * float, p3: float * float) =
    let (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
    let crossProduct = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
    match crossProduct with
    | cp when abs cp < 0.0001 -> SvgCollinear
    | _ -> SvgTurning

let svgCxlPrm (cxl : Coxel.Cxl) (elv : int) =
    let rec clean points =
        match points with
        | p1 :: p2 :: p3 :: rest ->
            match (p1, p2, p3) with
            | SvgCollinear -> clean (p1 :: p3 :: rest)
            | SvgTurning   -> p1 :: clean (p2 :: p3 :: rest)
        | _ -> points

    let outside = Hexel.hxlOfs cxl.Seqn elv cxl.Hxls
    let insideSet = Hexel.hxlSet cxl.Hxls
    
    outside 
    |> Array.collect (fun hout ->
        let (ox, oy, _) = Hexel.hxlCrd hout
        Hexel.adjacent cxl.Seqn hout 
        |> Array.choose (fun n -> 
            let (ix, iy, _) = Hexel.hxlCrd n
            match insideSet.Contains(Hexel.AV(ix, iy, elv)) with
            | true -> Some ((float ox + float ix) / 2.0, (float oy + float iy) / 2.0)
            | false -> None)
    )
    |> Array.distinct
    |> Array.toList
    |> clean
    |> List.toArray

let svgRemoveSawtooth (sqn : Hexel.Sqn) (arr : (float*float)[]) : (float*float)[] =
    match arr with
    | [||] -> [||]
    | _ ->
        let (primary, secondary) = 
            match sqn with
            | Hexel.Vertical   -> (snd, fst)
            | Hexel.Horizontal -> (fst, snd)

        let splitByDelta2 (points: (float*float)[]) =
            match points with
            | [||] -> [||]
            | _ -> 
                let folder (acc: (float*float) list list) point =
                    match acc with
                    | [] -> [[point]]
                    | currentGroup :: rest ->
                        let prev = List.head currentGroup
                        match abs(primary point - primary prev) with
                        | d when abs(d - 2.0) < 0.1 -> (point :: currentGroup) :: rest
                        | _ -> [point] :: currentGroup :: rest
                
                points 
                |> Array.fold folder [] 
                |> List.map (List.rev >> Array.ofList)
                |> List.rev
                |> Array.ofList

        let oscillates (values: float[]) =
            match values.Length with
            | len when len >= 3 ->
                values
                |> Array.pairwise
                |> Array.forall (fun (a, b) -> abs (abs (b - a) - 1.0) < 0.1)
            | _ -> false

        let groups = splitByDelta2 arr
        groups
        |> Array.collect (fun g ->
            match g with
            | _ when g.Length <= 3 -> g
            | _ ->
                let secValues = g |> Array.map secondary
                match oscillates secValues with
                | true ->
                    let f, l = Array.head g, Array.last g
                    let low = min (secondary f) (secondary l)
                    match sqn with
                    | Hexel.Vertical   -> [| (low, snd f); (low, snd l) |]
                    | Hexel.Horizontal -> [| (fst f, low); (fst l, low) |]
                | false -> g
        )

let svgToCartesian (sqn: Hexel.Sqn) (x: float, y: float) =
    match sqn with
    | Hexel.Vertical -> 
        let cartX = x + (0.5 * (y % 2.0))
        let cartY = y * 1.0
        (cartX, cartY)
    | Hexel.Horizontal ->
        let cartX = x * 1.0
        let cartY = y + (0.5 * (x % 2.0))
        (cartX, cartY)

let toCartesian (sqn: Hexel.Sqn) (x: int, y: int) =
    match sqn with
    | Hexel.Vertical -> 
        let cartX = float x + (0.5 * float (y % 2))
        let cartY = float y * 1.0
        (cartX, cartY)
    | Hexel.Horizontal ->
        let cartX = float x * 1.0
        let cartY = float y + (0.5 * float (x % 2))
        (cartX, cartY)

let svgDedupeSequential (pts: (float * float)[]) =
    pts
    |> Array.fold (fun acc p -> 
        match acc with
        | [] -> [p]
        | (hx, hy) :: _ when abs (hx - fst p) < 0.0001 && abs (hy - snd p) < 0.0001 -> acc
        | _ -> p :: acc
    ) []
    |> List.rev
    |> Array.ofList

let svgEnsureClosed (pts: (float * float)[]) =
    match pts with
    | [||] | [| _ |] -> pts
    | _ ->
        let (fx, fy) = Array.head pts
        let (lx, ly) = Array.last pts
        match (abs (fx - lx), abs (fy - ly)) with
        | dx, dy when dx < 0.0001 && dy < 0.0001 -> pts
        | _ -> Array.append pts [| (fx, fy) |]

let svgRemoveCollinear (pts: (float * float)[]) =
    match pts.Length with
    | len when len < 3 -> pts
    | _ ->
        let midPoints = 
            pts
            |> Array.windowed 3
            |> Array.choose (function
                | [| p1; p2; p3 |] ->
                    match (p1, p2, p3) with
                    | SvgTurning   -> Some p2
                    | SvgCollinear -> None
                | _ -> None)
        Array.concat [| [| Array.head pts |]; midPoints; [| Array.last pts |] |]

let svgRemoveHooks (pts: (float * float)[]) =
    let isHook (p1: float * float) (p2: float * float) (p3: float * float) =
        let dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
        let d13 = dist p1 p3
        match d13 with
        | d when d < 0.1 -> true
        | _ ->
            let d12 = dist p1 p2
            let d23 = dist p2 p3
            match (d12, d23) with
            | d1, d2 when d1 > 0.001 && d2 > 0.001 ->
                let v1x, v1y = (fst p1 - fst p2) / d1, (snd p1 - snd p2) / d1
                let v2x, v2y = (fst p3 - fst p2) / d2, (snd p3 - snd p2) / d2
                let dot = v1x * v2x + v1y * v2y
                dot > 0.97
            | _ -> false

    let rec loop (current: (float * float)[]) =
        match current.Length with
        | n when n < 4 -> current
        | n ->
            let kept =
                current
                |> Array.mapi (fun i p2 ->
                    let p1 = current.[(i + n - 1) % n]
                    let p3 = current.[(i + 1) % n]
                    p2, isHook p1 p2 p3
                )
                |> Array.choose (fun (p, hooked) ->
                    match hooked with
                    | true -> None
                    | false -> Some p
                )
            match kept.Length < n && kept.Length >= 3 with
            | true -> loop kept
            | false -> current

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
        let nextPoly = Array.append (Array.tail poly) [| Array.head poly |]
        let (sx, sy, a) =
            Array.zip poly nextPoly
            |> Array.fold (fun (accSx, accSy, accA) ((x1, y1), (x2, y2)) ->
                let cross = x1 * y2 - x2 * y1
                (accSx + (x1 + x2) * cross,
                 accSy + (y1 + y2) * cross,
                 accA + cross)
            ) (0.0, 0.0, 0.0)
        match a / 2.0 with
        | area when abs area < 0.0001 -> Array.head poly
        | area -> (sx / (6.0 * area), sy / (6.0 * area))
