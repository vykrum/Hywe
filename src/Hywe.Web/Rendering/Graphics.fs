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
