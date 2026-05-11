module CodeNode

open System
open Bolero
open Bolero.Html
open Hywe.Core.Lexel
open NodeCode

// --------------------
// Syntax Preprocessing
// --------------------

/// Standardizes Hywe syntax, ensuring markers, attributes and correctly formatted paths
let preprocessCode (code: string) : string =
    if String.IsNullOrWhiteSpace code then "" else
    
    let levels = processFullString code
    if levels.IsEmpty then 
        // Fallback for raw node list without parentheses
        if code.Contains "/" then $"L0(Q=VRCCNE/L=0/X=1)({code.Trim()})"
        else code
    else
        let processedLevels = 
            levels |> List.mapi (fun i levelData ->
                let marker = 
                    if String.IsNullOrWhiteSpace levelData.Marker then sprintf "L%d" i 
                    else levelData.Marker
                
                let attrs = levelData.Attributes
                let attrStr = 
                    let w = match attrs.Width with Some v -> $"/W={v}" | None -> ""
                    let h = match attrs.Height with Some v -> $"/H={v}" | None -> ""
                    $"Q={attrs.Sequence}/L={attrs.Level}/X={attrs.Scale}/E={attrs.Entry}/O={attrs.OuterBoundary}/I={attrs.Islands}/T={attrs.Thickness}{w}{h}"
                
                let nodes = 
                    levelData.Tree 
                    |> List.collect id 
                    |> List.map (fun n -> 
                        let extr = match n.Extrusion with Some v -> $"/{v}" | None -> ""
                        $"({n.Id}/{n.Area}/{n.Label}{extr})")
                    |> String.concat ""

                $"{marker}({attrStr}){nodes}"
            )

        String.Join("", processedLevels)

// --------------------
// Tree Parsing
// --------------------

/// Parses the standardized syntax into hierarchical tree structures for visualization
let parseOutput (code: string) : TreeNode list =
    let levels = processFullString code
    
    levels |> List.mapi (fun lvlIdx levelData ->
        let nodes = levelData.Tree |> List.collect id
        
        // Find the root (path "1") or default to the first node
        let rootNode = 
            nodes |> List.tryFind (fun n -> n.Id = "1")
            |> Option.orElse (nodes |> List.tryHead)

        match rootNode with
        | None ->
            { Id = Guid.NewGuid(); Name = sprintf "Level %d" lvlIdx; Weight = "0"; X = 0.0; Y = 0.0; Children = []; Level = lvlIdx; Extrusion = 3.0; Base = None }
        | Some root ->
            let rec build (n: LexelNode) =
                let children = 
                    nodes 
                    |> List.filter (fun c -> c.Id.StartsWith(n.Id + ".") && not (c.Id.Substring(n.Id.Length + 1).Contains(".")))
                    |> List.map build
                { 
                    Id = Guid.NewGuid()
                    Name = n.Label
                    Weight = string n.Area
                    X = 0.0
                    Y = 0.0
                    Children = children
                    Level = lvlIdx
                    Extrusion = n.Extrusion |> Option.defaultValue 3.0
                    Base = n.Base
                }
            build root
    )

// --------------------
// SVG Visualization
// --------------------

type VisualElement =
    | VLine of x1: float * y1: float * x2: float * y2: float * st: string
    | VPolygon of pts: string * fl: string * st: string * sw: string
    | VText of x: float * y: float * sz: int * ta: string * fl: string * nm: string

let private flattenTree (node: TreeNode) : TreeNode list =
    let rec loop n = n :: (n.Children |> List.collect loop)
    loop node

let rec private layoutTreeViz (node: TreeNode) (depth: int) (xRef: float ref) : TreeNode =
    let horizontalSpacing = 70.0
    let verticalSpacing = 55.0 // 40 (height) + 15 (gap)

    let laidOutChildren = node.Children |> List.map (fun child -> layoutTreeViz child (depth + 1) xRef)

    let x =
        if laidOutChildren.IsEmpty then
            let x = xRef.Value
            xRef.Value <- x + horizontalSpacing
            x
        else laidOutChildren |> List.averageBy (fun n -> n.X)

    { node with X = x; Y = float depth * verticalSpacing; Children = laidOutChildren }

let private calculateTreeBoundsWithNodes (root: TreeNode) =
    let root = layoutTreeViz root 0 (ref 100.0)
    let nodes = flattenTree root
    let margin = 50.0
    let minX = if nodes.IsEmpty then 0.0 else nodes |> List.map (fun n -> n.X) |> List.min
    let maxX = if nodes.IsEmpty then 100.0 else nodes |> List.map (fun n -> n.X) |> List.max
    let minY = if nodes.IsEmpty then 0.0 else nodes |> List.map (fun n -> n.Y) |> List.min
    let maxY = if nodes.IsEmpty then 100.0 else nodes |> List.map (fun n -> n.Y) |> List.max
    let contentW = maxX - minX + 2.0 * margin
    let contentH = maxY - minY + 2.0 * margin
    let vbX = minX - margin
    let vbY = minY - margin
    ((vbX, vbY, contentW, contentH), nodes)

let calculateTreeBounds (root: TreeNode) =
    let root = layoutTreeViz root 0 (ref 100.0)
    let nodes = flattenTree root
    let margin = 50.0
    let minX = if nodes.IsEmpty then 0.0 else nodes |> List.map (fun n -> n.X) |> List.min
    let maxX = if nodes.IsEmpty then 100.0 else nodes |> List.map (fun n -> n.X) |> List.max
    let minY = if nodes.IsEmpty then 0.0 else nodes |> List.map (fun n -> n.Y) |> List.min
    let maxY = if nodes.IsEmpty then 100.0 else nodes |> List.map (fun n -> n.Y) |> List.max
    (maxX - minX + 2.0 * margin, maxY - minY + 2.0 * margin)

let private generateVisualElements (root: TreeNode) (colorList: string[]) (forcedW: float option) (forcedH: float option) : VisualElement list * (float * float * float * float) =
    let (bounds, nodes) = calculateTreeBoundsWithNodes root
    let (vbX, vbY, vbW, vbH) = bounds
    
    let finalW = forcedW |> Option.defaultValue vbW
    let finalH = forcedH |> Option.defaultValue vbH
    
    let ox = (finalW - vbW) / 2.0
    let oy = (finalH - vbH) / 2.0
    
    let finalBounds = (vbX - ox, vbY - oy, finalW, finalH)
    
    let elements = [
        // Render Connections
        for node in nodes do
            for child in node.Children do
                // End at the pointy top (cy - 20) of children and start at pointy bottom (cy + 20) of parent
                yield VLine(node.X, node.Y + 20.0, child.X, child.Y - 20.0, "#ccc")

        // Render Nodes
        for i, node in nodes |> List.indexed do
            let safeName = node.Name.Replace("<", "&lt;").Replace(">", "&gt;")
            let fill = 
                if i < colorList.Length then colorList.[i]
                else "white"

            
            // Highlight Elevated nodes (extrusion <> 3.0)
            let isElevated = Math.Abs(node.Extrusion - 3.0) > 0.01
            let hasBase = node.Base.IsSome
            
            let stroke = if isElevated then "#4a90e2" else "none"
            let strokeWidth = if isElevated then "2" else "0"
            
            // Draw Hexagon (width 50, height 40)
            let w, h = 50.0, 40.0
            let cx, cy = node.X, node.Y
            let pts = sprintf "%f,%f %f,%f %f,%f %f,%f %f,%f %f,%f" 
                        cx (cy - h/2.0)
                        (cx + w/2.0) (cy - h/4.0)
                        (cx + w/2.0) (cy + h/4.0)
                        cx (cy + h/2.0)
                        (cx - w/2.0) (cy + h/4.0)
                        (cx - w/2.0) (cy - h/4.0)
            
            // svPol template needs stroke-width
            yield VPolygon(pts, fill, stroke, strokeWidth)
            
            let textFill = "#333"
            yield VText(node.X, node.Y - 6.0, 11, "middle", textFill, safeName)
            
            let weightColor = if isElevated then "#4a90e2" else "#888"
            yield VText(node.X, node.Y + 11.0, 9, "middle", weightColor, node.Weight)

            if hasBase then
                let baseLabel = sprintf "B:%s" node.Base.Value
                yield VText(node.X + 22.0, node.Y - 18.0, 7, "start", "#27ae60", baseLabel)
    ]
    
    elements, finalBounds

// --------------------
// UI Rendering (Bolero)
// --------------------

type private svLin = Template<"""<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${st}" stroke-width="0.8" stroke-linecap="round" />""">
type private svPol = Template<"""<polygon points="${pts}" fill="${fl}" stroke="${st}" stroke-width="${sw}" />""">
type private svTxt = Template<"""<text x="${x}" y="${y}" font-size="${sz}" text-anchor="${ta}" font-family="'Outfit', sans-serif" dominant-baseline="middle" fill="${fl}">${nm}</text>""">

let viewTreeSvg (root: TreeNode) (colorList: string[]) : Node =
    let elements, (vbX, vbY, vbW, vbH) = generateVisualElements root colorList None None

    
    svg {
        "viewBox" => sprintf "%f %f %f %f" vbX vbY vbW vbH
        attr.style "width: 100%; height: auto; display: block; margin: auto; background: #fafafa; border-radius: 12px; padding: 10px;"

        for el in elements do
            match el with
            | VLine(x1, y1, x2, y2, st) -> 
                svLin().x1(string x1).y1(string y1).x2(string x2).y2(string y2).st(st).Elt()
            | VPolygon(pts, fill, stroke, sw) -> 
                svPol().pts(pts).fl(fill).st(stroke).sw(sw).Elt()
            | VText(x, y, sz, ta, fl, nm) -> 
                svTxt().x(string x).y(string y).sz(string sz).ta(ta).fl(fl).nm(nm).Elt()
    }

/// Main entry point for rendering the architectural graph from a code string
let viewCodeGraphFromString (code: string) : Node =
    try
        let processed = preprocessCode code
        let roots = parseOutput processed
        
        div {
            attr.style "width: 100%; margin-top: 20px; display: flex; flex-direction: column; gap: 20px;"
            forEach (roots |> List.indexed) <| fun (i, root) ->
                div {
                    attr.style "background: white; border: 1px solid #eee; border-radius: 12px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.02);"
                    div {
                        attr.style "font-weight: 600; color: #888; text-transform: uppercase; margin-bottom: 12px; font-size: 11px; letter-spacing: 0.5px;"
                        text (sprintf "Architecture Level %d" i)
                    }
                    viewTreeSvg root [||]

                }
        }
    with ex ->
        div { attr.style "color:#e74c3c; padding: 10px; font-size: 12px;"; text $"Graph Error: {ex.Message}" }

// --------------------
// String Rendering (Report)
// --------------------

let renderSvgToString (root: TreeNode) (colorList: string[]) (forcedW: float option) (forcedH: float option) =
    let elements, bounds = generateVisualElements root colorList forcedW forcedH

    let (vx, vy, vw, vh) = bounds
    let sb = System.Text.StringBuilder()
    
    sprintf """<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg" width="100%%" height="auto" style="background: #fafafa; border-radius: 12px; max-height: 100%%;">""" vx vy vw vh |> sb.AppendLine |> ignore
    
    for el in elements do
        match el with
        | VLine(x1, y1, x2, y2, st) ->
            sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="0.8" stroke-linecap="round" />""" x1 y1 x2 y2 st |> sb.AppendLine |> ignore
        | VPolygon(pts, fill, stroke, sw) -> 
            sprintf """<polygon points="%s" fill="%s" stroke="%s" stroke-width="%s" />""" pts fill stroke sw |> sb.AppendLine |> ignore
        | VText(x, y, sz, ta, fl, nm) ->
            sprintf """<text x="%f" y="%f" font-family="Outfit, sans-serif" font-size="%dpx" text-anchor="%s" dominant-baseline="middle" fill="%s">%s</text>""" x y sz ta fl nm |> sb.AppendLine |> ignore
            
    sb.AppendLine("</svg>") |> ignore
    sb.ToString()
