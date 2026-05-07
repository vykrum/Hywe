module CodeNode

open System
open Bolero
open Bolero.Html
open Hywe.Core.Paxel
open NodeCode

// --------------------
// Syntax Preprocessing
// --------------------

/// Helper to identify if a block is a configuration attribute or a node definition
let private isAttrBlock (s: string) =
    not (s.Contains "/") || s.Contains "="

/// Standardizes Hywe syntax, ensuring markers, attributes and correctly formatted paths
let preprocessCode (code: string) : string =
    if String.IsNullOrWhiteSpace code then "" else
    
    let levels = splitIntoLevels code
    if levels.Length = 0 then 
        // Fallback for raw node list without parentheses
        if code.Contains "/" then $"L0(Q=VRCCNE/L=0/X=1)({code.Trim()})"
        else code
    else
        let processedLevels = 
            levels |> Array.mapi (fun i levelData ->
                let marker = 
                    if String.IsNullOrWhiteSpace levelData.Marker then sprintf "L%d" i 
                    else levelData.Marker
                
                let blocks = levelData.Blocks |> Array.filter (fun b -> not (String.IsNullOrWhiteSpace b))
                if blocks.Length = 0 then "" else
                
                // Separate attributes from nodes
                let attrBlock, nodeBlocks =
                    if isAttrBlock blocks.[0] then blocks.[0], blocks.[1..]
                    else "Q=VRCCNE/L=0/X=1", blocks // Default attributes
                
                // Remap legacy top-level integers (e.g., 2/ -> 1.0/) to standard paths if needed
                let topLevelInts =
                    nodeBlocks
                    |> Array.choose (fun line ->
                        let parts = line.Split('/')
                        match parts.Length >= 3 with
                        | true ->
                            let path = parts.[0]
                            let firstSeg = path.Split('.').[0]
                            match Int32.TryParse firstSeg with
                            | true, n when n > 1 -> Some n
                            | _ -> None
                        | false -> None
                    )
                    |> Set.ofArray

                let intMap =
                    topLevelInts
                    |> Seq.map (fun n -> 
                        let mapped = String.Join(".", Array.create (n - 1) "0")
                        (n.ToString(), match mapped = "" with true -> "1" | false -> "1." + mapped))
                    |> dict

                let updatedNodeStrings =
                    nodeBlocks
                    |> Array.map (fun line ->
                        let parts = line.Split('/')
                        match parts.Length >= 3 with
                        | true ->
                            let path = parts.[0]
                            let segments = path.Split('.')
                            let first = segments.[0]
                            let extra = if parts.Length > 3 then "/" + parts.[3] else ""
                            match intMap.ContainsKey(first) with
                            | true ->
                                let newPath = 
                                    if segments.Length > 1 then String.Join(".", Array.append [| intMap.[first] |] segments.[1..])
                                    else intMap.[first]
                                $"({newPath}/{parts.[1]}/{parts.[2]}{extra})"
                            | false -> $"({line})"
                        | false -> $"({line})"
                    )

                let result = String.Join("", updatedNodeStrings)
                $"{marker}({attrBlock}){result}"
            )

        String.Join("", processedLevels)

// --------------------
// Tree Parsing
// --------------------

/// Parses the standardized syntax into hierarchical tree structures for visualization
let parseOutput (code: string) : TreeNode list =
    let levels = splitIntoLevels code
    
    levels |> Array.toList |> List.mapi (fun lvlIdx levelData ->
        let entries =
            levelData.Blocks 
            |> Array.filter (fun b -> not (isAttrBlock b))
            |> Array.choose (fun item ->
                let parts = item.Split('/')
                if parts.Length >= 3 then
                    let path = parts.[0]
                    if path.StartsWith("0") then None
                    else
                        let weight = parts.[1]
                        let name = parts.[2]
                        let extrusion = 
                            if parts.Length > 3 then match parts.[3] with Float v -> v | _ -> 3.0 
                            else 3.0
                        Some (path, weight, name, extrusion)
                else None
            )

        // Find the root (path "1") or default to the first node
        let rootEntry = 
            entries |> Array.tryFind (fun (p, _, _, _) -> p = "1")
            |> Option.orElse (entries |> Array.tryHead)

        match rootEntry with
        | None ->
            { Id = Guid.NewGuid(); Name = sprintf "Level %d" lvlIdx; Weight = "0"; X = 0.0; Y = 0.0; Children = []; Level = lvlIdx; Extrusion = 3.0 }
        | Some (rootPath, _, _, _) ->
            let nodeMap =
                entries
                |> Seq.map (fun (path, weight, name, extrusion) ->
                    path, {
                        Id = Guid.NewGuid(); Name = name; Weight = weight; X = 0.0; Y = 0.0; Children = []; Level = lvlIdx; Extrusion = extrusion
                    }
                )
                |> dict
                |> System.Collections.Generic.Dictionary

            let byDepth =
                entries
                |> Array.groupBy (fun (path, _, _, _) -> path.Split('.').Length)
                |> Array.sortByDescending fst

            for (_, group) in byDepth do
                for (path, _, _, _) in group do
                    if path.Contains "." then
                        let parentPath = path.Substring(0, path.LastIndexOf('.'))
                        if nodeMap.ContainsKey(parentPath) && nodeMap.ContainsKey(path) then
                            let parent = nodeMap.[parentPath]
                            let child = nodeMap.[path]
                            nodeMap.[parentPath] <- { parent with Children = parent.Children @ [child] }

            nodeMap.[rootPath]
    )

// --------------------
// SVG Visualization
// --------------------

type VisualElement =
    | VLine of x1:float * y1:float * x2:float * y2:float * color:string
    | VPolygon of points:string * fill:string * stroke:string
    | VText of x:float * y:float * size:int * anchor:string * fill:string * text:string

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
    let minX = nodes |> List.map (fun n -> n.X) |> List.min
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    let minY = nodes |> List.map (fun n -> n.Y) |> List.min
    let maxY = nodes |> List.map (fun n -> n.Y) |> List.max
    let contentW = maxX - minX + 2.0 * margin
    let contentH = maxY - minY + 2.0 * margin
    let vbX = minX - margin
    let vbY = minY - margin
    ((vbX, vbY, contentW, contentH), nodes)

let calculateTreeBounds (root: TreeNode) =
    let root = layoutTreeViz root 0 (ref 100.0)
    let nodes = flattenTree root
    let margin = 50.0
    let minX = nodes |> List.map (fun n -> n.X) |> List.min
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    let minY = nodes |> List.map (fun n -> n.Y) |> List.min
    let maxY = nodes |> List.map (fun n -> n.Y) |> List.max
    (maxX - minX + 2.0 * margin, maxY - minY + 2.0 * margin)

let private generateVisualElements (root: TreeNode) (colorMap: Map<string, string>) (forcedW: float option) (forcedH: float option) : VisualElement list * (float * float * float * float) =
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
        for node in nodes do
            let safeName = node.Name.Replace("<", "&lt;").Replace(">", "&gt;")
            let fill = Map.tryFind node.Name colorMap |> Option.defaultValue "white"
            
            // Highlight Elevated nodes (extrusion <> 3.0)
            let isElevated = Math.Abs(node.Extrusion - 3.0) > 0.01
            let stroke = "none"
            
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
            
            yield VPolygon(pts, fill, stroke)
            
            let textFill = "#333"
            yield VText(node.X, node.Y - 6.0, 11, "middle", textFill, safeName)
            
            let weightColor = if isElevated then "#4a90e2" else "#888"
            yield VText(node.X, node.Y + 11.0, 9, "middle", weightColor, node.Weight)
    ]
    
    elements, finalBounds

// --------------------
// UI Rendering (Bolero)
// --------------------

type private svLin = Template<"""<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${st}" stroke-width="0.8" stroke-linecap="round" />""">
type private svPol = Template<"""<polygon points="${pts}" fill="${fl}" stroke="${st}" stroke-width="0" />""">
type private svTxt = Template<"""<text x="${x}" y="${y}" font-size="${sz}" text-anchor="${ta}" font-family="'Outfit', sans-serif" dominant-baseline="middle" fill="${fl}">${nm}</text>""">

let viewTreeSvg (root: TreeNode) (colorMap: Map<string, string>) : Node =
    let elements, (vbX, vbY, vbW, vbH) = generateVisualElements root colorMap None None
    
    svg {
        "viewBox" => sprintf "%f %f %f %f" vbX vbY vbW vbH
        attr.style "width: 100%; height: auto; display: block; margin: auto; background: #fafafa; border-radius: 12px; padding: 10px;"

        for el in elements do
            match el with
            | VLine(x1, y1, x2, y2, st) -> 
                svLin().x1(string x1).y1(string y1).x2(string x2).y2(string y2).st(st).Elt()
            | VPolygon(pts, fill, stroke) -> 
                svPol().pts(pts).fl(fill).st(stroke).Elt()
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
                    viewTreeSvg root Map.empty
                }
        }
    with ex ->
        div { attr.style "color:#e74c3c; padding: 10px; font-size: 12px;"; text $"Graph Error: {ex.Message}" }

// --------------------
// String Rendering (Report)
// --------------------

let renderSvgToString (root: TreeNode) (colorMap: Map<string, string>) (forcedW: float option) (forcedH: float option) =
    let elements, bounds = generateVisualElements root colorMap forcedW forcedH
    let (vx, vy, vw, vh) = bounds
    let sb = System.Text.StringBuilder()
    
    sprintf """<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg" width="100%%" height="auto" style="background: #fafafa; border-radius: 12px; max-height: 100%%;">""" vx vy vw vh |> sb.AppendLine |> ignore
    
    for el in elements do
        match el with
        | VLine(x1, y1, x2, y2, st) ->
            sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" stroke-width="0.8" stroke-linecap="round" />""" x1 y1 x2 y2 st |> sb.AppendLine |> ignore
        | VPolygon(pts, fl, st) ->
            sprintf """<polygon points="%s" fill="%s" stroke="none" stroke-width="0" />""" pts fl |> sb.AppendLine |> ignore
        | VText(x, y, sz, ta, fl, nm) ->
            sprintf """<text x="%f" y="%f" font-family="Outfit, sans-serif" font-size="%dpx" text-anchor="%s" dominant-baseline="middle" fill="%s">%s</text>""" x y sz ta fl nm |> sb.AppendLine |> ignore
            
    sb.AppendLine("</svg>") |> ignore
    sb.ToString()

