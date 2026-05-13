namespace Hywe.Node

open System
open System.Text

type VisualElement =
    | VLine of x1: float * y1: float * x2: float * y2: float * st: string
    | VPolygon of pts: string * fl: string * st: string * sw: string
    | VText of x: float * y: float * sz: int * ta: string * fl: string * nm: string

module Visualization =

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

    let private flattenTree (node: TreeNode) : TreeNode list =
        let rec loop n = n :: (n.Children |> List.collect loop)
        loop node

    let calculateTreeBounds (root: TreeNode) =
        let root = layoutTreeViz root 0 (ref 100.0)
        let nodes = flattenTree root
        let margin = 50.0
        let minX = if nodes.IsEmpty then 0.0 else nodes |> List.map (fun n -> n.X) |> List.min
        let maxX = if nodes.IsEmpty then 100.0 else nodes |> List.map (fun n -> n.X) |> List.max
        let minY = if nodes.IsEmpty then 0.0 else nodes |> List.map (fun n -> n.Y) |> List.min
        let maxY = if nodes.IsEmpty then 100.0 else nodes |> List.map (fun n -> n.Y) |> List.max
        (maxX - minX + 2.0 * margin, maxY - minY + 2.0 * margin)

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

    let generateVisualElements (root: TreeNode) (colorList: string[]) (forcedW: float option) (forcedH: float option) : VisualElement list * (float * float * float * float) =
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

    let renderSvgToString (root: TreeNode) (colorList: string[]) (forcedW: float option) (forcedH: float option) =
        let elements, bounds = generateVisualElements root colorList forcedW forcedH

        let (vx, vy, vw, vh) = bounds
        let sb = StringBuilder()
        
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
