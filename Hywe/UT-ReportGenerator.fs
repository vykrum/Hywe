module Hywe.ReportGenerator

open System
open System.Text
open Coxel
open Hexel
open ModelTypes
open NodeCode

type PageEntry = {
    PageNumber: int
    SectionTitle: string
    Depth: int
}

let buildPageManifest (opts: ReportOptions) (levels: int list) : PageEntry list =
    let mutable pages = []
    let mutable pageNum = 1

    let addPage title depth =
        pages <- { PageNumber = pageNum; SectionTitle = title; Depth = depth } :: pages
        pageNum <- pageNum + 1

    if opts.IncludeCover then addPage "Cover Page" 0
    // Table of Contents removed


    for level in levels do
        let section = 
            match Map.tryFind level opts.LevelSections with
            | Some s -> s
            | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23] }
            
        let hasAny = section.FlowChart || section.BatchOverview || section.Variations
        if hasAny then
            addPage (sprintf "Level %d" level) 0
            if section.FlowChart then addPage "Flow Chart" 1
            if section.BatchOverview then 
                addPage "Batch Overview" 1
                let numPages = int (Math.Ceiling(24.0 / 8.0))
                if numPages > 1 then pageNum <- pageNum + (numPages - 1)
            if section.Variations then
                for i = 0 to 23 do
                    if section.SelectedVariations.Contains(i) then
                        addPage (Page.indexToSqn i) 2
    
    let revPages = pages |> List.rev
    
    revPages |> List.map (fun p -> 
        if p.SectionTitle = "Cover Page" then p
        else { p with PageNumber = p.PageNumber }
    )

let renderFloorPlanSvg (shapes: BatchComponent[]) (cxOuIl: (float*float)[][]) : string =
    let sb = StringBuilder()
    let mutable minX = Double.MaxValue
    let mutable minY = Double.MaxValue
    let mutable maxX = Double.MinValue
    let mutable maxY = Double.MinValue

    let getPoints (pts: float[]) =
        let res = StringBuilder()
        for i = 0 to pts.Length / 2 - 1 do
            let x = pts.[i * 2]
            let y = pts.[i * 2 + 1]
            minX <- min minX x
            minY <- min minY y
            maxX <- max maxX x
            maxY <- max maxY y
            res.Append(sprintf "%f,%f " x y) |> ignore
        res.ToString().TrimEnd()

    let polygons = 
        shapes |> Array.map (fun shp ->
            let pts = getPoints shp.points
            sprintf """<polygon points="%s" fill="%s" opacity="0.75" />""" pts shp.color
        ) |> String.concat ""
        
    let boundaries = 
        cxOuIl |> Array.map (fun bdr ->
            let pts = 
                bdr |> Array.map (fun (x,y) -> 
                    minX <- min minX x
                    minY <- min minY y
                    maxX <- max maxX x
                    maxY <- max maxY y
                    sprintf "%f,%f" x y
                ) |> String.concat " "
            sprintf """<polygon points="%s" fill="none" stroke="#000" stroke-width="2" opacity="0.1" />""" pts
        ) |> String.concat ""
        
    // default viewBox if no shapes
    if shapes.Length = 0 && cxOuIl.Length = 0 then
        minX <- 0.0; minY <- 0.0; maxX <- 1.0; maxY <- 1.0
    
    let pad = 8.0
    let w = maxX - minX + 2.0 * pad
    let h = maxY - minY + 2.0 * pad
    
    sprintf """<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg" width="100%%" height="100%%">
    %s
    %s
    </svg>""" (minX - pad) (minY - pad) w h polygons boundaries

let rec flattenTree (node: TreeNode) : TreeNode list =
    node :: (node.Children |> List.collect flattenTree)

let rec layoutTree (node: TreeNode) (depth: int) (xRef: float ref) : TreeNode =
    let horizontalSpacing = 100.0
    let verticalSpacing = 70.0

    let laidOutChildren =
        node.Children
        |> List.map (fun child -> layoutTree child (depth + 1) xRef)

    let x =
        if laidOutChildren.IsEmpty then
            let x = xRef.Value
            xRef.Value <- x + horizontalSpacing
            x
        else
            laidOutChildren |> List.averageBy (fun n -> n.X)

    {
        node with
            X = x
            Y = float depth * verticalSpacing
            Children = laidOutChildren
    }

let renderFlowchartSvg (root: TreeNode) : string =
    let root = layoutTree root 0 (ref 100.0)
    let nodes = flattenTree root

    let margin = 60.0
    let minX = nodes |> List.map (fun n -> n.X) |> List.min
    let maxX = nodes |> List.map (fun n -> n.X) |> List.max
    let minY = nodes |> List.map (fun n -> n.Y) |> List.min
    let maxY = nodes |> List.map (fun n -> n.Y) |> List.max

    let vbX = minX - margin
    let vbY = minY - margin
    let vbW = maxX - minX + 2.0 * margin
    let vbH = maxY - minY + 2.0 * margin
    
    let sb = StringBuilder()
    sprintf """<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg" width="100%%" height="100%%">""" vbX vbY vbW vbH |> sb.AppendLine |> ignore
    
    // Connections
    let rec renderConnections (parent: TreeNode) =
        for child in parent.Children do
            sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="#cccccc" stroke-width="2" />""" parent.X parent.Y child.X child.Y |> sb.AppendLine |> ignore
            renderConnections child
    
    renderConnections root

    // Nodes
    for node in nodes do
        let safeName = node.Name.Replace("<", "&lt;").Replace(">", "&gt;")
        sprintf """<rect x="%f" y="%f" width="80" height="40" rx="8" ry="8" fill="white" stroke="#aaaaaa" stroke-width="1.5" />""" (node.X - 40.0) (node.Y - 20.0) |> sb.AppendLine |> ignore
        sprintf """<text x="%f" y="%f" font-family="sans-serif" font-size="12px" text-anchor="middle" dominant-baseline="middle" fill="#333333">%s</text>""" node.X (node.Y - 5.0) safeName |> sb.AppendLine |> ignore
        sprintf """<text x="%f" y="%f" font-family="sans-serif" font-size="10px" text-anchor="middle" dominant-baseline="middle" fill="#888888">%s</text>""" node.X (node.Y + 10.0) node.Weight |> sb.AppendLine |> ignore

    sb.AppendLine("</svg>") |> ignore
    sb.ToString()

let renderLegend (shapes: {| color: string; points: float[]; name: string; lx: float; ly: float |}[]) : string =
    let uniqueRooms = 
        shapes 
        |> Array.distinctBy (fun s -> s.color)
        |> Array.sortBy (fun s -> s.name)
    
    let items = 
        uniqueRooms 
        |> Array.map (fun s -> 
            let safeName = s.name.Replace("<", "&lt;").Replace(">", "&gt;")
            sprintf """<div style="display: flex; align-items: center; gap: 6px; font-size: 9px; white-space: nowrap;">
                <div style="width: 10px; height: 10px; background: %s; border: 1px solid #eee; border-radius: 2px;"></div>
                <span>%s</span>
            </div>""" s.color safeName)
        |> String.concat ""
    
    sprintf """<div class="legend" style="display: flex; flex-wrap: wrap; gap: 12px; padding: 4px 10px; background: #fafafa; border-radius: 4px; margin-top: 20px; margin-bottom: 10px;">%s</div>""" items

let renderAreaTable (cxls: Cxl[]) (cxlAvl: int[]) : string =
    let sb = StringBuilder()
    sb.AppendLine("""<table class="report-table">
        <thead>
            <tr><th>Room Name</th><th>Required</th><th>Achieved</th><th>Open</th></tr>
        </thead>
        <tbody>""") |> ignore
        
    let hxlAreaX = 4
    for i = 0 to cxls.Length - 1 do
        let cxl = cxls.[i]
        let avl = if i < cxlAvl.Length then cxlAvl.[i] else 0
        let reqSz = (prpVlu cxl.Size |> int) * hxlAreaX
        let achSz = (Array.length cxl.Hxls) * hxlAreaX
        let opnSz = avl * hxlAreaX
        let name = (prpVlu cxl.Name).Replace("<", "&lt;").Replace(">", "&gt;")
        sb.AppendLine(sprintf """<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td></tr>""" name reqSz achSz opnSz) |> ignore
        
    sb.AppendLine("</tbody></table>") |> ignore
    sb.ToString()

let renderAdjacencyMatrix (cxls: Cxl[]) : string =
    let names, matrix = Coxel.cxlAdj cxls
    let sb = StringBuilder()
    sb.AppendLine("""<table class="report-table adjacency-matrix">
        <thead>
            <tr><th>Room</th>""") |> ignore
    
    for name in names do
        let safeName = name.Replace("<", "&lt;").Replace(">", "&gt;")
        sb.Append(sprintf """<th class="adj-header"><div class="rotated-text">%s</div></th>""" safeName) |> ignore
    sb.AppendLine("</tr></thead><tbody>") |> ignore
    
    for i = 0 to matrix.Length - 1 do
        let row = matrix.[i]
        let safeName = names.[i].Replace("<", "&lt;").Replace(">", "&gt;")
        sb.Append(sprintf """<tr><th>%s</th>""" safeName) |> ignore
        for adj in row do
            let cell = if adj then "■" else ""
            let cls = if adj then "adj-true" else "adj-false"
            sb.Append(sprintf """<td class="%s">%s</td>""" cls cell) |> ignore
        sb.AppendLine("</tr>") |> ignore
        
    sb.AppendLine("</tbody></table>") |> ignore
    sb.ToString()


// --- TEMPLATES ---
let tBase : Printf.StringFormat<string -> string> = """<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>%s Report</title>
<style>
@page { size: A3 landscape; margin: 0; }
body { font-family: 'Inter', system-ui, sans-serif; margin: 0; padding: 0; background: #fff; color: #333; font-size: 14px; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
.page { width: 420mm; height: 297mm; page-break-after: always; position: relative; box-sizing: border-box; overflow: hidden; }
.page-inner { padding: 8mm 15mm 15mm 15mm; height: 100%%; box-sizing: border-box; display: flex; flex-direction: column; }
.cover-page { display: flex; flex-direction: row; }
.cover-left { flex: 1; padding: 30mm; background: #f8f8f8; display: flex; flex-direction: column; justify-content: space-between; }
.cover-right { flex: 1.5; background: #fff; display: flex; align-items: center; justify-content: center; position: relative; }
.cover-right img { max-width: 100%%; max-height: 100%%; object-fit: contain; }
.cover-title { font-size: 42px; font-weight: 300; margin: 0 0 10px 0; letter-spacing: 2px; }
.cover-number { font-size: 18px; color: #666; margin-bottom: 40px; }
.cover-meta { margin-top: auto; }
.cover-meta table { border-collapse: collapse; font-size: 16px; }
.cover-meta td { padding: 8px 20px 8px 0; vertical-align: top; }
.cover-meta td:first-child { color: #888; text-transform: uppercase; letter-spacing: 1px; font-size: 12px; }
.header { display: flex; justify-content: space-between; align-items: flex-end; border-bottom: 1px solid #eee; padding-bottom: 4mm; margin-bottom: 4mm; }
.header-title { font-size: 24px; font-weight: 300; }
.header-subtitle { font-size: 14px; color: #888; letter-spacing: 1px; text-transform: uppercase; }
.footer { position: absolute; bottom: 10mm; left: 15mm; right: 15mm; display: flex; justify-content: space-between; font-size: 10px; color: #aaa; border-top: 1px solid #eee; padding-top: 3mm; }
.content-area { display: flex; flex: 1; gap: 10mm; min-height: 0; }
.col-left { flex: 2.5; display: flex; flex-direction: column; min-width: 0; }
.col-right { flex: 1.5; display: flex; flex-direction: column; min-width: 0; overflow: hidden; container-type: inline-size; }
.report-table { width: 100%%; border-collapse: collapse; font-size: 9.5px; margin-bottom: 10px; }
.report-table th, .report-table td { padding: 4px 6px; border-bottom: 1px solid #eee; text-align: left; }
.report-table th { background: #fafafa; font-weight: 600; color: #555; }
.adjacency-matrix { table-layout: fixed; width: 100%%; border-collapse: collapse; }
.adjacency-matrix th.adj-header { height: 60px; white-space: nowrap; vertical-align: bottom; }
.rotated-text { transform: rotate(-90deg); width: 100%%; display: inline-block; transform-origin: bottom left; margin-left: 50%%; font-size: clamp(6px, 2.5cqw, 9px); }
.adjacency-matrix td { text-align: center; border: 1px solid #eee; padding: 0; aspect-ratio: 1/1; font-size: clamp(6px, 2cqw, 10px); }
.adj-true { background: #ddd; color: #ddd; }
.adj-false { background: #fff; color: #fff; }
.toc-d0 { font-weight: 600; padding-top: 20px !important; border-bottom: 2px solid #eee !important; }
.batch-grid { display: grid; grid-template-columns: repeat(4, 1fr); grid-template-rows: repeat(2, 1fr); gap: 10px; width: 100%%; flex: 1; min-height: 0; }
.batch-cell { border: 1px solid #eee; display: flex; flex-direction: column; padding: 5px; box-sizing: border-box; }
.batch-cell svg { flex: 1; min-height: 0; }
.batch-label { font-size: 9px; text-align: center; color: #888; margin-top: 5px; }
.flow-chart { width: 100%%; height: 100%%; }
</style>
</head>
<body>
"""

let tHeader : Printf.StringFormat<string -> string -> string> = """<div class="header">
    <div class="header-left">
        <div class="header-title">%s</div>
        <div class="header-subtitle">%s</div>
    </div>
    <div class="header-right" style="text-align: right;">
        <img src="https://vykrum.github.io/Hywe/images/hyweLogoBanner.png" style="width: 150px; height: auto;" />
    </div>
</div>"""

let tFooter : Printf.StringFormat<string -> int -> string> = """<div class="footer">
    <div style="font-size: 10px; color: #aaa;">%s</div>
    <span>Page %d</span>
</div>"""

let tCover : Printf.StringFormat<string -> string -> string -> string -> string -> string -> string -> string -> string> = """<div class="page"><div class="cover-page" style="height:100%%;">
    <div class="cover-left" style="flex: 1; padding: 30mm; background: #f8f8f8; display: flex; flex-direction: column; justify-content: space-between; border-right: 1px solid #eee;">
        <div>
            <h1 class="cover-title">%s</h1>
            <div class="cover-number">%s</div>
            <div style="font-size: 16px; line-height: 1.6; color: #444; max-width: 400px;">%s</div>
        </div>
        <div class="cover-meta">
            <table>
                <tr><td>Author</td><td>%s</td></tr>
                <tr><td>Client</td><td>%s</td></tr>
                <tr><td>Date</td><td>%s</td></tr>
            </table>
        </div>
    </div>
    <div class="cover-right" style="flex: 2; display: flex; align-items: center; justify-content: center; overflow: hidden; padding: 40px; background: #ffffff; position: relative;">
        <div style="position: absolute; top: 10mm; right: 15mm;">
            <img src="https://vykrum.github.io/Hywe/images/hyweLogoBanner.png" style="width: 150px; height: auto;" />
        </div>
        <img src="%s" style="max-width: 100%%; max-height: 100%%; object-fit: contain;" />
    </div>
</div>%s</div>"""


let tFlowChart : Printf.StringFormat<string -> string -> string -> string> = """<div class="page"><div class="page-inner">
    %s
    <div class="content-area" style="display: flex; align-items: center; justify-content: center;">
        %s
    </div>
    %s
</div></div>"""

let tBatchGrid1 : Printf.StringFormat<string -> string> = """<div class="page"><div class="page-inner">
    %s
    <div class="content-area" style="flex-direction: column;">
        <div class="batch-grid">"""

let tBatchCell : Printf.StringFormat<string -> string -> string> = """<div class="batch-cell">%s<div class="batch-label">%s</div></div>"""

let tBatchGrid2 : Printf.StringFormat<string -> string -> string> = """</div>%s</div>%s</div></div>"""

let tVariation : Printf.StringFormat<string -> string -> string -> string -> string -> string -> string> = """<div class="page"><div class="page-inner">
    %s
    <div class="content-area">
        <div class="col-left" style="display: flex; flex-direction: column;">
            <div style="flex: 1; min-height: 0;">%s</div>
            %s
        </div>
        <div class="col-right">
            %s
            %s
        </div>
    </div>
    %s
</div></div>"""


let generateReportHtml (opts: ReportOptions) (tree: SubModel) (batches: Map<int, BatchConfgrtns[]>) : string =
    let sb = StringBuilder()
    let d = DateTime.Now.ToString("dd MMM yyyy")
    
    sprintf tBase opts.ProjectTitle |> sb.AppendLine |> ignore

    let levels = batches.Keys |> Seq.toList |> List.sort
    let manifest = buildPageManifest opts levels
    let mutable currentPage = 1

    let renderHeader title subtitle =
        sprintf tHeader title subtitle

    let renderFooter () =
        let html = sprintf tFooter d currentPage
        currentPage <- currentPage + 1
        html

    if opts.IncludeCover then
        let svgContent = (defaultArg opts.Captured3DImage "").Replace("%%", "%").Replace("%", "%%")
        sprintf tCover opts.ProjectTitle opts.ProjectNumber opts.Description opts.Author opts.ClientName d svgContent (renderFooter()) |> sb.AppendLine |> ignore

    // Table of Contents removed as requested

    for level in levels do
        let section = 
            match Map.tryFind level opts.LevelSections with
            | Some s -> s
            | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23] }
            
        let batchInfo = batches.[level]

        if section.FlowChart then
            let root = tree.Levels.[level]
            let svg = renderFlowchartSvg root
            sprintf tFlowChart (renderHeader (sprintf "Flow Chart — Level %d" level) opts.ProjectTitle) svg (renderFooter()) |> sb.AppendLine |> ignore

        if section.BatchOverview && batchInfo.Length > 0 then
            let limit = Math.Min(23, batchInfo.Length - 1)
            let totalPages = int (Math.Ceiling(float (limit + 1) / 8.0))
            
            for pageIndex = 0 to totalPages - 1 do
                let chunkStart = pageIndex * 8
                let chunkEnd = Math.Min(chunkStart + 7, limit)
                let pageStr = if totalPages > 1 then sprintf " (%d/%d)" (pageIndex+1) totalPages else ""
                
                sprintf tBatchGrid1 (renderHeader (sprintf "Batch Overview — Level %d%s" level pageStr) opts.ProjectTitle) |> sb.AppendLine |> ignore
                
                for i = chunkStart to chunkEnd do
                    let svg = renderFloorPlanSvg batchInfo.[i].shapes batchInfo.[i].cxOuIl
                    sprintf tBatchCell svg batchInfo.[i].sqnName |> sb.AppendLine |> ignore
                
                let legend = if batchInfo.Length > 0 then renderLegend batchInfo.[0].shapes else ""
                sprintf tBatchGrid2 legend (renderFooter()) |> sb.AppendLine |> ignore

        if section.Variations then
            for i = 0 to Math.Min(23, batchInfo.Length - 1) do
                if section.SelectedVariations.Contains(i) then
                    let conf = batchInfo.[i]
                    let svg = renderFloorPlanSvg conf.shapes conf.cxOuIl
                    let legend = renderLegend conf.shapes
                    
                    // Filter Coxels for this level specifically
                    let levelCxls = 
                        conf.cxCxl1 |> Array.filter (fun c -> 
                            let (_, _, z) = Hexel.hxlCrd c.Base
                            z = level)
                            
                    let areaTable = renderAreaTable levelCxls conf.cxlAvl
                    let adjMatrix = renderAdjacencyMatrix levelCxls
                    
                    sprintf tVariation (renderHeader (sprintf "%s — Level %d" conf.sqnName level) "") svg legend areaTable adjMatrix (renderFooter()) |> sb.AppendLine |> ignore

    sb.AppendLine("</body></html>") |> ignore
    sb.ToString()
