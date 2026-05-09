module Hywe.ReportGenerator

open System
open System.Text
open ModelTypes
open Hywe.Core
open Hywe.Core.Coxel
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
            | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
            
        let hasAny = section.FlowChart || section.BatchOverview || section.Variations
        if hasAny then
            addPage (sprintf "Level %d" level) 0
            if section.FlowChart then addPage "Flow Chart" 1
            if section.BatchOverview then 
                addPage "Batch Overview" 1
                let numPages = int (System.Math.Ceiling(24.0 / 8.0))
                if numPages > 1 then pageNum <- pageNum + (numPages - 1)
            if section.Variations then
                for i = 0 to 23 do
                    if section.SelectedVariations.Contains(i) then
                        addPage (Page.labelPhrase.[i].ToString()) 2
    
    let revPages = pages |> List.rev
    
    revPages |> List.map (fun p -> 
        if p.SectionTitle = "Cover Page" then p
        else { p with PageNumber = p.PageNumber }
    )

let renderFloorPlanSvg (shapes: BatchComponent[]) (cxOuIl: (int*int)[][]) (maxW: float option) (maxH: float option) : string =
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
                    let fx, fy = float x, float y
                    minX <- min minX fx
                    minY <- min minY fy
                    maxX <- max maxX fx
                    maxY <- max maxY fy
                    sprintf "%f,%f" fx fy
                ) |> String.concat " "
            sprintf """<polygon points="%s" fill="none" stroke="#000" stroke-width="0.1" opacity="0.1" />""" pts
        ) |> String.concat ""
        
    if shapes.Length = 0 && cxOuIl.Length = 0 then
        minX <- 0.0; minY <- 0.0; maxX <- 1.0; maxY <- 1.0
    
    let pad = 8.0
    let contentW = maxX - minX
    let contentH = maxY - minY
    
    let w = maxW |> Option.defaultValue contentW
    let h = maxH |> Option.defaultValue contentH
    
    // Centering offset if forced size is larger
    let ox = (w - contentW) / 2.0
    let oy = (h - contentH) / 2.0
    
    sprintf """<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg" width="100%%" height="100%%">
    <g transform="translate(%f, %f)">
    %s
    %s
    </g>
    </svg>""" (minX - pad - ox) (minY - pad - oy) (w + 2.0 * pad) (h + 2.0 * pad) 0.0 0.0 polygons boundaries

let renderFlowchartSvg (root: TreeNode) (colorList: string[]) (maxW: float option) (maxH: float option) : string =
    CodeNode.renderSvgToString root colorList maxW maxH


let renderLegend (shapes: {| color: string; points: float[]; name: string; lx: float; ly: float |}[]) : string =
    let uniqueRooms = 
        shapes 
        |> Array.distinctBy (fun s -> s.name.Trim(), s.color)
        |> Array.sortBy (fun s -> s.name.Trim())
    
    let items = 
        uniqueRooms 
        |> Array.map (fun s -> 
            let safeName = s.name.Replace("<", "&lt;").Replace(">", "&gt;")
            sprintf """<div style="display: flex; align-items: center; gap: 6px; font-size: 9px; white-space: nowrap;">
                <div style="width: 10px; height: 10px; background: %s; border: 1px solid #eee; border-radius: 2px;"></div>
                <span>%s</span>
            </div>""" s.color safeName)
        |> String.concat ""
    
    if uniqueRooms.Length = 0 then ""
    else sprintf """<div class="legend" style="display: flex; flex-wrap: wrap; gap: 12px; padding: 4px 10px; background: #fafafa; border-radius: 4px; margin-top: 20px; margin-bottom: 10px;">%s</div>""" items
    
let renderAreaTable (cxls: Cxl[]) (cxlAvl: int[]) (colorMap: Map<string, string>) (elv: int) : string =
    let sb = StringBuilder()
    let fontSize = if cxls.Length > 25 then "7.5px" else if cxls.Length > 15 then "8.5px" else "9.5px"
    sb.AppendLine(sprintf """<table class="report-table" style="font-size: %s;">
        <thead>
            <tr><th></th><th>Required</th><th>Achieved</th><th>Open</th></tr>
        </thead>
        <tbody>""" fontSize) |> ignore
        
    let hxlAreaX = 4
    for i = 0 to cxls.Length - 1 do
        let cxl = cxls[i]
        let avl = if i < cxlAvl.Length then cxlAvl[i] else 0
        let isRootLvl0 = (prpVlu cxl.Rfid = "1" || prpVlu cxl.Name = "Root") && elv = 0
        let count = if isRootLvl0 then (prpVlu cxl.Size |> float) + 1.0 else (prpVlu cxl.Size |> float)
        let reqSz = int (count * float hxlAreaX)
        let achSz = (Array.length cxl.Hxls) * hxlAreaX
        let opnSz = avl * hxlAreaX
        let name = (prpVlu cxl.Name).Replace("<", "&lt;").Replace(">", "&gt;")
        
        let clr = Map.tryFind name colorMap |> Option.defaultValue "#eee"
        let swatch = sprintf """<div style="width: 8px; height: 8px; background: %s; border: 1px solid #ddd; display: inline-block; margin-right: 6px; border-radius: 1px; vertical-align: middle;"></div>""" clr
        
        sb.AppendLine(sprintf """<tr><td>%s<span style="vertical-align: middle;">%s</span></td><td>%d</td><td>%d</td><td>%d</td></tr>""" swatch name reqSz achSz opnSz) |> ignore
        
    sb.AppendLine("</tbody></table>") |> ignore
    sb.ToString()

let renderAdjacencyMatrix (cxls: Cxl[]) (colorMap: Map<string, string>) : string =
    let names, matrix = Coxel.cxlAdj cxls
    let sb = StringBuilder()
    let fontSize = if names.Length > 25 then "6px" else if names.Length > 15 then "7.5px" else "9px"
    let headerHeight = if names.Length > 20 then "40px" else "60px"
    sb.AppendLine(sprintf """<table class="report-table adjacency-matrix" style="font-size: %s;">
        <thead>
            <tr style="height: %s;"><th></th>""" fontSize headerHeight) |> ignore
    
    for name in names do
        let safeName = name.Replace("<", "&lt;").Replace(">", "&gt;")
        let clr = Map.tryFind name colorMap |> Option.defaultValue "#eee"
        let swatch = sprintf """<div style="width: 6px; height: 6px; background: %s; border: 1px solid #ddd; display: inline-block; margin-right: 3px; border-radius: 1px; vertical-align: middle;"></div>""" clr
        sb.Append(sprintf """<th class="adj-header"><div class="rotated-text">%s%s</div></th>""" swatch safeName) |> ignore
    sb.AppendLine("</tr></thead><tbody>") |> ignore
    
    for i = 0 to matrix.Length - 1 do
        let row = matrix[i]
        let name = names[i]
        let safeName = name.Replace("<", "&lt;").Replace(">", "&gt;")
        let clr = Map.tryFind name colorMap |> Option.defaultValue "#eee"
        let swatch = sprintf """<div style="width: 6px; height: 6px; background: %s; border: 1px solid #ddd; display: inline-block; margin-right: 4px; border-radius: 1px; vertical-align: middle;"></div>""" clr
        sb.Append(sprintf """<tr><th>%s<span style="vertical-align: middle;">%s</span></th>""" swatch safeName) |> ignore
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
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600&display=swap" rel="stylesheet">
<style>
@page { size: A3 landscape; margin: 0; }
body { font-family: 'Outfit', system-ui, sans-serif; margin: 0; padding: 0; background: #fff; color: #333; font-size: 14px; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
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
    <div class="cover-right" style="flex: 2; display: flex; align-items: center; justify-content: center; overflow: hidden; background: #ffffff; position: relative;">
        <div style="position: absolute; top: 10mm; right: 15mm;">
            <img src="https://vykrum.github.io/Hywe/images/hyweLogoBanner.png" style="width: 150px; height: auto;" />
        </div>
        %s
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

let tBatchCell : Printf.StringFormat<string -> string -> string> = """<div class="batch-cell" style="overflow: hidden; display: flex; flex-direction: column;">%s<div class="batch-label">%s</div></div>"""

let tBatchGrid2 : Printf.StringFormat<string -> string -> string> = """</div>%s</div>%s</div></div>"""

let tVariation : Printf.StringFormat<string -> string -> string -> string -> string -> string -> string> = """<div class="page"><div class="page-inner">
    %s
    <div class="content-area">
        <div class="col-left" style="display: flex; flex-direction: column; min-width: 0;">
            <div style="flex: 1; min-height: 0;">%s</div>
            %s
        </div>
        <div class="col-right" style="display: flex; flex-direction: column; gap: 15px; overflow: hidden;">
            <div style="flex: 1.2; min-height: 0; overflow: auto;">%s</div>
            <div style="flex: 1; min-height: 0; overflow: auto;">%s</div>
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
        let captureHtml = 
            match opts.Captured3DImage with
            | Some url -> sprintf """<img src="%s" style="width: 100%%; height: 100%%; object-fit: contain;" />""" url
            | None -> ""
        sprintf tCover opts.ProjectTitle opts.ProjectNumber opts.Description opts.Author opts.ClientName d captureHtml (renderFooter()) |> sb.AppendLine |> ignore

    // Table of Contents removed as requested
    
    // Pre-calculate global flowchart bounds for consistent scaling across levels
    let flowChartMaxW, flowChartMaxH =
        let allTrees = 
            levels |> List.choose (fun lvl ->
                let s = Map.tryFind lvl opts.LevelSections |> Option.defaultValue { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
                if s.FlowChart then Some (tree.Levels.[lvl]) else None
            )
        if allTrees.IsEmpty then None, None
        else
            let bounds = allTrees |> List.map CodeNode.calculateTreeBounds
            let mw = bounds |> List.map fst |> List.max
            let mh = bounds |> List.map snd |> List.max
            Some mw, Some mh

    for level in levels do
        let section = 
            match Map.tryFind level opts.LevelSections with
            | Some s -> s
            | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
        let batchInfo = batches.[level]
        let maxW = if batchInfo.Length > 0 then Some (batchInfo |> Array.map (fun c -> c.w) |> Array.max) else None
        let maxH = if batchInfo.Length > 0 then Some (batchInfo |> Array.map (fun c -> c.h) |> Array.max) else None

        if section.FlowChart then
            let root = tree.Levels.[level]
            let colors = 
                if batchInfo.Length > 0 then
                    batchInfo.[0].shapes |> Array.map (fun s -> s.color)
                else [||]
            let svg = renderFlowchartSvg root colors flowChartMaxW flowChartMaxH

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
                    let conf = batchInfo[i]
                    let levelCxlNames = 
                        conf.cxCxl1 
                        |> Array.filter (fun c -> 
                            let hasHxls = c.Hxls |> Array.exists (fun h -> let (_, _, z) = Hexel.hxlCrd h in z = level)
                            let (_, _, bz) = Hexel.hxlCrd c.Base
                            hasHxls || bz = level)
                        |> Array.map (fun c -> prpVlu c.Name)
                        |> Set.ofArray
                    
                    let levelShapes = conf.shapes |> Array.filter (fun s -> levelCxlNames.Contains(s.name) && s.points.Length > 0)
                    let svg = renderFloorPlanSvg levelShapes conf.cxOuIl maxW maxH
                    sprintf tBatchCell svg (Page.labelPhrase.[i].ToString()) |> sb.AppendLine |> ignore
                
                let allPageShapes = [| chunkStart .. chunkEnd |] |> Array.collect (fun idx -> batchInfo.[idx].shapes)
                let legendNames = 
                    [| chunkStart .. chunkEnd |] 
                    |> Array.collect (fun idx -> batchInfo.[idx].cxCxl1)
                    |> Array.filter (fun c -> 
                        let hasHxls = c.Hxls |> Array.exists (fun h -> let (_, _, z) = Hexel.hxlCrd h in z = level)
                        let (_, _, bz) = Hexel.hxlCrd c.Base
                        hasHxls || bz = level)
                    |> Array.map (fun c -> prpVlu c.Name)
                    |> Set.ofArray
                let levelShapes = allPageShapes |> Array.filter (fun s -> legendNames.Contains(s.name) && s.points.Length > 0)
                let legend = if levelShapes.Length > 0 then renderLegend levelShapes else ""
                sprintf tBatchGrid2 legend (renderFooter()) |> sb.AppendLine |> ignore

        if section.Variations then
            for i = 0 to Math.Min(23, batchInfo.Length - 1) do
                if section.SelectedVariations.Contains(i) then
                    let conf = batchInfo[i]
                    
                    // Filter Coxels for this level specifically
                    let levelCxls : Cxl[] = 
                        conf.cxCxl1 |> Array.filter (fun (c: Cxl) -> 
                            let hasHxls = c.Hxls |> Array.exists (fun h -> let (_, _, z) = Hexel.hxlCrd h in z = level)
                            let (_, _, bz) = Hexel.hxlCrd c.Base
                            hasHxls || bz = level)

                    let levelNames = levelCxls |> Array.map (fun c -> prpVlu c.Name) |> Set.ofArray
                    let levelShapes = conf.shapes |> Array.filter (fun s -> levelNames.Contains(s.name) && s.points.Length > 0)
                    
                    let svg = renderFloorPlanSvg levelShapes conf.cxOuIl maxW maxH
                    let legend = renderLegend levelShapes
                            
                    let colorMap = 
                        levelShapes 
                        |> Array.map (fun s -> s.name, s.color) 
                        |> Map.ofArray

                    let areaTable = renderAreaTable levelCxls conf.cxlAvl colorMap level
                    let adjMatrix = renderAdjacencyMatrix levelCxls colorMap
                    
                    sprintf tVariation (renderHeader (sprintf "%s — Level %d" (Page.labelPhrase.[i].ToString()) level) "") svg legend areaTable adjMatrix (renderFooter()) |> sb.AppendLine |> ignore

    sb.AppendLine("</body></html>") |> ignore
    sb.ToString()
