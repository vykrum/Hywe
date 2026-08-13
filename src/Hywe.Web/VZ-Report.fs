module Hywe.Report

open Bolero.Html
open ModelTypes
open System
open System.Text
open Hywe.Core
open Hywe.Core.Coxel
open Hywe.Node

// --- DATA TYPES ---

type PageEntry = {
    PageNumber: int
    SectionTitle: string
    Depth: int
}

// --- GENERATOR LOGIC (from UT-ReportGenerator.fs) ---

let getOrderedMarkers (tree: SubModel) =
    tree.Levels.Keys |> Seq.toList |> List.sort |> List.collect (fun lvl ->
        let levelMarker = match lvl with | 0 -> "L0" | _ -> sprintf "L%d" lvl
        let nests = tree.Nests |> Map.toList |> List.filter (fun (_, n) -> n.Level = lvl) |> List.map (fun (id, _) -> sprintf "N%d" id)
        levelMarker :: nests
    )

let getMarkerTitle (marker: string) =
    match marker.StartsWith("N") with
    | true -> sprintf "Nest %s" (marker.Substring(1))
    | false -> sprintf "Level %s" (marker.Substring(1))

let buildPageManifest (opts: ReportOptions) (markers: string list) : PageEntry list =
    let initialPages, initialPageNum = 
        match opts.IncludeCover with
        | true -> [{ PageNumber = 1; SectionTitle = "Cover Page"; Depth = 0 }], 2
        | false -> [], 1

    let foldMarker (pages, pageNum) marker =
        let section = 
            Map.tryFind marker opts.LevelSections 
            |> Option.defaultValue { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
            
        let hasAny = section.FlowChart || section.BatchOverview || section.Variations
        match hasAny with
        | false -> (pages, pageNum)
        | true ->
            let title = getMarkerTitle marker
            let isNest = marker.StartsWith("N")
            let titleDepth, contentDepth = 
                match isNest with
                | true -> 1, 2
                | false -> 0, 1
            
            let p1 = { PageNumber = pageNum; SectionTitle = title; Depth = titleDepth } :: pages
            let n1 = pageNum + 1
            
            let p2, n2 = 
                match section.FlowChart with
                | true -> { PageNumber = n1; SectionTitle = "Flow Chart"; Depth = contentDepth } :: p1, n1 + 1
                | false -> p1, n1
                
            let p3, n3 =
                match section.BatchOverview with
                | true ->
                    let pOut = { PageNumber = n2; SectionTitle = "Batch Overview"; Depth = contentDepth } :: p2
                    let numPages = int (System.Math.Ceiling(24.0 / 8.0))
                    let nOut = n2 + 1 + (match numPages > 1 with | true -> numPages - 1 | false -> 0)
                    pOut, nOut
                | false -> p2, n2
                
            let p4, n4 =
                match section.Variations with
                | true ->
                    [0..23] 
                    |> List.filter (fun i -> section.SelectedVariations.Contains(i))
                    |> List.fold (fun (accP, accN) i ->
                        { PageNumber = accN; SectionTitle = labelPhrase.[i].ToString(); Depth = contentDepth + 1 } :: accP, accN + 1
                    ) (p3, n3)
                | false -> p3, n3
                
            (p4, n4)
            
    let (finalPages, _) = markers |> List.fold foldMarker (initialPages, initialPageNum)
    finalPages |> List.rev

let renderFloorPlanSvg (shapes: BatchComponent[]) (wtmkShapes: BatchComponent[] option) (cxOuIl: (int*int)[][]) (maxW: float option) (maxH: float option) (targetFontSize: float) (containerWidth: float) : string =
    let shapePoints = 
        shapes 
        |> Array.collect (fun s -> 
            Array.init (s.points.Length / 2) (fun i -> s.points.[i*2], s.points.[i*2 + 1])
        )

    let wtmkPoints =
        match wtmkShapes with
        | Some ws -> 
            ws |> Array.collect (fun s -> 
                Array.init (s.points.Length / 2) (fun i -> s.points.[i*2], s.points.[i*2 + 1])
            )
        | None -> [||]
        
    let boundPoints = 
        cxOuIl 
        |> Array.collect id
        |> Array.map (fun (x, y) -> float x, float y)
        
    let allPoints = Array.concat [| shapePoints; wtmkPoints; boundPoints |]
    
    let minX, minY, maxX, maxY =
        match allPoints.Length with
        | 0 -> 0.0, 0.0, 1.0, 1.0
        | _ ->
            let xs = allPoints |> Array.map fst
            let ys = allPoints |> Array.map snd
            Array.min xs, Array.min ys, Array.max xs, Array.max ys

    let wtmkPolygons =
        match wtmkShapes with
        | Some ws ->
            ws |> Array.map (fun shp ->
                let pts = 
                    Array.init (shp.points.Length / 2) (fun i -> sprintf "%f,%f" shp.points.[i*2] shp.points.[i*2+1])
                    |> String.concat " "
                sprintf """<polygon points="%s" fill="#DDDDDD" stroke="#AAAAAA" stroke-width="0.1" opacity="0.3" />""" pts
            ) |> String.concat ""
        | None -> ""

    let polygons = 
        shapes |> Array.map (fun shp ->
            let pts = 
                Array.init (shp.points.Length / 2) (fun i -> sprintf "%f,%f" shp.points.[i*2] shp.points.[i*2+1])
                |> String.concat " "
            sprintf """<polygon points="%s" fill="%s" opacity="0.75" />""" pts shp.color
        ) |> String.concat ""
        
    let boundaries = 
        cxOuIl |> Array.map (fun bdr ->
            let pts = 
                bdr |> Array.map (fun (x,y) -> sprintf "%f,%f" (float x) (float y))
                |> String.concat " "
            sprintf """<polygon points="%s" fill="none" stroke="#000" stroke-width="0.1" opacity="0.1" />""" pts
        ) |> String.concat ""
    
    let pad = 8.0
    let contentW = maxX - minX
    let contentH = maxY - minY
    let w = match maxW with | Some v when v > 0.0 -> v | _ -> contentW
    let h = match maxH with | Some v when v > 0.0 -> v | _ -> contentH
    let ox = (w - contentW) / 2.0
    let oy = (h - contentH) / 2.0
    
    let viewBoxW = w + 2.0 * pad
    let fontSize = targetFontSize * viewBoxW / (containerWidth *1.5)
    
    let labels =
        shapes |> Array.map (fun shp ->
            match System.String.IsNullOrWhiteSpace(shp.name) || shp.points.Length = 0 with
            | true -> ""
            | false ->
                let safeName = shp.name.Replace("<", "&lt;").Replace(">", "&gt;")
                sprintf """<text x="%f" y="%f" text-anchor="middle" dominant-baseline="central" font-size="%f" fill="#111" font-family="Outfit, sans-serif" font-weight="normal" style="pointer-events: none;">%s</text>""" shp.lx shp.ly fontSize safeName
        ) |> String.concat ""
    
    sprintf """<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg" width="100%%" height="100%%">
    <g transform="translate(%f, %f)">
    %s
    %s
    %s
    %s
    </g>
    </svg>""" (minX - pad - ox) (minY - pad - oy) (w + 2.0 * pad) (h + 2.0 * pad) 0.0 0.0 wtmkPolygons polygons boundaries labels

let renderFlowchartSvg (root: TreeNode) (colorMap: Map<string, string>) (maxW: float option) (maxH: float option) : string =
    Visualization.renderSvgToString root colorMap maxW maxH

let renderLegend (shapes: {| color: string; points: float[]; name: string; lx: float; ly: float |}[]) (validNames: Set<string>) : string =
    let uniqueRooms = 
        shapes 
        |> Array.filter (fun s -> validNames.Contains s.name || validNames.Contains (s.name.Trim()))
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
    
    match uniqueRooms.Length with
    | 0 -> ""
    | _ -> sprintf """<div class="legend" style="display: flex; flex-wrap: wrap; gap: 12px; padding: 4px 10px; background: #fafafa; border-radius: 4px; margin-top: 20px; margin-bottom: 10px;">%s</div>""" items
    
let renderAreaTable (cxls: Cxl[]) (cxlAvl: int[]) (colorMap: Map<string, string>) (elv: int) : string =
    let fontSize = 
        match cxls.Length with
        | l when l > 25 -> "7.5px"
        | l when l > 15 -> "8.5px"
        | _ -> "9.5px"
    let header = sprintf "<table class=\"report-table\" style=\"font-size: %s;\">\n        <thead>\n            <tr><th>Label</th><th>Required</th><th>Achieved</th><th>Open</th></tr>\n        </thead>\n        <tbody>\n" fontSize
        
    let hxlAreaX = 4
    let rows = 
        cxls |> Array.mapi (fun i cxl ->
            let avl = match i < cxlAvl.Length with | true -> cxlAvl[i] | false -> 0
            let isRootLvl0 = (prpVlu cxl.Rfid = "1" || prpVlu cxl.Name = "Root") && elv = 0
            let count = match isRootLvl0 with | true -> (prpVlu cxl.Size |> float) + 1.0 | false -> (prpVlu cxl.Size |> float)
            let reqSz = int (count * float hxlAreaX)
            let achSz = (Array.length cxl.Hxls) * hxlAreaX
            let opnSz = avl * hxlAreaX
            let rfid = prpVlu cxl.Rfid
            let rawName = prpVlu cxl.Name
            let safeName = rawName.Replace("<", "&lt;").Replace(">", "&gt;")
            
            let clr = Map.tryFind rfid colorMap |> Option.defaultValue "#eee"
            let swatch = sprintf """<div style="width: 100%%; max-width: 12px; aspect-ratio: 1/1; background: %s; border: 1px solid #ccc; display: inline-block; border-radius: 2px; box-sizing: border-box; vertical-align: middle;"></div>""" clr
            
            sprintf "<tr><td title=\"%s\" style=\"white-space: nowrap;\">%s<span style=\"margin-left: 6px; vertical-align: middle;\">%s</span></td><td>%d</td><td>%d</td><td>%d</td></tr>\n" safeName swatch safeName reqSz achSz opnSz
        ) |> String.concat ""
        
    header + rows + "</tbody></table>\n"

let renderAdjacencyMatrix (cxls: Cxl[]) (colorMap: Map<string, string>) : string =
    let names, matrix = Coxel.cxlAdj cxls
    let rfids = cxls |> Array.map (fun c -> prpVlu c.Rfid)
    let fontSize = 
        match names.Length with
        | l when l > 25 -> "6px"
        | l when l > 15 -> "7.5px"
        | _ -> "9px"
    let headerStart = sprintf "<table class=\"report-table adjacency-matrix\" style=\"font-size: %s; height: 100%%;\">\n        <thead>\n            <tr><th></th>\n" fontSize
    
    let headerCols = 
        names |> Array.mapi (fun i name ->
            let safeName = name.Replace("<", "&lt;").Replace(">", "&gt;")
            let rfid = rfids.[i]
            let clr = Map.tryFind rfid colorMap |> Option.defaultValue "#eee"
            let swatch = sprintf """<div style="width: 100%%; aspect-ratio: 1/1; background: %s; border: 1px solid #ccc; display: block; border-radius: 2px; box-sizing: border-box; margin: auto;"></div>""" clr
            sprintf """<th class="adj-header" title="%s">%s</th>""" safeName swatch
        ) |> String.concat ""
        
    let theadEnd = "</tr></thead><tbody>\n"
    
    let rows = 
        matrix |> Array.mapi (fun i row ->
            let name = names.[i]
            let rfid = rfids.[i]
            let safeName = name.Replace("<", "&lt;").Replace(">", "&gt;")
            let clr = Map.tryFind rfid colorMap |> Option.defaultValue "#eee"
            let swatch = sprintf """<div style="width: 100%%; aspect-ratio: 1/1; background: %s; border: 1px solid #ccc; display: block; border-radius: 2px; box-sizing: border-box; margin: auto;"></div>""" clr
            let rowHeader = sprintf """<tr><th title="%s" style="text-align:center;">%s</th>""" safeName swatch
            let rowCells = 
                row |> Array.map (fun adj ->
                    let cls = match adj with | true -> "adj-true" | false -> "adj-false"
                    sprintf """<td class="%s"></td>""" cls
                ) |> String.concat ""
            rowHeader + rowCells + "</tr>\n"
        ) |> String.concat ""
        
    headerStart + headerCols + theadEnd + rows + "</tbody></table>\n"

// --- HTML TEMPLATES ---

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
body { font-family: 'Outfit', system-ui, -apple-system, sans-serif; margin: 0; padding: 0; background: #fff; color: #333; font-size: 14px; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
.page { width: 420mm; height: 297mm; page-break-after: always; position: relative; box-sizing: border-box; overflow: hidden; }
.page-inner { padding: 8mm 15mm 25mm 15mm; height: 100%%; box-sizing: border-box; display: flex; flex-direction: column; }
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
.col-right { flex: 1.5; display: flex; flex-direction: column; min-width: 0; container-type: inline-size; }
.report-table { width: 100%%; border-collapse: collapse; font-size: 9.5px; margin-bottom: 10px; }
.report-table tr { break-inside: avoid; page-break-inside: avoid; }
.report-table th, .report-table td { padding: 4px 6px; border-bottom: 1px solid #eee; text-align: left; }
.report-table th { background: #fafafa; font-weight: 600; color: #555; }
.adjacency-matrix { table-layout: fixed; width: 100%%; height: 100%%; margin: 0; border-collapse: collapse; font-size: 0; line-height: 0; }
.adjacency-matrix tr { height: 1%%; }
.adjacency-matrix th, .adjacency-matrix td { padding: 0; border: 1px solid #eee; text-align: center; vertical-align: middle; }
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
        <div class="col-right" style="display: flex; flex-direction: column; gap: 15px;">
            <div style="flex: 1; min-height: 0; column-width: 300px; column-gap: 15px; column-fill: auto; direction: rtl; text-align: left; align-self: flex-end;">
                <div style="direction: ltr;">%s</div>
            </div>
            <div style="width: 100%%; max-width: 300px; margin-top: auto; margin-left: auto;">
                <div style="font-weight: 500; font-size: 10px; margin-bottom: 5px; text-transform: uppercase; letter-spacing: 1px; color: #555; text-align: left; border-bottom: 1px solid #eee; padding-bottom: 2px;">Adjacency Matrix</div>
                <div style="width: 100%%; aspect-ratio: 1/1; overflow: hidden;">%s</div>
            </div>
        </div>
    </div>
    %s
</div></div>"""

let generateReportHtml (opts: ReportOptions) (tree: SubModel) (batches: Map<string, BatchConfgrtns[]>) : string =
    let d = DateTime.Now.ToString("dd MMM yyyy")
    let baseHtml = sprintf tBase opts.ProjectTitle
    let markers = getOrderedMarkers tree
    
    let renderHeader title subtitle = sprintf tHeader title subtitle
    let renderFooter page = sprintf tFooter d page

    let coverHtml, coverPage = 
        match opts.IncludeCover with
        | true ->
            let captureHtml = 
                match opts.Captured3DImage with
                | Some url -> sprintf """<img src="%s" style="width: 100%%; height: 100%%; object-fit: contain;" />""" url
                | None -> ""
            [sprintf tCover opts.ProjectTitle opts.ProjectNumber opts.Description opts.Author opts.ClientName d captureHtml (renderFooter 1)], 2
        | false -> [], 1

    let flowChartMaxW, flowChartMaxH =
        let allTrees = 
            markers |> List.choose (fun marker ->
                let s = Map.tryFind marker opts.LevelSections |> Option.defaultValue { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
                match s.FlowChart with
                | true -> 
                    match marker.StartsWith("N") with
                    | true ->
                        let nId = match Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
                        Map.tryFind nId tree.Nests
                    | false ->
                        let lvl = match Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0
                        Map.tryFind lvl tree.Levels
                | false -> None
            )
        match allTrees.IsEmpty with
        | true -> None, None
        | false ->
            let bounds = allTrees |> List.map Visualization.calculateTreeBounds
            let mw = bounds |> List.map fst |> List.max
            let mh = bounds |> List.map snd |> List.max
            Some mw, Some mh

    let generateMarkerHtml (accHtml: string list, currentPage: int) marker =
        let section = 
            match Map.tryFind marker opts.LevelSections with
            | Some s -> s
            | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
        let batchInfo = batches.[marker]
        let maxW = match batchInfo.Length > 0 with | true -> Some (batchInfo |> Array.map (fun c -> c.w) |> Array.max) | false -> None
        let maxH = match batchInfo.Length > 0 with | true -> Some (batchInfo |> Array.map (fun c -> c.h) |> Array.max) | false -> None
        let title = getMarkerTitle marker

        let html1, page1 = 
            match section.FlowChart with
            | true ->
                let root = 
                    match marker.StartsWith("N") with
                    | true ->
                        let nId = match Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
                        Map.tryFind nId tree.Nests |> Option.defaultValue tree.Levels.[0]
                    | false ->
                        let lvl = match Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0
                        Map.tryFind lvl tree.Levels |> Option.defaultValue tree.Levels.[0]
                let colorMap = 
                    match batchInfo.Length > 0 with 
                    | true -> 
                        batchInfo 
                        |> Array.collect (fun c -> c.shapes)
                        |> Array.map (fun s -> s.name, s.color)
                        |> Map.ofArray 
                    | false -> Map.empty
                let svg = renderFlowchartSvg root colorMap flowChartMaxW flowChartMaxH
                let res = sprintf tFlowChart (renderHeader (sprintf "Flow Chart — %s" title) opts.ProjectTitle) svg (renderFooter currentPage)
                res :: accHtml, currentPage + 1
            | false -> accHtml, currentPage

        let html2, page2 = 
            match section.BatchOverview && batchInfo.Length > 0 with
            | true ->
                let limit = Math.Min(23, batchInfo.Length - 1)
                let totalPages = int (Math.Ceiling(float (limit + 1) / 8.0))
                
                let chunkHtmls, nextPg = 
                    [0 .. totalPages - 1] 
                    |> List.fold (fun (acc, pg) pageIndex ->
                        let chunkStart = pageIndex * 8
                        let chunkEnd = Math.Min(chunkStart + 7, limit)
                        let pageStr = match totalPages > 1 with | true -> sprintf " (%d/%d)" (pageIndex+1) totalPages | false -> ""
                        let grid1 = sprintf tBatchGrid1 (renderHeader (sprintf "Batch Overview — %s%s" title pageStr) opts.ProjectTitle)
                        
                        let cells = 
                            [chunkStart .. chunkEnd] |> List.map (fun i ->
                                let conf = batchInfo.[i]
                                let svg = renderFloorPlanSvg conf.shapes conf.wtmkShapes conf.cxOuIl maxW maxH 7.5 200.0
                                sprintf tBatchCell svg (labelPhrase.[i].ToString())
                            ) |> String.concat ""
                            
                        let grid2 = sprintf tBatchGrid2 "" (renderFooter pg)
                        (grid1 + cells + grid2) :: acc, pg + 1
                    ) (html1, page1)
                    
                chunkHtmls, nextPg
            | false -> html1, page1

        let html3, page3 = 
            match section.Variations with
            | true ->
                let limit = Math.Min(23, batchInfo.Length - 1)
                [0 .. limit] 
                |> List.filter (fun i -> section.SelectedVariations.Contains(i))
                |> List.fold (fun (acc, pg) i ->
                    let conf = batchInfo.[i]
                    let levelCxls = conf.cxCxl1
                    let levelShapes = conf.shapes
                    let svg = renderFloorPlanSvg levelShapes conf.wtmkShapes conf.cxOuIl maxW maxH 7.5 550.0
                    let cxlColorMap = Array.zip conf.cxCxl1 conf.cxClr1 |> Array.map (fun (c, clr) -> prpVlu c.Rfid, clr) |> Map.ofArray
                    
                    let baseLevel = 
                        match marker.StartsWith("N") with
                        | true ->
                            let nestId = match Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 1
                            match tree.Nests |> Map.tryFind nestId with | Some n -> n.Level | None -> 0
                        | false ->
                            match Int32.TryParse(marker.Substring(1)) with true, v -> v | _ -> 0
                            
                    let areaTable = renderAreaTable levelCxls conf.cxlAvl cxlColorMap baseLevel
                    let adjMatrix = renderAdjacencyMatrix levelCxls cxlColorMap
                    let varHtml = sprintf tVariation (renderHeader (sprintf "%s — %s" (labelPhrase.[i].ToString()) title) "") svg "" areaTable adjMatrix (renderFooter pg)
                    varHtml :: acc, pg + 1
                ) (html2, page2)
            | false -> html2, page2

        html3, page3

    let markerHtmls, _ = markers |> List.fold generateMarkerHtml (coverHtml, coverPage)
    
    let allHtmls = markerHtmls |> List.rev |> String.concat "\n"
    baseHtml + "\n" + allHtmls + "\n</body></html>\n"

// --- UI COMPONENTS ---

let viewReport (model: Model) dispatch =
    let opts = model.ReportOptions
    let updateOpts f = dispatch (UpdateReportOptions f)
    
    let renderToggleRow textLabel isChecked onChange =
        elt "label" {
            attr.``class`` "report-toggle-row"
            input {
                attr.``type`` "checkbox"
                attr.``checked`` isChecked
                on.change (fun _ -> onChange (not isChecked))
            }
            text textLabel
        }

    div {
        attr.``class`` "u-flex-col u-items-center u-gap-xl u-p-lg u-w-full u-max-w-800 fade-in"
        
        div {
            attr.``class`` "teach-intro-section"
            h2 { attr.``class`` "teach-intro-title"; text "Report Generation" }
            p { 
                attr.``class`` "teach-intro-text"
                text "Consolidates all generated configurations in a single compilation."
            }
        }
        
        div {
            attr.``class`` "report-section-title"
            text "1. Project Details"
        }
        
        div {
            attr.style "width: 100%; max-width: 800px; display: flex; flex-direction: column; gap: 8px;"
            div {
                attr.style "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;"
                div {
                    attr.style "display: flex; flex-direction: column; gap: 8px;"
                    div {
                        attr.``class`` "report-field"
                        elt "label" { text "Project Title" }
                        input {
                            attr.``class`` "hywe-input"
                            attr.value opts.ProjectTitle
                            on.input (fun e -> updateOpts (fun o -> { o with ProjectTitle = e.Value :?> string }))
                        }
                    }
                    div {
                        attr.``class`` "report-field"
                        elt "label" { text "Project Number" }
                        input {
                            attr.``class`` "hywe-input"
                            attr.value opts.ProjectNumber
                            on.input (fun e -> updateOpts (fun o -> { o with ProjectNumber = e.Value :?> string }))
                        }
                    }
                }
                div {
                    attr.style "display: flex; flex-direction: column; gap: 8px;"
                    div {
                        attr.``class`` "report-field"
                        elt "label" { text "Author" }
                        input {
                            attr.``class`` "hywe-input"
                            attr.value opts.Author
                            on.input (fun e -> updateOpts (fun o -> { o with Author = e.Value :?> string }))
                        }
                    }
                    div {
                        attr.``class`` "report-field"
                        elt "label" { text "Client Name" }
                        input {
                            attr.``class`` "hywe-input"
                            attr.value opts.ClientName
                            on.input (fun e -> updateOpts (fun o -> { o with ClientName = e.Value :?> string }))
                        }
                    }
                }
            }
            div {
                attr.``class`` "report-field"
                elt "label" { text "Description" }
                textarea {
                    attr.``class`` "hywe-input"
                    attr.rows 2
                    attr.value opts.Description
                    on.input (fun e -> updateOpts (fun o -> { o with Description = e.Value :?> string }))
                }
            }
        }
        
        div {
            attr.style "margin-top: 10px;"
            attr.``class`` "report-section-title"
            text "2. Project Content"
        }
        
        renderToggleRow "Cover Page" opts.IncludeCover (fun v -> updateOpts (fun o -> { o with IncludeCover = v }))
        
        if opts.IncludeCover then
            let color, msg = 
                if model.ViewLocked then 
                    "#4caf50", "(3D view locked: will be included in cover page)"
                else 
                    "#e65100", "Please lock 3D view for inclusion in cover page"

            div {
                attr.style (sprintf "font-size: 11px; color: %s; margin: -5px 0 10px 25px; font-style: italic;" color)
                text msg
            }

        forEach (getOrderedMarkers model.Tree) <| fun marker ->
            let isNest = marker.StartsWith("N")
            let s = match Map.tryFind marker opts.LevelSections with | Some sections -> sections | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23]; IsFilterExpanded = false }
            div {
                attr.``class`` "report-level-card"
                attr.style (if isNest then "margin-left: 40px;" else "")
                div { attr.``class`` "report-level-header"; text (getMarkerTitle marker) }
                div {
                    attr.style "display: flex; gap: 40px; align-items: flex-start; flex-wrap: wrap; margin-bottom: 5px;"
                    div {
                        attr.style "display: flex; flex-direction: column;"
                        renderToggleRow "Flow Chart" s.FlowChart (fun v -> updateOpts (fun o -> { o with LevelSections = Map.add marker { s with FlowChart = v } o.LevelSections }))
                        renderToggleRow "Batch Overview (Grid)" s.BatchOverview (fun v -> updateOpts (fun o -> { o with LevelSections = Map.add marker { s with BatchOverview = v } o.LevelSections }))
                    }
                    div {
                        attr.style "display: flex; flex-direction: column; align-items: flex-start; gap: 8px;"
                        renderToggleRow "Individual Variations" s.Variations (fun v -> updateOpts (fun o -> { o with LevelSections = Map.add marker { s with Variations = v } o.LevelSections }))
                        button {
                            attr.disabled (not s.Variations)
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-light"
                            attr.style (if s.Variations then "margin-left: 24px; padding: 4px 10px; font-size: 11px; font-weight: normal; text-transform: none; color: #666; background: transparent; border: 1px solid #ddd;" else "margin-left: 24px; padding: 4px 10px; font-size: 11px; font-weight: normal; text-transform: none; color: #aaa; background: transparent; border: 1px solid #eee; cursor: not-allowed;")
                            on.click (fun _ -> if s.Variations then updateOpts (fun o -> { o with LevelSections = Map.add marker { s with IsFilterExpanded = not s.IsFilterExpanded } o.LevelSections }))
                            text (if s.Variations && s.IsFilterExpanded then "Hide filters" else "Filter variations")
                        }
                    }
                }
                if s.Variations && s.IsFilterExpanded then
                    concat {
                        div {
                            attr.``class`` "variation-grid-controls"
                            button {
                                attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-light report-mini-btn"
                                on.click (fun _ -> updateOpts (fun o -> { o with LevelSections = Map.add marker { s with SelectedVariations = Set.ofList [0..23] } o.LevelSections }))
                                text "All"
                            }
                            button {
                                attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-light report-mini-btn"
                                on.click (fun _ -> updateOpts (fun o -> { o with LevelSections = Map.add marker { s with SelectedVariations = Set.empty } o.LevelSections }))
                                text "None"
                            }
                        }
                        div {
                            attr.``class`` "variation-selection-grid"
                            forEach [0..23] <| fun i ->
                                let isSelected = s.SelectedVariations.Contains(i)
                                div {
                                    attr.``class`` (if isSelected then "var-chip selected" else "var-chip")
                                    on.click (fun _ ->
                                        let newSet = if isSelected then Set.remove i s.SelectedVariations else Set.add i s.SelectedVariations
                                        updateOpts (fun o -> { o with LevelSections = Map.add marker { s with SelectedVariations = newSet } o.LevelSections }))
                                    text (labelPhrase.[i].ToString())
                                }
                        }
                    }
            }

        div {
            attr.style "margin-top: 10px;"
            attr.``class`` "report-section-title"
            text "3. Generate"
        }
        
        let reportPages = buildPageManifest opts (getOrderedMarkers model.Tree)
        div {
            attr.``class`` "report-page-count"
            text (sprintf "Report ready — %d pages" reportPages.Length)
        }
        
        button {
            attr.``class`` "hywe-btn hywe-btn-dark hywe-btn-lg u-w-full u-max-w-800 u-mt-md"
            attr.disabled model.IsGeneratingReport
            on.click (fun _ -> dispatch GenerateReport)
            text (if model.IsGeneratingReport then "Generating..." else "Generate Report (PDF)")
        }
        
        if model.IsGeneratingReport then
            div {
                attr.``class`` "report-status"
                span { attr.``class`` "report-spinner" }
                text "Processing layouts and compiling report..."
            }
    }
