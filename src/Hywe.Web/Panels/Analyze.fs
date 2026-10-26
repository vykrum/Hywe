module Analyze

open Hywe.Core.Coxel
open Bolero.Html
open ModelTypes

// --- HELPERS ---

let getRoomAspect (cxl: Cxl) =
    if Array.isEmpty cxl.Hxls then 1.0
    else
        let xs = cxl.Hxls |> Array.map (fun h -> let x, _, _ = Hywe.Core.Hexel.hxlCrd h in float x)
        let ys = cxl.Hxls |> Array.map (fun h -> let _, y, _ = Hywe.Core.Hexel.hxlCrd h in float y)
        let dx = (Array.max xs) - (Array.min xs) + 1.0
        let dy = (Array.max ys) - (Array.min ys) + 1.0
        max dx dy / max 1.0 (min dx dy)

let getAspectLabel aspect =
    if aspect <= 1.35 then "Compact", "#ecfdf5", "#047857"
    elif aspect <= 1.9 then "Proportionate", "#f1f5f9", "#334155"
    else "Elongated", "#fff7ed", "#c2410c"

let getOrientationLabel (solarVal: float option) (avl: int) =
    if avl <= 0 then "Internal", "#f2f2f2", "#888888"
    else
        match solarVal with
        | None -> "Exterior", "#f8fafc", "#475569"
        | Some s when s <= 0.0 -> "Internal", "#f2f2f2", "#888888"
        | Some s when s < 160.0 -> "North", "#e0f2fe", "#0369a1"
        | Some s when s < 235.0 -> "East / West", "#fef3c7", "#b45309"
        | _ -> "South", "#fee2e2", "#b91c1c"

let computeSummaryMetrics (cxCxl1: Cxl[]) (cxlAvl: int[]) =
    let allHxls = cxCxl1 |> Array.collect (fun c -> Array.append [| c.Base |] c.Hxls)
    let occSet = Hywe.Core.Hexel.hxlSet allHxls
    let totalHexels = allHxls.Length
    
    let totalPerimeter = 
        allHxls 
        |> Array.sumBy (fun h -> 
            let seqn = if cxCxl1.Length > 0 then cxCxl1.[0].Seqn else Hywe.Core.Hexel.VRCCNE
            Hywe.Core.Hexel.adjacent seqn h 
            |> Array.filter (occSet.Contains >> not) 
            |> Array.length)
    
    let compactnessCI = 
        match totalPerimeter with
        | 0 -> 0.0
        | p -> (4.0 * System.Math.PI * float totalHexels) / float (p * p)
    
    let typology, typoDesc =
        if compactnessCI >= 0.67 then "Centripetal Cluster", "Concentric layout with central common core"
        elif compactnessCI >= 0.62 then "Biaxial / Zoned", "Balanced multi-branch grouping"
        elif compactnessCI >= 0.55 then "Dendritic Spine", "Directional layout with distinct wings"
        else "Linear Pavilion", "Extroverted layout with high exterior exposure"
        
    let compactnessBadge, compBg, compClr =
        if compactnessCI >= 0.67 then "Compact", "#ecfdf5", "#047857"
        elif compactnessCI >= 0.60 then "Balanced", "#f1f5f9", "#334155"
        else "Sprawled", "#fff7ed", "#c2410c"
        
    let exposedCount = cxlAvl |> Array.filter (fun a -> a > 0) |> Array.length
    let exposedPct = 
        if cxCxl1.Length > 0 then (float exposedCount / float cxCxl1.Length) * 100.0 else 0.0
        
    let maxDepth = 
        if cxCxl1.Length = 0 then 1
        else
            cxCxl1 
            |> Array.map (fun c -> (prpVlu c.Rfid).Split('.').Length) 
            |> Array.max
            
    {| Typology = typology
       TypoDesc = typoDesc
       CompactnessBadge = compactnessBadge
       CompactnessScore = compactnessCI
       CompBg = compBg
       CompClr = compClr
       ExposedPct = exposedPct
       ExposedCount = exposedCount
       TotalRooms = cxCxl1.Length
       MaxDepth = maxDepth |}

let renderKpiCard (title: string) (value: string) (badgeBg: string) (badgeClr: string) (subtext: string) =
    div {
        attr.``style`` "flex: 1 1 calc(25% - 10px); min-width: 170px; background: #ffffff; border: 1px solid #eee; border-radius: 6px; padding: 12px 14px; box-sizing: border-box;"
        div {
            attr.``style`` "font-size: 10px; color: #888; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 500; margin-bottom: 6px;"
            text title
        }
        div {
            attr.``style`` "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;"
            span {
                attr.``style`` $"display: inline-block; padding: 3px 8px; border-radius: 4px; font-weight: 600; font-size: 13px; background: {badgeBg}; color: {badgeClr};"
                text value
            }
        }
        div {
            attr.``style`` "font-size: 11px; color: #666; line-height: 1.3;"
            text subtext
        }
    }

// --- ROW RENDERERS ---

let renderAreaRow (cxl: Cxl) (clr: string) (ratio: float) (elv: int) =
    let isRootLvl0 = (prpVlu cxl.Rfid = "1" || prpVlu cxl.Name = "Root") && elv = 0
    let count = if isRootLvl0 then (prpVlu cxl.Size |> float) + 1.0 else (prpVlu cxl.Size |> float)
    let reqSz = count
    let achSz = float (Array.length cxl.Hxls)
    let achCl = match achSz < (reqSz * 0.99) with | true -> "red" | false -> "#646464"
    let aspect = getRoomAspect cxl
    let aspLabel, aspBg, aspClr = getAspectLabel aspect

    tr {
        attr.``style`` $"background-color:{clr}; height: 32px;"
        td { 
            attr.width "15%"
            attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
            text (prpVlu cxl.Rfid) 
        }
        td { 
            attr.width "35%"
            attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
            text (prpVlu cxl.Name) 
        }
        td { 
            attr.width "15%"
            attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
            text (sprintf "%.0f" reqSz) 
        }
        td { 
            attr.width "15%"
            attr.``style`` $"padding: 8px; text-align: center; color:{achCl}; border: 1px solid #eee;"
            text (sprintf "%.0f" achSz) 
        }
        td {
            attr.width "20%"
            attr.``style`` "padding: 6px 4px; text-align: center; border: 1px solid #eee; vertical-align: middle;"
            attr.title (sprintf "Proportion aspect: %.2f" aspect)
            div {
                attr.``style`` $"display: inline-block; padding: 2px 7px; border-radius: 4px; font-weight: 500; font-size: 11px; background: {aspBg}; color: {aspClr};"
                text aspLabel
            }
        }
    }

let renderSolarRow (cxl: Cxl) (clr: string) (avl: int) (solarVal: float option) =
    let openCount = float avl
    let ori, oriBg, oriClr = getOrientationLabel solarVal avl

    tr {
        attr.``style`` $"background-color:{clr}; height: 32px;"
        td { 
            attr.width "15%"
            attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
            text (prpVlu cxl.Rfid) 
        }
        td { 
            attr.width "30%"
            attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
            text (prpVlu cxl.Name) 
        }
        td { 
            attr.width "15%"
            attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
            if avl <= 0 then
                span {
                    attr.``style`` "color: red; font-size: 10.5px; font-weight: 500;"
                    text "0 (Landlocked)"
                }
            else
                text (sprintf "%.0f" openCount) 
        }
        td {
            attr.width "20%"
            attr.``style`` "padding: 6px 4px; text-align: center; border: 1px solid #eee; vertical-align: middle;"
            div {
                attr.``style`` $"display: inline-block; padding: 2px 6px; border-radius: 4px; font-size: 11px; background: {oriBg}; color: {oriClr};"
                text ori
            }
        }
        td {
            attr.width "20%"
            attr.``style`` "padding: 6px 4px; text-align: center; border: 1px solid #eee; vertical-align: middle;"
            match solarVal with
            | Some s -> attr.title (sprintf "Solar exposure rating: %.0f" s)
            | None -> attr.empty()

            match solarVal with
            | Some s ->
                let tier, bgClr, textClr = 
                    match s with
                    | v when v <= 0.0 -> "None", "#f2f2f2", "#888888"
                    | v when v < 160.0 -> "Low", "#e0f2fe", "#0369a1"
                    | v when v < 235.0 -> "Moderate", "#fef3c7", "#b45309"
                    | _ -> "High", "#fee2e2", "#b91c1c"
                
                div {
                    attr.``style`` $"display: inline-block; padding: 2px 7px; border-radius: 4px; font-weight: 500; font-size: 11px; line-height: 1.2; background: {bgClr}; color: {textClr};"
                    text tier
                }
                div {
                    attr.``style`` "font-size: 9.5px; color: #888; margin-top: 2px; font-family: var(--font-mono, monospace);"
                    text (sprintf "%.0f" s)
                }
            | None -> 
                text "-"
        }
    }

let renderAdjacencyCell isAdj color =
    match isAdj with
    | true ->
        td {
            attr.``style`` $"border: 1px solid #eee; padding: 0; background: {color}; height: 12px;"
            text ""
        }
    | false ->
        td {
            attr.``style`` "border: 1px solid #eee; padding: 0; background: #fafafa; height: 12px;"
            text ""
        }

let viewAdjacencyTable (sqnName: string) (names: string[]) (colors: string[]) (matrix: bool[][]) =
    match names.Length with
    | 0 -> div {}
    | _ ->
        div {
            attr.``style`` "width: 100%; box-sizing: border-box;"
            h3 { 
                attr.``style`` "font-size: 14px; color: #444; margin-bottom: 20px; border-left: 4px solid #888; padding-left: 10px; font-family: 'Outfit', system-ui, sans-serif; display: flex; justify-content: space-between; align-items: center;"
                text "ADJACENCY MATRIX" 
                span {
                    attr.``style`` "font-size: 10px; color: #aaa; font-weight: normal; margin-left: 10px; letter-spacing: 1px;"
                    text $"[{sqnName}]"
                }
            }
            div {
                attr.``style`` "overflow-x: auto;"
                table {
                    attr.``style`` "table-layout: fixed; width: 100%; border-collapse: collapse; font-size: 11px; color: #646464; font-family: 'Outfit', system-ui, sans-serif; opacity: 0.85;"
                    thead {
                        tr {
                            attr.``style`` "height: 32px;"
                            th { 
                                attr.``style`` "border: 1px solid #eee; padding: 4px; background: #fdfdfd; width: 80px;"
                                text "" 
                            }
                            for i in 0 .. colors.Length - 1 do
                                let c = colors.[i]
                                th { 
                                    attr.``style`` $"border: 1px solid #eee; padding: 0; background: {c}; color: #333; height: 12px;"
                                    text "" 
                                }
                        }
                    }
                    tbody {
                        for i in 0 .. matrix.Length - 1 do
                            let row = matrix.[i]
                            tr {
                                attr.``style`` "height: 32px;"
                                td { 
                                    attr.``style`` $"border: 1px solid #eee; padding: 4px 8px; background: {colors.[i]}; color: #333; text-align: left; font-size: 10px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"
                                    text names.[i] 
                                }
                                for j in 0 .. row.Length - 1 do
                                    renderAdjacencyCell row.[j] colors.[j]
                            }
                    }
                }
            }
        }

let viewHyweAnalyze (dispatch: Message -> unit) (sqn: string) (cxCxl1: Cxl[]) (cxClr1: string[]) (cxlAvl: int[]) (solarVals: float[] option) (cxAdj1: string[] * bool[][]) (ratio: float) (elv: int) (isCoordsVisible: bool) (useMapBase: bool) =
    let totalReq = cxCxl1 |> Array.sumBy (fun c -> 
        let isRootLvl0 = (prpVlu c.Rfid = "1" || prpVlu c.Name = "Root") && elv = 0
        let count = if isRootLvl0 then (prpVlu c.Size |> float) + 1.0 else (prpVlu c.Size |> float)
        count)
    let totalAch = cxCxl1 |> Array.sumBy (fun c -> float (Array.length c.Hxls))
    let totalOpen = cxlAvl |> Array.sumBy float
    
    let adjNames, adjMatrix = cxAdj1
    let kpis = computeSummaryMetrics cxCxl1 cxlAvl
    let exposedPctStr = sprintf "%.0f%%" kpis.ExposedPct
    let exposedDescStr = sprintf "%d of %d spaces with perimeter contact" kpis.ExposedCount kpis.TotalRooms
    let depthStr = sprintf "%d Tiers" kpis.MaxDepth

    div {
        attr.``style`` "width: 100%; padding: 10px; box-sizing: border-box; display: flex; flex-direction: column; gap: 20px;"

        // --- TOP QUALITATIVE ARCHITECTURAL KPIS ---
        div {
            attr.``style`` "width: 100%; display: flex; flex-wrap: wrap; gap: 12px;"
            renderKpiCard "Typology" kpis.Typology "#f1f5f9" "#0f172a" kpis.TypoDesc
            renderKpiCard "Plan Compactness" kpis.CompactnessBadge kpis.CompBg kpis.CompClr "Envelope efficiency & form factor"
            renderKpiCard "Exterior Access" exposedPctStr "#ecfdf5" "#047857" exposedDescStr
            renderKpiCard "Circulation Hierarchy" depthStr "#f8fafc" "#475569" "Spatial depth from arrival to private core"
        }

        // --- MAIN ANALYTICS TABLES ---
        div {
            attr.``style`` "width: 100%; display: flex; flex-wrap: wrap; gap: 30px; align-items: flex-start;"
            
            // 1. AREA METRICS (Pure Program Allocation)
            div {
                attr.``style`` "flex: 1 1 calc(50% - 15px); min-width: 250px; max-width: 100%;"
                h3 { 
                    attr.``style`` "font-size: 14px; color: #444; margin-bottom: 15px; border-left: 4px solid #888; padding-left: 10px; font-family: 'Outfit', system-ui, sans-serif; display: flex; justify-content: space-between; align-items: center;"
                    text "AREA METRICS" 
                    span {
                        attr.``style`` "font-size: 10px; color: #aaa; font-weight: normal; margin-left: 10px; letter-spacing: 1px;"
                        text $"[{sqn}]"
                    }
                }

                div {
                    attr.``style`` "overflow-x: auto;"
                    table {
                        attr.``style`` "width: 100%; border-collapse: collapse; font-size: 11px; color: #646464; font-family: 'Outfit', system-ui, sans-serif; opacity: 0.85;"
                        thead {
                            tr {
                                attr.``style`` "height: 32px;"
                                th { attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"; text "Index" }
                                th { attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"; text "Label" }
                                th { attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"; text "Required" }
                                th { attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"; text "Achieved" }
                                th { 
                                    attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd; cursor: help;"
                                    attr.title "Geometric room proportion derived from layout bounding aspect"
                                    text "Proportion" 
                                }
                            }
                        }
                        tbody {
                            for i in 0 .. cxCxl1.Length - 1 do
                                renderAreaRow cxCxl1.[i] cxClr1.[i] ratio elv
                        }
                        tfoot {
                            tr {
                                attr.``style`` "font-weight: bold; border-top: 2px solid #ddd; height: 32px;"
                                td { attr.colspan 2; attr.``style`` "border: 1px solid #eee; text-align: center; font-size: 10.5px; color: #888;"; text "Total" }
                                td { 
                                    attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
                                    text (sprintf "%.0f" totalReq) 
                                }
                                td { 
                                    attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
                                    text (sprintf "%.0f" totalAch) 
                                }
                                td { attr.``style`` "border: 1px solid #eee;" }
                            }
                        }
                    }
                }
            }

            // 2. ADJACENCY MATRIX
            div {
                attr.``style`` "flex: 1 1 calc(50% - 15px); min-width: 250px; max-width: 100%;"
                viewAdjacencyTable sqn adjNames cxClr1 adjMatrix
            }

            // 3. SOLAR ANALYSIS (Active when Map Base is enabled or solar data available)
            if useMapBase then
                div {
                    attr.``style`` "flex: 1 1 calc(50% - 15px); min-width: 250px; max-width: 100%;"
                    h3 { 
                        attr.``style`` "font-size: 14px; color: #444; margin-bottom: 15px; border-left: 4px solid #f59e0b; padding-left: 10px; font-family: 'Outfit', system-ui, sans-serif; display: flex; justify-content: space-between; align-items: center;"
                        text "SOLAR ANALYSIS" 
                        span {
                            attr.``style`` "font-size: 10px; color: #aaa; font-weight: normal; margin-left: 10px; letter-spacing: 1px;"
                            text $"[{sqn}]"
                        }
                    }

                    div {
                        attr.``style`` "overflow-x: auto;"
                        table {
                            attr.``style`` "width: 100%; border-collapse: collapse; font-size: 11px; color: #646464; font-family: 'Outfit', system-ui, sans-serif; opacity: 0.85;"
                            thead {
                                tr {
                                    attr.``style`` "height: 32px;"
                                    th { attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"; text "Index" }
                                    th { attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"; text "Label" }
                                    th { 
                                        attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd; cursor: help;"
                                        attr.title "Exterior boundary contact count"
                                        text "Open" 
                                    }
                                    th { 
                                        attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd; cursor: help;"
                                        attr.title "Primary outward-facing compass orientation"
                                        text "Orientation" 
                                    }
                                    th { 
                                        attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd; cursor: help;"
                                        attr.title "Relative solar exposure rating"
                                        text "Solar" 
                                    }
                                }
                            }
                            tbody {
                                for i in 0 .. cxCxl1.Length - 1 do
                                    let sVal = match solarVals with | Some arr -> Some arr.[i] | None -> None
                                    renderSolarRow cxCxl1.[i] cxClr1.[i] cxlAvl.[i] sVal
                            }
                            tfoot {
                                tr {
                                    attr.``style`` "font-weight: bold; border-top: 2px solid #ddd; height: 32px;"
                                    td { attr.colspan 2; attr.``style`` "border: 1px solid #eee; text-align: center; font-size: 10.5px; color: #888;"; text "Total Open" }
                                    td { 
                                        attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
                                        text (sprintf "%.0f" totalOpen) 
                                    }
                                    td { attr.``style`` "border: 1px solid #eee;" }
                                    td { attr.``style`` "border: 1px solid #eee;" }
                                }
                            }
                        }
                    }
                }
        }

        // --- COORDINATES WINDOW ---
        if isCoordsVisible then
            div {
                attr.``style`` "width: 100%; margin-top: 10px; background: #fdfdfd; border: 1px solid #eee; padding: 15px; box-shadow: inset 0 2px 4px rgba(0,0,0,0.02); border-radius: 4px;"
                div {
                    attr.``style`` "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;"
                    h4 { attr.``style`` "font-size: 11px; color: #888; margin: 0; letter-spacing: 1px;"; text "HEXEL COORDINATES (CSV)" }
                    div {
                        attr.``style`` "display: flex; gap: 10px;"
                        button {
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                            attr.``style`` "font-size: 9px; padding: 4px 8px;"
                            on.click (fun _ -> dispatch DownloadCoordCsv)
                            text "Download CSV"
                        }
                        button {
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                            attr.``style`` "font-size: 9px; padding: 4px 8px;"
                            on.click (fun _ -> dispatch ToggleCoords)
                            text "Close"
                        }
                    }
                }
                div {
                    attr.``style`` "max-height: 250px; overflow-y: auto; background: #fff; border: 1px solid #f5f5f5; padding: 10px;"
                    pre {
                        attr.``style`` "font-family: 'Cascadia Code', 'Consolas', monospace; font-size: 10px; color: #444; margin: 0; white-space: pre-wrap; word-break: break-all;"
                        text (FileManager.generateCoordinatesCsv [| (sqn, elv, cxCxl1) |])
                    }
                }
            }
    }
