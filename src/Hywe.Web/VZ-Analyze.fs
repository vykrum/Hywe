module Analyze

open Hywe.Core.Coxel
open Bolero.Html
open ModelTypes

// --- INTERFACE ---

let renderRow (cxl: Cxl) (clr: string) (avl: int) (ratio: float) (elv: int) =
    let hxlAreaX = 4
    let isRootLvl0 = (prpVlu cxl.Rfid = "1" || prpVlu cxl.Name = "Root") && elv = 0
    let count = if isRootLvl0 then (prpVlu cxl.Size |> float) + 1.0 else (prpVlu cxl.Size |> float)
    let reqSz = count * float hxlAreaX
    let achSz = (float (Array.length cxl.Hxls)) * float hxlAreaX
    let achCl = match achSz < (reqSz * 0.99) with | true -> "red" | false -> "#646464"
    let avlCl = match float avl * float hxlAreaX < 0.1 with | true -> "red" | false -> "#646464"

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
            attr.width "15%"
            attr.``style`` $"padding: 8px; text-align: center; color:{avlCl}; border: 1px solid #eee;"
            text (sprintf "%.0f" (float avl * float hxlAreaX)) 
        }
    }

let renderAdjacencyCell isAdj color =
    match isAdj with
    | true -> 
        td {
            attr.``style`` $"border: 1px solid #eee; padding: 8px; text-align: center; background: {color}; color: #646464;"
            text "✓"
        }
    | false ->
        td {
            attr.``style`` "border: 1px solid #eee; padding: 8px; text-align: center; color: #646464; opacity: 0.4;"
            text "✕"
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
                    attr.``style`` "width: 100%; border-collapse: collapse; font-size: 11px; color: #646464; font-family: 'Outfit', system-ui, sans-serif; opacity: 0.85;"
                    thead {
                        tr {
                            attr.``style`` "height: 32px;"
                            th { 
                                attr.``style`` "border: 1px solid #eee; padding: 8px; background: #fdfdfd;"
                                text "" 
                            }
                            for i in 0 .. colors.Length - 1 do
                                let c = colors.[i]
                                th { 
                                    attr.``style`` $"border: 1px solid #eee; padding: 8px; background: {c}; color: #333; min-width: 30px;"
                                    text names.[i] 
                                }
                        }
                    }
                    tbody {
                        for i in 0 .. matrix.Length - 1 do
                            let row = matrix.[i]
                            tr {
                                attr.``style`` "height: 32px;"
                                td { 
                                    attr.``style`` $"border: 1px solid #eee; padding: 8px; background: {colors.[i]}; color: #333; white-space: nowrap;"
                                    text names.[i] 
                                }
                                for j in 0 .. row.Length - 1 do
                                    renderAdjacencyCell row.[j] colors.[j]
                            }
                    }
                }
            }
        }

let viewHyweAnalyze (dispatch: Message -> unit) (sqn: string) (cxCxl1: Cxl[]) (cxClr1: string[]) (cxlAvl: int[]) (cxAdj1: string[] * bool[][]) (ratio: float) (elv: int) =
    let hxlAreaX = 4
    let totalReq = cxCxl1 |> Array.sumBy (fun c -> 
        let isRootLvl0 = (prpVlu c.Rfid = "1" || prpVlu c.Name = "Root") && elv = 0
        let count = if isRootLvl0 then (prpVlu c.Size |> float) + 1.0 else (prpVlu c.Size |> float)
        count * float hxlAreaX)
    let totalAch = cxCxl1 |> Array.sumBy (fun c -> (float (Array.length c.Hxls)) * float hxlAreaX)
    
    let adjNames, adjMatrix = cxAdj1

    div {
        attr.``style`` "width: 100%; padding: 10px; box-sizing: border-box; display: flex; flex-wrap: wrap; gap: 30px; align-items: flex-start;"
        
        div {
            attr.``style`` "flex: 1 1 450px; min-width: 300px;"
            h3 { 
                attr.``style`` "font-size: 14px; color: #444; margin-bottom: 15px; border-left: 4px solid #888; padding-left: 10px; font-family: 'Outfit', system-ui, sans-serif; display: flex; justify-content: space-between; align-items: center;"
                text "AREA METRICS" 
                div {
                    attr.``style`` "display: flex; gap: 8px; align-items: center;"
                    span {
                        attr.``style`` "font-size: 10px; color: #aaa; font-weight: normal; margin-left: 10px; letter-spacing: 1px;"
                        text $"[{sqn}]"
                    }
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
                            th { attr.``style`` "border: 1px solid #eee; padding: 12px; background: #fdfdfd;"; text "Open" }
                        }
                    }
                    tbody {
                        for i in 0 .. cxCxl1.Length - 1 do
                            renderRow cxCxl1.[i] cxClr1.[i] cxlAvl.[i] ratio elv
                    }
                    tfoot {
                        tr {
                            attr.``style`` "font-weight: bold; border-top: 2px solid #ddd; height: 32px;"
                            td { attr.colspan 2; attr.``style`` "border: 1px solid #eee;" }
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

        div {
            attr.``style`` "flex: 1 1 450px; min-width: 300px;"
            viewAdjacencyTable sqn adjNames cxClr1 adjMatrix
        }
        // --- DOWNLOAD GROUP ---
        div {
            attr.``style`` "width: 100%; display: flex; gap: 10px; margin-top: 20px; justify-content: center; align-items: center; flex-wrap: wrap;"
            button {
                attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                attr.``style`` "font-size: 10px; padding: 6px 12px;"
                attr.title "Coordinates"
                on.pointerdown (fun _ -> dispatch DownloadCoordCsv)
                text "Crd"
            }
            button {
                attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                attr.``style`` "font-size: 10px; padding: 6px 12px;"
                attr.title "Area Metrics"
                on.pointerdown (fun _ -> dispatch DownloadMetricsCsv)
                text "ARE"
            }
            button {
                attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                attr.``style`` "font-size: 10px; padding: 6px 12px;"
                attr.title "Adjacency Matrix"
                on.pointerdown (fun _ -> dispatch DownloadAdjCsv)
                text "Adj"
            }
        }
    }
