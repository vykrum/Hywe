module Analyze

open Coxel
open Bolero.Html

// --- PROCESSING (UI HELPER) ---
// Adjacency calculation is now in Coxel.cxlAdj

// --- INTERFACE ---

let renderRow (cxl: Cxl) (clr: string) (avl: int) =
    let hxlAreaX = 4
    let reqSz = (prpVlu cxl.Size |> int) * hxlAreaX
    let achSz = (Array.length cxl.Hxls)*hxlAreaX
    let achCl = match achSz < reqSz with | true -> "red" | false -> "#646464"
    let avlCl = match avl < 1 with | true -> "red" | false -> "#646464"

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
            text (string reqSz) 
        }
        td { 
            attr.width "15%"
            attr.``style`` $"padding: 8px; text-align: center; color:{achCl}; border: 1px solid #eee;"
            text (string achSz) 
        }
        td { 
            attr.width "15%"
            attr.``style`` $"padding: 8px; text-align: center; color:{avlCl}; border: 1px solid #eee;"
            text (string (avl * hxlAreaX)) 
        }
    }

let renderAdjacencyCell isAdj color =
    match isAdj with
    | true -> 
        td {
            attr.``style`` $"border: 1px solid #eee; padding: 8px; text-align: center; background: {color}; color: #2c3e50;"
            text "✓"
        }
    | false ->
        td {
            attr.``style`` "border: 1px solid #eee; padding: 8px; text-align: center; color: #ddd;"
            text "✕"
        }

let viewAdjacencyTable (sqnName: string) (names: string[]) (colors: string[]) (matrix: bool[][]) =
    match names.Length with
    | 0 -> div {}
    | _ ->
        div {
            attr.``style`` "width: 100%; box-sizing: border-box;"
            h3 { 
                attr.``style`` "font-size: 14px; color: #444; margin-bottom: 20px; border-left: 4px solid #888; padding-left: 10px; font-family: sans-serif;"
                text "ADJACENCY MATRIX" 
                span {
                    attr.``style`` "font-size: 10px; color: #aaa; font-weight: normal; margin-left: 10px; letter-spacing: 1px;"
                    text $"[{sqnName}]"
                }
            }
            div {
                attr.``style`` "overflow-x: auto;"
                table {
                    attr.``style`` "width: 100%; border-collapse: collapse; font-size: 11px; color: #646464; font-family: Verdana, sans-serif; opacity: 0.85;"
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

let viewHyweAnalyze (sqn: string) (cxCxl1: Cxl[]) (cxClr1: string[]) (cxlAvl: int[]) =
    let hxlAreaX = 4
    let totalReq = cxCxl1 |> Array.sumBy (fun c -> (prpVlu c.Size |> int) * hxlAreaX)
    let totalAch = cxCxl1 |> Array.sumBy (fun c -> (Array.length c.Hxls) * hxlAreaX)
    
    let adjNames, adjMatrix = cxlAdj cxCxl1

    div {
        attr.``style`` "width: 100%; padding: 10px; box-sizing: border-box; display: flex; flex-wrap: wrap; gap: 30px; align-items: flex-start;"
        
        div {
            attr.``style`` "flex: 1 1 450px; min-width: 300px;"
            h3 { 
                attr.``style`` "font-size: 14px; color: #444; margin-bottom: 15px; border-left: 4px solid #888; padding-left: 10px; font-family: sans-serif;"
                text "AREA METRICS" 
                span {
                    attr.``style`` "font-size: 10px; color: #aaa; font-weight: normal; margin-left: 10px; letter-spacing: 1px;"
                    text $"[{sqn}]"
                }
            }

            div {
                attr.``style`` "overflow-x: auto;"
                table {
                    attr.``style`` "width: 100%; border-collapse: collapse; font-size: 11px; color: #646464; font-family: Verdana, sans-serif; opacity: 0.85;"
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
                            renderRow cxCxl1.[i] cxClr1.[i] cxlAvl.[i]
                    }
                    tfoot {
                        tr {
                            attr.``style`` "font-weight: bold; border-top: 2px solid #ddd; height: 32px;"
                            td { attr.colspan 2; attr.``style`` "border: 1px solid #eee;" }
                            td { 
                                attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
                                text (string totalReq) 
                            }
                            td { 
                                attr.``style`` "padding: 8px; text-align: center; border: 1px solid #eee;"
                                text (string totalAch) 
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
    }
