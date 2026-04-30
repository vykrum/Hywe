module Table

open Coxel
open Bolero.Html

// Hywe Table
let renderRow (cxl: Cxl) (clr: string) (avl: int) =
    // Area of each Hexel is 4 sq units
    let hxlAreaX = 4
    // Required Size: (Node Size from reproportioning) * (Area Factor 4.0)
    let reqSz = (prpVlu cxl.Size |> int) * hxlAreaX

    // Achieved Size: (Count of generated hexels in Coxel) * (Area Factor 4.0)
    let achSz = (Array.length cxl.Hxls)*hxlAreaX
    let opnSz = avl * hxlAreaX
    let achCl = if achSz < reqSz then "red" else "#646464"
    let avlCl = if avl < 1 then "red" else "#646464"

    tr {
        attr.``style`` $"background-color:{clr}"
        td { attr.width "15%"; attr.``style`` "padding: 5px 10px; text-align: center;"; prpVlu cxl.Rfid }
        td { attr.width "35%"; attr.``style`` "padding: 5px 10px; text-align: center;"; prpVlu cxl.Name }
        td { attr.width "15%"; attr.``style`` "padding: 5px 10px; text-align: center;"; reqSz.ToString() }
        td { attr.width "15%"; attr.``style`` $"padding: 5px 10px; text-align: center; color:{achCl};"; $"{achSz}" }
        td { attr.width "15%"; attr.``style`` $"padding: 5px 10px; text-align: center; color:{avlCl};"; $"{opnSz}" }
    }

let viewHyweTable (cxCxl1: Cxl[]) (cxClr1: string[]) (cxlAvl: int[]) =
    let hxlAreaX = 4
    let totalReq = cxCxl1 |> Array.sumBy (fun c -> (prpVlu c.Size |> int) * hxlAreaX)
    let totalAch = cxCxl1 |> Array.sumBy (fun c -> (Array.length c.Hxls) * hxlAreaX)

    div {
        attr.style "width: 100%; overflow-x: auto; padding: 10px; box-sizing: border-box;"
        table {
            attr.style "width: 100%; border-collapse: collapse; font-size: 14px; color: #646464; opacity: 0.85;"
            thead {
                tr {
                    th { "Index" }
                    th { "Label" }
                    th { "Required" }
                    th { "Achieved" }
                    th { "Open" }
                }
            }
            tbody {
                for (cxl, clr, avl) in Array.zip3 cxCxl1 cxClr1 cxlAvl do
                    yield renderRow cxl clr avl
            }
            tfoot {
                tr {
                    attr.style "font-weight: bold; border-top: 2px solid #ddd;"
                    td { attr.colspan 2; attr.style "padding: 10px; text-align: right;"; text "TOTAL SITE AREA" }
                    td { attr.style "padding: 10px; text-align: center;"; text (string totalReq) }
                    td { attr.style "padding: 10px; text-align: center;"; text (string totalAch) }
                    td { }
                }
            }
        }
    }