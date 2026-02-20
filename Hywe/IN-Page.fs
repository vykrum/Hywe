module Page

open Bolero.Html
open System
open Coxel
open Parse

/// <summary> Specifies the currently visible configuration panel. </summary>
type ActivePanel =
    | BoundaryPanel
    | LayoutPanel
    | TablePanel
    | ViewPanel
    | BatchPanel
    | TeachPanel

/// <summary> Specifies the input methodology - flowchart or text </summary>
type EditorMode =
    | Interactive
    | Syntax


type EditorTab =
    | Boundary
    | Editor of isAdvanced: bool

type DerivedData = {
    cxCxl1: Cxl[]
    cxlAvl: int[]
    cxClr1: string[]
    cxOuIl: (int*int)[][]
}

// Consistent Pastel Color
let hexToRgb (hex: string) =
    let hex = hex.TrimStart('#')
    let r = Convert.ToInt32(hex.Substring(0, 2), 16)
    let g = Convert.ToInt32(hex.Substring(2, 2), 16)
    let b = Convert.ToInt32(hex.Substring(4, 2), 16)
    (r, g, b)

let clamp value = min 255 (max 0 value)

/// Deterministic pastel generator — first color is base color
let generatePastels (rootHex: string) (count: int) (opacity: float) : string[] =
    let (baseR, baseG, baseB) = hexToRgb rootHex

    [| 0 .. count - 1 |]
    |> Array.map (fun i ->
        if i = 0 then
            // First color: base color
            $"rgba({baseR}, {baseG}, {baseB}, {opacity})"
        else
            // Remaining: pastel variations
            let hueShift = (i * 137) % 360
            let angleRad = float hueShift * Math.PI / 180.0

            let vary cmpnent phase =
                let offset = int (40.0 * Math.Sin(angleRad + phase))
                clamp ((cmpnent + offset + 255) >>> 1)

            let r = vary baseR 0.0
            let g = vary baseG 2.0
            let b = vary baseB 4.0

            $"rgba({r}, {g}, {b}, {opacity})"
    )

let deriveData (stx: string) (elv : int): DerivedData =
    let bsOc = [||]
    //let cxCxl1 = fst(spaceCxl bsOc stx)
    //let cxOuIl = snd(spaceCxl bsOc stx)
    let cxCxl1, cxOuIl = spaceCxl bsOc stx
    let cxlAvl = cxlExp cxCxl1 (Array.head cxCxl1).Seqn elv
    let cxClr1 = generatePastels "#888888" (Array.length cxCxl1) 0.85
    {
        cxCxl1 = cxCxl1
        cxlAvl = cxlAvl
        cxClr1 = cxClr1
        cxOuIl = cxOuIl

    }

///

// Default Syntax
let beeyond = $"(1/{NodeCode.initWeight}/Dock),(1.1/{NodeCode.initWeight}/Logistics),(1.2/{NodeCode.initWeight}/Lab),"+
              $"(1.3/{NodeCode.initWeight}/Habitation),(1.4/{NodeCode.initWeight}/Power)"

let beedroom = "(0/W=15/H=15/S=5,5/X=1/Q=VRCWEE),"+
                "(1/7/Foyer),(2/12/Living),(3/18/Dining),(1.1/12/Study),(2.1/12/Staircase),"+
                "(3.1/15/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),"+
                "(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/11/Closet-3),"+
                "(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"

let bedroom1 = "(1/16/Entry),(1.1/40/Living),(1.1.1/25/Study)," +
                "(1.1.1.1/10/Powder),(1.1.2/40/Dining),(1.1.2.1/20/Kitchen)," + 
                "(1.1.2.1.1/10/Utility),(1.1.2.2/26/Bed),(1.1.2.2.1/12/Bath)"


/// Sqn selection via slider
let allSqns : string list = [
    "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
    "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
]
let indexToSqn i = allSqns.[i]
let sqnToIndex sqn = allSqns |> List.findIndex ((=) sqn)

// Sequence Slider Component
let sequenceSlider (selected: string) (dispatch: int -> unit) =
    let currentIndex = sqnToIndex selected
    let maxIndex = 23

    // Label string (24 characters)
    let labelPhrase = "alternATE◦CONFIGURATions"

    div {
        attr.``class`` "slider-wrapper"

        // Labels
        div {
            yield attr.``class`` "slider-labels"
            for i in 0 .. maxIndex -> 
                span {
                    attr.``class`` (
                        if i = currentIndex then "slider-label active"
                        else "slider-label"
                    )
                    text (labelPhrase.[i].ToString())
                }
        }

        // Slider track
        div {
            attr.``class`` "slider-track-container"
            input {
                attr.``type`` "range"
                attr.``class`` "custom-slider"
                attr.min "0"
                attr.max (string maxIndex)
                attr.step "1"
                attr.value (string currentIndex)
                on.input (fun ev ->
                    match ev.Value with
                    | :? string as s ->
                        match System.Int32.TryParse(s) with
                        | true, i -> dispatch i
                        | _ -> ()
                    | _ -> ()
                )
            }
        }
    }

// Tab Panel Icons
let iconBoundary = "M3 3h18v18H3V3zm16 16V5H5v14h14zM7 7h10v10H7V7z"
let iconLayout   = "M12 2l3.5 2v4l-3.5 2-3.5-2V4l3.5-2z M7 11.5l3.5 2v4l-3.5 2-3.5-2v-4l3.5-2z M17 11.5l3.5 2v4l-3.5 2-3.5-2v-4l3.5-2z"
let iconTable    = "M4 18h16V6H4v12zm2-10h12v2H6V8zm0 4h12v2H6v-2zm0 4h12v2H6v-2z"
let icon3D = "M21 16.5c0 .38-.21.71-.53.88l-7.97 4.65c-.31.18-.69.18-1 0l-7.97-4.65c-.32-.17-.53-.5-.53-.88V7.5c0-.38.21-.71.53-.88l7.97-4.65c.31-.18.69-.18 1 0l7.97 4.65c.32.17.53.5.53.88v9zM12 4.15L6.04 7.5 12 10.85l5.96-3.35L12 4.15zM5 15.91l6 3.5v-6.71L5 9.21v6.7zm14 0v-6.7l-6 3.49v6.71l6-3.5z"
let iconBatch    = "M4 11h5V5H4v6zm0 7h5v-6H4v6zm6 0h5v-6h-5v6zm6 0h5v-6h-5v6zm-6-7h5V5h-5v6zm6-6v6h5V5h-5z"
let iconTeach    = "M5 13.18v2.81c0 .73.4 1.41 1.05 1.76l5 2.63c.59.32 1.29.32 1.88 0l5-2.63c.65-.35 1.05-1.03 1.05-1.76v-2.81l-6 3.16c-.63.33-1.37.33-2 0l-6-3.16zM12 3L1 9l11 6 9-4.91V17h2V9L12 3z"

let drawIcon path =
    svg {
        "viewBox" => "0 0 24 24"
        attr.style "width: 1.6rem; height: 1.6rem; fill: currentColor;"
        elt "path" { "d" => path }
    }

// Hywe Table
let renderRow (cxl: Cxl) (clr: string) (avl: int) =
    // Area of each Hexel is 4 sq units
    let hxlAreaX = 4
    let reqSz = 
        match prpVlu cxl.Rfid with
        | "1" -> (((prpVlu cxl.Size |> int) + 1)*hxlAreaX).ToString()
        | _ -> ((prpVlu cxl.Size |> int) * hxlAreaX).ToString()

    let achSz = (Array.length cxl.Hxls)*hxlAreaX
    let opnSz = avl * hxlAreaX
    let achCl = if achSz < int reqSz then "red" else "#646464"
    let avlCl = if avl < 1 then "red" else "#646464"

    tr {
        attr.``style`` $"background-color:{clr}"
        td { attr.width "15%"; attr.``style`` "padding: 5px 10px; text-align: center;"; prpVlu cxl.Rfid }
        td { attr.width "35%"; attr.``style`` "padding: 5px 10px; text-align: center;"; prpVlu cxl.Name }
        td { attr.width "15%"; attr.``style`` "padding: 5px 10px; text-align: center;"; reqSz }
        td { attr.width "15%"; attr.``style`` $"padding: 5px 10px; text-align: center; color:{achCl};"; $"{achSz}" }
        td { attr.width "15%"; attr.``style`` $"padding: 5px 10px; text-align: center; color:{avlCl};"; $"{opnSz}" }
    }

let viewHyweTable (cxCxl1: Cxl[]) (cxClr1: string[]) (cxlAvl: int[]) =
    div {
        attr.style "
                width: 100%;
                overflow-x: auto;
                padding: 10px;
                box-sizing: border-box;
            "
        table {
            attr.style "width: 100%;
                border-collapse: collapse;
                font-size: 14px;
                color: #646464;
                opacity: 0.85;"

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
        }
    }


