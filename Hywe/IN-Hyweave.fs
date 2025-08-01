module Page

open Bolero.Html
open System
open Coxel
open Parse

type Beeset = 
    | Beewhich
    | Beegin
    | Beespoke

type Props = {
Selected: Beeset option
OnChange: Beeset -> unit
}

type DerivedData = {
    cxCxl1: Cxl[]
    cxlAvl: int[]
    cxClr1: string[]
}

/// Hywe logo and title
let hyweHeader =
    header {
        attr.style "display: flex; flex-direction: row; font-family: 'Optima', Candara, Calibri; width: 100%; height: 37px; opacity: 1; background: #363636; padding-left: 5px; padding-top: 5px;"

        div {
            img {
                attr.src "https://hywe.in/images/icon-32x32.png"
                attr.width 30
                attr.height 30
            }
        }
        div {
            attr.style "color: white; font-family: 'Optima', Candara, Calibri; font-size: 20px; font-weight: normal; padding-left: 10px; padding-right: 10px; padding-bottom: 7px;"
            text "H Y W E"
        }
        div {
            attr.style "opacity: 1;"
            img {
                attr.src "https://vykrum.github.io/Hywe/images/hyweLogoAcronym.png"
                attr.width 200
                attr.height 45
            }
        }
    }

/// Hywe Introduction
let hyweIntro =
    section {
        attr.id "introduction"
        attr.style "background: #d3d3d1; color: #363636; font-family: 'Optima', Candara, Calibri; font-size: 18px; padding: 15px 12px;"
        p {
            attr.style "margin: 0;"
            text "Weave spatial configurations with "
            strong { text "HYWE" }
            text ", an endogenous space planning concept with an intuitive interface and no learning curve. Combining graphical interaction with embedded spatial logic, it introduces a novel and distinctive approach to early-stage layout design."
        }
    }

// Random pastel color
let pastel () =
    let rand = Random()
    let mixWithWhite (color: int) = (color + 255) >>> 1
    let red = mixWithWhite (rand.Next(256))
    let green = mixWithWhite (rand.Next(256))
    let blue = mixWithWhite (rand.Next(256))
    // Ensure the values are within the 0-255 range
    let clamp value = min 255 (max 0 value)
    let red = clamp red
    let green = clamp green
    let blue = clamp blue
    $"rgba({red}, {green}, {blue}, 0.75)"

// Function to create an array of random pastel colors
let pastels (size: int) =
    Array.init size (fun _ -> pastel())

let deriveData (stx: string) : DerivedData =
    let bsOc = [||]
    let cxCxl1 = spaceCxl bsOc stx
    let cxlAvl = cxlExp cxCxl1 (Array.head cxCxl1).Seqn
    let cxClr1 = pastels (Array.length cxCxl1)
    {
        cxCxl1 = cxCxl1
        cxlAvl = cxlAvl
        cxClr1 = cxClr1
    }

///

(*let pageTitle = 
    div{ 
                    attr.style="display: flex;
                                flex-direction: row;
                                font-family: 'Optima', Candara, Calibri;
                                width: 100%;
                                height: 37px;
                                opacity: 1;
                                background: #363636;
                                padding-left: 5px;
                                padding-top: 5px;"
                    img{src="https://hywe.in/icon-32x32.png" width="30" height="30"}
        
                    span{
                        attr.style="color: white; 
                        font-family: 'Optima', Candara, Calibri; 
                        font-size: 20px;
                        font-weight: normal;
                        padding-left: 10px;
                        padding-right: 10px;
                        padding-bottom: 7px;" 
                        "H Y W E" 
                        }
                    span{img {src="https://vykrum.github.io/Hywe/hyweLogoAcronym.png" width="200" height="45"}}
            }*)
// Defaults
let stxInstr = 
    " 1. Make a choice above.\n" +
    " 2. View HYWE syntax here.\n" +
    " 3. Click HYWEAVE below."

let stx2Ini = "(0/Q=1),(1/3/.)"

let beeline = "(1/19/Start),(2/15/End)"

let beeyond = "(1/24/Dock),(1.1/25/Logistics),(1.2/24/Lab),"+
              "(1.3/25/Habitation),(1.4/25/Power)"

let beedroom = "(0/W=15/H=15/I=0/S=1/Q=VRCWEE),"+
                "(1/7/Foyer),(2/12/Living),(3/18/Dining),(1.1/12/Study),(2.1/12/Staircase),"+
                "(3.1/15/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),"+
                "(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/11/Closet-3),"+
                "(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"

// Dropdown Beeselect

let beeSelect (selected: Beeset option) (onSelect: Beeset -> unit) =
    select{
        attr.name "options"
        attr.``class`` "dropdown1"
        attr.id "options"
        on.change (fun e -> 
            let value = (e.Value :?> string)
            let beeset = 
                match value with
                | "Bee-gin" -> Beegin
                | "Bee-spoke" -> Beespoke
                | _ -> Beewhich

            onSelect beeset)
        option {
            attr.selected "true"
            attr.value "Bee-which"
            "To  Bee or To Bee . . ."
        }
        option {
            attr.value "Bee-gin"
            "Bee-gin : Space Flow Chart"
        }
        option {
            attr.value "Bee-spoke"
            "Bee-spoke : Space Flow Script"
        }
    }

/// A UI component that lets user select an Sqn via slider
let allSqns : string list = [
    "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
    "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
]

let indexToSqn i = allSqns.[i]
let sqnToIndex sqn = allSqns |> List.findIndex ((=) sqn)

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
                on.change (fun ev ->
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

// Hywe Table
let renderRow (cxl: Cxl) (clr: string) (avl: int) =
    let reqSz = 
        match prpVlu cxl.Rfid with
        | "1" -> ((prpVlu cxl.Size |> int) + 1).ToString()
        | _ -> prpVlu cxl.Size

    let achSz = Array.length cxl.Hxls
    let achCl = if achSz < int reqSz then "red" else "#646464"
    let avlCl = if avl < 1 then "red" else "#646464"

    tr {
        attr.``style`` $"background-color:{clr}"
        td { attr.width "15%"; attr.``style`` "padding: 5px 10px; text-align: center;"; prpVlu cxl.Rfid }
        td { attr.width "35%"; attr.``style`` "padding: 5px 10px; text-align: center;"; prpVlu cxl.Name }
        td { attr.width "15%"; attr.``style`` "padding: 5px 10px; text-align: center;"; reqSz }
        td { attr.width "15%"; attr.``style`` $"padding: 5px 10px; text-align: center; color:{achCl};"; $"{achSz}" }
        td { attr.width "15%"; attr.``style`` $"padding: 5px 10px; text-align: center; color:{avlCl};"; $"{avl}" }
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



