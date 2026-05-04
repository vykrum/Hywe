module Page

open Bolero.Html
open System
open Hywe.Core

/// <summary> Specifies the currently visible configuration panel. </summary>
type ActivePanel =
    | BoundaryPanel
    | LayoutPanel
    | AnalyzePanel
    | ViewPanel
    | BatchPanel
    | TeachPanel
    | ReportPanel

/// <summary> Specifies the input methodology - flowchart or text </summary>
type EditorMode =
    | Interactive
    | Syntax


type EditorTab =
    | Boundary
    | Editor of isAdvanced: bool


// Default Syntax
let emptyState = "| (0/X=1), (1/75/Root) |"

let beeyond = $"| (0/X=1), (1/105/Dock),(1.1/85/Logistics),(1.2/95/Lab),"+
              $"(1.3/65/Habitation),(1.4/75/Power) |"

let beedroom = "| (0/Q=HRCCNW/L=0/W=30/H=30/X=0/E=0/O=0,0,30,0,30,30,0,30/I=/T=3), (1/12/Foyer), (1.1/12/Living), (1.1.1/18/Dining)," +
                "(1.1.1.1/15/Kitchen), (1.1.1.1.1/6/Utility), (1.1.1.2/14/Bed-1), (1.1.1.2.1/8/Bath-1), (1.1.1.3/18/Bed-2)," +
                " (1.1.1.3.1/10/Closet-2), (1.1.1.3.1.1/10/Bath-2), (1.1.1.4/18/Bed-3), (1.1.1.4.1/11/Closet-3), (1.1.1.4.2/10/Bath-3)," +
                " (1.1.2/12/Staircase), (1.2/12/Study) |"

let stackedTower = "| (0/Q=VRCCNE/L=0/W=30/H=30/X=1/E=0/O=/I=), (1/75/Lobby), (1.1/88/Retail), (1.2/54/Toilets), (1.3/67/Retail)," +
                    "(1.4/94/Retail) | ; | (0/Q=VRCCNE/L=3/E=1), (1/75/Lobby), (1.1/43/Office), (1.2/123/Office), (1.2.1/34/Toilets)," + 
                    "(1.3/52/Office) | ; | (0/Q=VRCCNE/L=6/E=1/T=5), (1/75/Lobby), (1.1/99/Suite) |"


/// Sqn selection via slider
let allSqns : string list = [
    "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
    "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
]
let indexToSqn i = allSqns.[i]
let sqnToIndex sqn = allSqns |> List.findIndex ((=) sqn)

// Label string (24 characters)
let labelPhrase = "alternATE◦CONFIGURATions"

// Sequence Slider Component
let sequenceSlider (selected: string) (minIdx: int) (maxIdx: int) (dispatch: int -> unit) =
    let currentIndex = sqnToIndex selected
    
    div {
        attr.``class`` "slider-wrapper"

        // Labels
        div {
            yield attr.``class`` "slider-labels"
            for i in 0 .. 23 -> 
                let inRange = i >= minIdx && i <= maxIdx
                span {
                    attr.``class`` (
                        match i = currentIndex, inRange with 
                        | true, true  -> "slider-label active"
                        | true, false -> "slider-label active inactive"
                        | false, true -> "slider-label"
                        | false, false -> "slider-label inactive"
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
                attr.max "23"
                attr.step "1"
                attr.value (string currentIndex)
                on.input (fun ev ->
                    match ev.Value with
                    | :? string as s ->
                        match System.Int32.TryParse(s) with
                        | true, i -> 
                            let clamped = max minIdx (min maxIdx i)
                            dispatch clamped
                        | _ -> ()
                    | _ -> ()
                )
            }
        }
    }

// Tab Panel Icons
let iconBoundary = "M3 3h18v18H3V3zm16 16V5H5v14h14zM7 7h10v10H7V7z"
let iconLayout   = "M12 2l3.5 2v4l-3.5 2-3.5-2V4l3.5-2z M7 11.5l3.5 2v4l-3.5 2-3.5-2v-4l3.5-2z M17 11.5l3.5 2v4l-3.5 2-3.5-2v-4l3.5-2z"
let iconAnalyze  = "M5 9.2h3V19H5zM10.6 5h2.8v14h-2.8zm5.6 8H19v6h-2.8z"
let icon3D       = "M21 16.5c0 .38-.21.71-.53.88l-7.97 4.65c-.31.18-.69.18-1 0l-7.97-4.65c-.32-.17-.53-.5-.53-.88V7.5c0-.38.21-.71.53-.88l7.97-4.65c.31-.18.69-.18 1 0l7.97 4.65c.32.17.53.5.53.88v9zM12 4.15L6.04 7.5 12 10.85l5.96-3.35L12 4.15zM5 15.91l6 3.5v-6.71L5 9.21v6.7zm14 0v-6.7l-6 3.49v6.71l6-3.5z"
let iconBatch    = "M4 11h5V5H4v6zm0 7h5v-6H4v6zm6 0h5v-6h-5v6zm6 0h5v-6h-5v6zm-6-7h5V5h-5v6zm6-6v6h5V5h-5z"
let iconTeach    = "M5 13.18v2.81c0 .73.4 1.41 1.05 1.76l5 2.63c.59.32 1.29.32 1.88 0l5-2.63c.65-.35 1.05-1.03 1.05-1.76v-2.81l-6 3.16c-.63.33-1.37.33-2 0l-6-3.16zM12 3L1 9l11 6 9-4.91V17h2V9L12 3z"
let iconReport   = "M18 2H6c-1.1 0-2 .9-2 2v16c0 1.1.9 2 2 2h12c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zM18 20H6V4h12v16zM8 6h8v2H8zm0 4h8v2H8zm0 4h4v2H8z"
let iconLocked   = "M18 8h-1V6c0-2.76-2.24-5-5-5S7 3.24 7 6v2H6c-1.1 0-2 .9-2 2v10c0 1.1.9 2 2 2h12c1.1 0 2-.9 2-2V10c0-1.1-.9-2-2-2zM9 6c0-1.66 1.34-3 3-3s3 1.34 3 3v2H9V6zm9 14H6V10h12v10zm-6-3c1.1 0 2-.9 2-2s-.9-2-2-2-2 .9-2 2 .9 2 2 2z"
let iconUnlocked = "M18 8h-1V6c0-2.76-2.24-5-5-5S7 3.24 7 6h2c0-1.66 1.34-3 3-3s3 1.34 3 3v2H6c-1.1 0-2 .9-2 2v10c0 1.1.9 2 2 2h12c1.1 0 2-.9 2-2V10c0-1.1-.9-2-2-2zm0 12H6V10h12v10zm-6-3c1.1 0 2-.9 2-2s-.9-2-2-2-2 .9-2 2 .9 2 2 2z"

let drawIcon path =
    svg {
        "viewBox" => "0 0 24 24"
        attr.style "width: 1.6rem; height: 1.6rem; fill: currentColor;"
        elt "path" { "d" => path }
    }

let drawIconSized size path =
    svg {
        "viewBox" => "0 0 24 24"
        attr.style (sprintf "width: %s; height: %s; fill: currentColor;" size size)
        elt "path" { "d" => path }
    }
