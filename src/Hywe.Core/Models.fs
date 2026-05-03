namespace Hywe.Core

open Coxel
open Hexel
open Parse
open System

type DerivedData = {
    cxCxl1: Cxl[]
    cxlAvl: int[]
    cxClr1: string[]
    cxOuIl: (float*float)[][]
    cxElv1: float[]
    cxAdj1: string[] * bool[][]
    cxB36: string[]
}

module PageHelpers =
    let labelPhrase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    let indexToLabel (i: int) =
        if i < 0 then "A"
        elif i < 26 then string labelPhrase.[i]
        else
            let first = labelPhrase.[(i / 26) - 1]
            let second = labelPhrase.[i % 26]
            $"{first}{second}"

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
            match i = 0 with
            | true ->
                $"rgba({baseR}, {baseG}, {baseB}, {opacity})"
            | false ->
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

    let deriveDataFromLayout (cxCxl1: Cxl[]) (cxOuIl: (float*float)[][]) (cxElv1: float[]) (elv: int) : DerivedData =
        let fallbackSqn = Hexel.VRCCNE 
        let activeSqn = if Array.isEmpty cxCxl1 then fallbackSqn else (Array.head cxCxl1).Seqn
        
        let cxlAvl = if Array.isEmpty cxCxl1 then [||] else cxlExp cxCxl1 activeSqn elv
        let cxClr1 = generatePastels "#888888" (max 1 (Array.length cxCxl1)) 0.85
        let cxAdj1 = cxlAdj cxCxl1
        let cxB36 = cxCxl1 |> Array.map getCxlCoordsString

        {
            cxCxl1 = cxCxl1
            cxlAvl = cxlAvl
            cxClr1 = cxClr1
            cxOuIl = cxOuIl
            cxElv1 = cxElv1
            cxAdj1 = cxAdj1
            cxB36 = cxB36
        }

    let deriveData (stx: string) (enStr: string) (elv : int): DerivedData =
        let bsOc = [||]
        let cxCxl1, cxOuIl, cxElv1 = 
            try generateMultiLevelLayout stx enStr bsOc None None None
            with _ -> [||], [||], [| 0.0; 3.0 |]
        
        deriveDataFromLayout cxCxl1 cxOuIl cxElv1 elv
