#r "../bin/Debug/net8.0/Hywe.Core.dll"

open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Layout

let attrs = 
    Map.ofList [
        "L", "0"
        "W", "30"
        "H", "30"
        "E", "15,15"
        "Q", "VRCCNE"
    ]

let tree = [| [| "R1", 900, "Main Room" |] |]

let result, boundary, ratio = 
    generateCxlLayout attrs tree "0,0" None None None None None [||] None None None

let hxlAreaX = 1.0 / ratio

printfn "Ratio: %f" ratio
printfn "Hxl Area Factor (1/Ratio): %f" hxlAreaX

if result.Length > 0 then
    let cxl = result.[0]
    let numHxls = cxl.Hxls.Length + 1
    let achSz = float numHxls * hxlAreaX
    
    printfn "--- ROOM DATA ---"
    printfn "Required Area: 900"
    printfn "Hexels Placed: %d" numHxls
    printfn "Achieved Area calculation: %d * %f = %f" numHxls hxlAreaX achSz
    
    if achSz > 900.0 then
        printfn "RESULT: Achieved (>900) is MORE than Required (900)"
    elif achSz < 900.0 then
        printfn "RESULT: Achieved (<900) is LESS than Required (900)"
    else
        printfn "RESULT: Achieved is EQUAL to Required"

printfn "Boundary Area Check: %d" (Geometry.polygonArea boundary.[0])
