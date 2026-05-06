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

printfn "Ratio: %f" ratio
printfn "Total Coxels: %d" result.Length

if result.Length > 0 then
    let cxl = result.[0]
    printfn "Coxel Name: %s" (prpVlu cxl.Name)
    printfn "Coxel Size (Requested): %s" (prpVlu cxl.Size)
    printfn "Hexels Placed: %d" (cxl.Hxls.Length + 1) // +1 for the Base hexel
    
    // Check if the base hexel is valid
    let bx, by, bz = hxlCrd cxl.Base
    printfn "Base Hexel: (%d, %d, %d)" bx by bz

printfn "Boundary Points: %A" boundary.[0]
