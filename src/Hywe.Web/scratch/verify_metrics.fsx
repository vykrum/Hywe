#r "../bin/Debug/net8.0/Hywe.Core.dll"

open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

let attrs = 
    Map.ofList [
        "L", "0"
        "W", "30"
        "H", "30"
        "E", "15,15"
        "Q", "VRCCNE"
    ]

let tree = [| [| "R1", 900, "Main Room" |] |]

let treeObj = LayoutTree.Create tree
let opts = {
    EntryFallback = "0,0"
    InitialOcc = [||]
    Seq = None
    Width = None
    Height = None
    OuterStr = None
    IslandsStr = None
    ParentCxl = None
    Ratio = None
    Elevation = None
}

let ctx = prepareLayoutContext attrs (LayoutTree.Create tree) opts
let baseRes = generateBaseCxl ctx
let result, boundary, ratio = 
    match baseRes with
    | Some (root, occ) -> generateCxlLayout ctx root occ
    | None -> [||], [||], 0.0

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

printfn "Boundary Area Check: %d" (Goxel.polygonArea boundary.[0])
