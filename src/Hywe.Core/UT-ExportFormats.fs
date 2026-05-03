module Hywe.Core.ExportFormats

open Coxel
open Hexel
open System.Text
open Hywe.Core

// --- COORDINATE HELPERS ---

// Base36 helpers moved to Coxel module

// --- CSV EXPORTS ---

let generateCoordinatesCsv (data: (string * int * Cxl[] * string[])[]) =
    let sb = StringBuilder()
    sb.AppendLine("Orientation,Level,CoxelID,CoxelName,BaseCoordinate,Coordinates") |> ignore
    for (sqn, elv, cxls, b36s) in data do
        for i = 0 to cxls.Length - 1 do
            let cxl = cxls.[i]
            let id = prpVlu cxl.Rfid
            let name = prpVlu cxl.Name
            let baseCoord = getBaseCoordString cxl
            let coords = b36s.[i]
            sb.AppendLine(sprintf "%s,%d,%s,%s,%s,%s" sqn elv id name baseCoord coords) |> ignore
    sb.ToString()

let generateAreaMetricsCsv (data: (string * int * Cxl[])[]) =
    let sb = StringBuilder()
    let hxlAreaX = 1
    sb.AppendLine("Orientation,Level,CoxelID,CoxelName,Required,Achieved,TargetMet") |> ignore
    for (sqn, elv, cxls) in data do
        for cxl in cxls do
            let reqSz = (prpVlu cxl.Size |> int) * hxlAreaX
            let achSz = (Array.length cxl.Hxls) * hxlAreaX
            let id = prpVlu cxl.Rfid
            let name = prpVlu cxl.Name
            let targetMet = if achSz >= reqSz then "Yes" else "No"
            sb.AppendLine(sprintf "%s,%d,%s,%s,%d,%d,%s" sqn elv id name reqSz achSz targetMet) |> ignore
    sb.ToString()

let generateAdjacencyCsv (data: (string * int * (string[] * bool[][]))[]) =
    let sb = StringBuilder()
    for (sqn, elv, (names, matrix)) in data do
        if not (Array.isEmpty names) then
            sb.AppendLine(sprintf "--- %s | Level %d ---" sqn elv) |> ignore
            let header = "Room," + String.concat "," names
            sb.AppendLine(header) |> ignore
            for i = 0 to matrix.Length - 1 do
                let row = matrix.[i]
                let rowStr = names.[i] + "," + String.concat "," (row |> Array.map (fun adj -> if adj then "1" else "0"))
                sb.AppendLine(rowStr) |> ignore
            sb.AppendLine() |> ignore
    sb.ToString()

// --- HYNTERACT & DATASET GENERATION ---

let generateHynteractPayload (str: string) (seq: Sqn) (ouStr: string) (ilStr: string) (enStr: string) (initialOcc : Hxl[]) = 
    let finalBatch, _, _ = Parse.generateMultiLevelLayout str enStr initialOcc (Some seq) (Some ouStr) (Some ilStr)

    finalBatch
    |> Array.groupBy (fun cxl -> let (_, _, z) = hxlCrd cxl.Base in z)
    |> Array.sortBy fst
    |> Array.map (fun (_, levelCxls) ->
        levelCxls
        |> Array.map getCxlCoordsString
        |> String.concat ";"
    )
    |> String.concat "|"

let generateDataset (fullStr: string) (ouStr: string) (ilStr: string) (enStr: string) =
    Hexel.sqnArray 
    |> Array.collect (fun sqn ->
        let sqnName = sprintf "%A" sqn
        let forcedStr = Parsing.injectSqn fullStr sqnName
        try
            let cxls, _, _ = Parse.generateMultiLevelLayout forcedStr enStr [||] (Some sqn) (Some ouStr) (Some ilStr)
            cxls 
            |> Array.groupBy (fun c -> let (_, _, z) = hxlCrd c.Base in z)
            |> Array.map (fun (z, lvlCxls) -> (sqnName, z, lvlCxls))
        with _ -> [||]
    )

// --- DXF Export (2D Layout) ---
let generateDxf (cxls: Cxl[]) (offsetX: float) (offsetY: float) =
    let sb = StringBuilder()
    
    // Hex size and scale parameters (adjust if necessary)
    let hexScale = 1.0 // Standardize to 1 unit per hex center distance
    
    for cxl in cxls do
        // Get the ordered polygon vertices for the coxel
        let prm = cxlPrm cxl 0
        if prm.Length > 2 then
            sb.AppendLine("0\nLWPOLYLINE") |> ignore
            sb.AppendLine("8\nRooms") |> ignore
            sb.AppendLine("90\n" + string prm.Length) |> ignore // Number of vertices
            sb.AppendLine("70\n1") |> ignore // Closed polygon flag
            
            for (x, y) in prm do
                // Convert hex grid coordinates to Cartesian coordinates
                // Using standard flat-topped hex grid math
                let q = x
                let r = y
                let cx = (hexScale * (1.5 * q)) + offsetX
                let cy = (hexScale * (sqrt(3.0) / 2.0 * q + sqrt(3.0) * r)) + offsetY
                
                sb.AppendLine("10\n" + string cx) |> ignore
                sb.AppendLine("20\n" + string cy) |> ignore
    sb.ToString()

let generateDxfBatch (batch: Cxl[] list) =
    let sb = StringBuilder()
    sb.AppendLine("0\nSECTION\n2\nHEADER\n0\nENDSEC") |> ignore
    sb.AppendLine("0\nSECTION\n2\nTABLES\n0\nTABLE\n2\nLAYER\n70\n1") |> ignore
    sb.AppendLine("0\nLAYER\n2\nRooms\n70\n0\n62\n7\n0\nENDTAB\n0\nENDSEC") |> ignore
    sb.AppendLine("0\nSECTION\n2\nBLOCKS\n0\nENDSEC") |> ignore
    sb.AppendLine("0\nSECTION\n2\nENTITIES") |> ignore
    
    let cols = 4
    batch |> List.iteri (fun i cxls ->
        let r = i / cols
        let c = i % cols
        let ox = float c * 100.0 // spacing
        let oy = float r * -100.0
        sb.Append(generateDxf cxls ox oy) |> ignore
    )

    sb.AppendLine("0\nENDSEC") |> ignore
    sb.AppendLine("0\nEOF") |> ignore
    sb.ToString()

// --- OBJ Export (3D Geometry) ---
let generateObj (cxls: Cxl[]) (elevations: float[]) (offsetX: float) (offsetY: float) (vOffset: int ref) =
    let sb = StringBuilder()
    for cxl in cxls do
        let (_, _, zInt) = Hexel.hxlCrd cxl.Base
        let zBottom = if zInt < elevations.Length then elevations.[zInt] else float zInt * 3.0
        let zTop = if zInt + 1 < elevations.Length then elevations.[zInt + 1] else zBottom + 3.0
        
        let prm = cxlPrm cxl zInt
        let n = prm.Length
        if n > 2 then
            for (x, y) in prm do
                sb.AppendLine(sprintf "v %f %f %f" (x + offsetX) zBottom (y + offsetY)) |> ignore
            for (x, y) in prm do
                sb.AppendLine(sprintf "v %f %f %f" (x + offsetX) zTop (y + offsetY)) |> ignore
            
            for i = 0 to n - 1 do
                let nextI = (i + 1) % n
                let b1 = !vOffset + i
                let b2 = !vOffset + nextI
                let t1 = !vOffset + n + i
                let t2 = !vOffset + n + nextI
                sb.AppendLine(sprintf "f %d %d %d" b1 b2 t1) |> ignore
                sb.AppendLine(sprintf "f %d %d %d" b2 t2 t1) |> ignore
            
            sb.Append("f ") |> ignore
            for i = 0 to n - 1 do sb.Append(sprintf "%d " (!vOffset + n + i)) |> ignore
            sb.AppendLine() |> ignore
            sb.Append("f ") |> ignore
            for i = n - 1 downto 0 do sb.Append(sprintf "%d " (!vOffset + i)) |> ignore
            sb.AppendLine() |> ignore
            
            vOffset := !vOffset + 2 * n
    sb.ToString()

let generateObjBatch (batch: (Cxl[] * float[]) list) =
    let sb = StringBuilder()
    sb.AppendLine("# Hywe 3D Batch Export") |> ignore
    sb.AppendLine("g Batch") |> ignore
    
    let mutable vOff = ref 1
    let cols = 4
    batch |> List.iteri (fun i (cxls, elvs) ->
        let r = i / cols
        let c = i % cols
        let ox = float c * 100.0
        let oy = float r * -100.0
        sb.Append(generateObj cxls elvs ox oy vOff) |> ignore
    )
    sb.ToString()
