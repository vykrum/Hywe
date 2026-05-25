module FileManager

open System
open System.Text
open System.Text.Json
open Microsoft.JSInterop
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Lexel
open Hywe.Site
open Hywe.Site.State
open ModelTypes

// --- FILE IMPORT/EXPORT ---

/// Generates timestamped filename and triggers download
let saveFile (js: IJSRuntime) (content: string) =
    let timestamp = DateTime.Now.ToString("yyMMddHHmm")
    let fileName = sprintf "%s.hyw" timestamp
    js.InvokeVoidAsync("downloadFile", fileName, content, "application/octet-stream") |> ignore

/// Exports Map Extents or Terrain Grid from Topography JSON
let exportMapData (js: IJSRuntime) (topoJson: string) (exportType: string) =
    try
        use doc = JsonDocument.Parse(topoJson)
        let root = doc.RootElement
        if exportType = "extents" && root.TryGetProperty("extents") |> fst then
            let content = root.GetProperty("extents").GetRawText()
            js.InvokeVoidAsync("downloadFile", "hywe-map-extents.json", content, "application/json") |> ignore
        elif exportType = "terrain" && root.TryGetProperty("points") |> fst then
            let content = root.GetProperty("points").GetRawText()
            js.InvokeVoidAsync("downloadFile", "hywe-terrain-grid.json", content, "application/json") |> ignore
        else
            let fileName = if exportType = "extents" then "hywe-map-extents.json" else "hywe-terrain-grid.json"
            js.InvokeVoidAsync("downloadFile", fileName, topoJson, "application/json") |> ignore
    with _ ->
        ()

/// Traditional import
let importFile (js: IJSRuntime) (inputId: string) =
    js.InvokeAsync<string>("readHywFile", inputId)

/// <summary> Parses .hyw content and updates a PolygonEditorModel. </summary>
let importFromHyw (content: string) (current: PolygonEditorModel) : EditorState =
    let parsed = processFullString content |> List.truncate 1
    
    let mutable finalState = current
    match List.tryHead parsed with
    | Some segment ->
        let attrs = match segment with | Level l -> l.Attributes | Nest n -> n.Attributes
        finalState <- 
            { finalState with
                LogicalWidth = attrs.Width |> Option.map (fun num -> (max 10.0 num) * 10.0) |> Option.defaultValue finalState.LogicalWidth
                LogicalHeight = attrs.Height |> Option.map (fun num -> (max 10.0 num) * 10.0) |> Option.defaultValue finalState.LogicalHeight
                Elevation = attrs.Level
                BaseStr = "" // Not directly in attributes now, usually handled by nodes
                UseAbsolute = (attrs.Scale = 1.0)
                UseBoundary = (attrs.Scale <> 1.0)
            }

        // Handle Entry point
        match parsePoint attrs.Entry with
        | Ok pt -> finalState <- { finalState with EntryPoint = pt }
        | _ -> ()

        // Handle Outer boundary
        match parsePoly attrs.OuterBoundary with
        | Ok pts -> finalState <- { finalState with Outer = pts }
        | _ -> ()

        // Handle Islands
        match parseIslands attrs.Islands with
        | Ok pts -> finalState <- { finalState with Islands = pts }
        | _ -> ()
    | None -> ()
    
    let isZeroBoundary = finalState.LogicalWidth <= 0.0 || finalState.LogicalHeight <= 0.0
    
    let finalStateWithBoundary = 
        let isBoundary = not finalState.UseAbsolute && not isZeroBoundary
        { finalState with 
            Outer = if Array.isEmpty finalState.Outer then initOuter else finalState.Outer
            LogicalWidth = if finalState.LogicalWidth <= 0.0 then 300.0 else finalState.LogicalWidth
            LogicalHeight = if finalState.LogicalHeight <= 0.0 then 300.0 else finalState.LogicalHeight
            UseBoundary = isBoundary
            PolygonEnabled = isBoundary }
        |> refreshCachedStrings

    FreshlyImported finalStateWithBoundary

// --- EXPORT FORMATS ---

let private getCxlCoordsStringDec (cxl: Cxl) =
    Array.append [|cxl.Base|] cxl.Hxls 
    |> Array.map (fun h -> 
        let (x, y, _) = hxlCrd h
        sprintf "%d.%d" x y)
    |> String.concat " "

let private getBaseCoordStringDec (cxl: Cxl) =
    let (x, y, _) = hxlCrd cxl.Base
    sprintf "%d.%d" x y

/// Generates a CSV of all Coxel coordinates
let generateCoordinatesCsv (data: (string * int * Cxl[])[]) =
    let sb = StringBuilder()
    sb.AppendLine("Orientation,Level,Rooms (ID Name Base Coordinates...)") |> ignore
    for (sqn, elv, cxls) in data do
        let roomStrings =
            cxls |> Array.map (fun cxl ->
                let id = prpVlu cxl.Rfid
                let name = prpVlu cxl.Name
                let baseCoord = getBaseCoordStringDec cxl
                let coords = getCxlCoordsStringDec cxl
                sprintf "%s %s %s %s" id name baseCoord coords
            )
        let rowStr = sprintf "%s,%d,%s" sqn elv (String.concat "," roomStrings)
        sb.AppendLine(rowStr) |> ignore
    sb.ToString()

/// Generates a CSV of area metrics
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

/// Generates a CSV for adjacency matrices
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

/// Generates Hynteract payload
let generateHynteractPayloadFromCxls (cxls: Cxl[]) =
    let getCxlCoordsString (cxl: Cxl) =
        Array.append [|cxl.Base|] cxl.Hxls 
        |> Array.map (fun h -> 
            let (x, y, _) = hxlCrd h
            sprintf "%d,%d" x y)
        |> String.concat " "

    cxls
    |> Array.groupBy (fun cxl -> let (_, _, z) = hxlCrd cxl.Base in z)
    |> Array.sortBy fst
    |> Array.map (fun (_, levelCxls) ->
        let parentCoordsMap =
            levelCxls
            |> Array.map (fun c ->
                let coords = 
                    Array.append [| c.Base |] c.Hxls
                    |> Array.map (fun h -> let (x, y, _) = hxlCrd h in x, y)
                    |> Set.ofArray
                c, coords)
            |> Map.ofArray

        let isNested (c: Cxl) =
            levelCxls
            |> Array.exists (fun p ->
                if p = c then false
                else
                    let pCoords = parentCoordsMap.[p]
                    let (cx, cy, _) = hxlCrd c.Base
                    pCoords.Contains(cx, cy) && (Array.append [|p.Base|] p.Hxls).Length > (Array.append [|c.Base|] c.Hxls).Length
            )

        let findHost (c: Cxl) =
            levelCxls
            |> Array.tryFind (fun p ->
                if p = c then false
                else
                    let pCoords = parentCoordsMap.[p]
                    let (cx, cy, _) = hxlCrd c.Base
                    pCoords.Contains(cx, cy) && (Array.append [|p.Base|] p.Hxls).Length > (Array.append [|c.Base|] c.Hxls).Length
            )

        let topLevels = levelCxls |> Array.filter (fun c -> not (isNested c))
        let nestedGroups = 
            levelCxls 
            |> Array.filter isNested
            |> Array.groupBy findHost
            |> Array.map (fun (hostOpt, children) ->
                let childrenStr = 
                    children 
                    |> Array.map getCxlCoordsString 
                    |> String.concat ";"
                hostOpt, sprintf "{%s}" childrenStr
            )
            |> Map.ofArray

        let parts = 
            topLevels
            |> Array.map (fun host ->
                let hostStr = getCxlCoordsString host
                match nestedGroups |> Map.tryFind (Some host) with
                | Some nestStr -> hostStr + ";" + nestStr
                | None -> hostStr
            )
        
        let orphanNests =
            nestedGroups
            |> Map.toList
            |> List.filter (fun (hostOpt, _) -> hostOpt.IsNone)
            |> List.map snd

        let allParts = Array.append parts (List.toArray orphanNests)
        allParts |> String.concat ";"
    )
    |> String.concat "|"

/// DXF Export (2D Layout)
let generateDxf (cxls: Cxl[]) (offsetX: float) (offsetY: float) =
    let sb = StringBuilder()   
    let hexScale = 1.0
    for cxl in cxls do
        let prm = cxlPrm cxl 0
        if prm.Length > 2 then
            sb.AppendLine("0\nLWPOLYLINE") |> ignore
            sb.AppendLine("8\nRooms") |> ignore
            sb.AppendLine("90\n" + string prm.Length) |> ignore
            sb.AppendLine("70\n1") |> ignore           
            for (x, y) in prm do
                let q = float x
                let r = float y
                let cx = (hexScale * q) + offsetX
                let cy = (hexScale * r) + offsetY
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
        let ox = float c * 100.0
        let oy = float r * -100.0
        sb.Append(generateDxf cxls ox oy) |> ignore
    )
    sb.AppendLine("0\nENDSEC") |> ignore
    sb.AppendLine("0\nEOF") |> ignore
    sb.ToString()

/// OBJ Export (3D Geometry)
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
                sb.AppendLine(sprintf "v %f %f %f" (float x + offsetX) zBottom (float y + offsetY)) |> ignore
            for (x, y) in prm do
                sb.AppendLine(sprintf "v %f %f %f" (float x + offsetX) zTop (float y + offsetY)) |> ignore
            
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
    
    let vOff = ref 1
    let cols = 4
    batch |> List.iteri (fun i (cxls, elvs) ->
        let r = i / cols
        let c = i % cols
        let ox = float c * 100.0
        let oy = float r * -100.0
        sb.Append(generateObj cxls elvs ox oy vOff) |> ignore
    )
    sb.ToString()


// --- PROTOCOL (State Transfer & Persistence) ---

module Protocol =
    
    /// Low-level JS interop for URL and LocalStorage
    let private setUrlHash (js: IJSRuntime) (content: string) =
        js.InvokeVoidAsync("setUrlHash", content) |> ignore

    let private getUrlHash (js: IJSRuntime) =
        js.InvokeAsync<string>("getUrlHash")

    let private setBackup (js: IJSRuntime) (content: string) =
        js.InvokeVoidAsync("localStorage.setItem", "hywe_backup", content) |> ignore

    let private getBackup (js: IJSRuntime) =
        js.InvokeAsync<string>("localStorage.getItem", "hywe_backup")

    let private clearBackup (js: IJSRuntime) =
        js.InvokeVoidAsync("localStorage.removeItem", "hywe_backup") |> ignore

    let panelToString = function
        | BoundaryPanel -> "boundary"
        | LayoutPanel -> "layout"
        | AnalyzePanel -> "analyze"
        | ViewPanel -> "3d"
        | BatchPanel -> "batch"
        | TeachPanel -> "teach"
        | ReportPanel -> "report"

    let stringToPanel (s: string) = 
        match s.Trim().ToLower() with
        | "boundary" -> Some BoundaryPanel
        | "layout" -> Some LayoutPanel
        | "analyze" -> Some AnalyzePanel
        | "view" | "3d" -> Some ViewPanel
        | "teach" -> Some TeachPanel
        | "report" -> Some ReportPanel
        | "batch" -> Some BatchPanel
        | _ -> None

    /// Synchronizes the current design state to both LocalStorage and the URL Hash.
    let sync (js: IJSRuntime) (content: string) (panel: ActivePanel) =
        if not (String.IsNullOrWhiteSpace content) then
            setBackup js content
            let p = panelToString panel
            let hash = sprintf "%s|P=%s" content p
            setUrlHash js hash
        else
            setUrlHash js ""

    /// Parses a raw hash string (already decoded) into (content, panel option, isFromUrl).
    /// Used synchronously by HandleHashChange when the URL changes in an already-running app.
    let resolveHashChange (rawHash: string) : string * ActivePanel option * bool =
        if String.IsNullOrWhiteSpace rawHash then "", None, false
        else
            try
                let upperHash = rawHash.ToUpperInvariant()
                let pIdx = 
                    let i1 = upperHash.LastIndexOf("|P=")
                    if i1 <> -1 then i1 
                    else 
                        let i2 = upperHash.LastIndexOf("%7CP=")
                        if i2 <> -1 then i2 else -1
                if pIdx <> -1 then
                    let delimLength = if upperHash.Substring(pIdx).StartsWith("|P=") then 3 else 5
                    let c = rawHash.Substring(0, pIdx)
                    let pName = rawHash.Substring(pIdx + delimLength)
                    // console.log("Hywe: resolveHashChange", pIdx, delimLength, pName)
                    c, stringToPanel pName, true
                else
                    rawHash, None, true
            with _ -> rawHash, None, true

    /// Orchestrates the startup state resolution.
    let resolveStartupState (js: IJSRuntime) =
        async {
            try
                // 1. Try URL Hash first
                let! urlHash = (getUrlHash js).AsTask() |> Async.AwaitTask
                if not (String.IsNullOrWhiteSpace urlHash) then 
                    // Case-insensitive search for |P= or %7CP=
                    let upperHash = urlHash.ToUpperInvariant()
                    let pIdx = 
                        let i1 = upperHash.LastIndexOf("|P=")
                        if i1 <> -1 then i1 
                        else 
                            let i2 = upperHash.LastIndexOf("%7CP=")
                            if i2 <> -1 then i2 else -1
                    
                    if pIdx <> -1 then
                        // Calculate content length (excluding the delimiter)
                        let delimLength = if upperHash.Substring(pIdx).StartsWith("|P=") then 3 else 5
                        let c = urlHash.Substring(0, pIdx)
                        let pName = urlHash.Substring(pIdx + delimLength)
                        return c, stringToPanel pName, true
                    else
                        return urlHash, None, true // Source: URL
                else
                    // 2. Fallback to Local Backup
                    let! backup = (getBackup js).AsTask() |> Async.AwaitTask
                    if not (isNull backup) && not (String.IsNullOrWhiteSpace backup) then
                        return backup, None, false // Source: Local
                    else
                        return "", None, false
            with _ -> return "", None, false
        }

    /// Clears the local backup safely.
    let purgeLocalBackup (js: IJSRuntime) =
        clearBackup js
