module FileManager

open System
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

/// Safely parse JSON into a JsonDocument using F#'s functional Async.Catch to avoid try...with.
let private tryParseJson (json: string) =
    match Async.RunSynchronously(Async.Catch(async { return JsonDocument.Parse(json) })) with
    | Choice1Of2 doc -> Some doc
    | Choice2Of2 _ -> None

/// Exports Map Extents or Terrain Grid from Topography JSON
let exportMapData (js: IJSRuntime) (topoJson: string) (exportType: string) =
    match tryParseJson topoJson with
    | Some doc ->
        use doc = doc
        let root = doc.RootElement
        
        match exportType, root.TryGetProperty("extents") |> fst, root.TryGetProperty("elevations") |> fst with
        | "extents", true, _ ->
            let content = root.GetProperty("extents").GetRawText()
            js.InvokeVoidAsync("downloadFile", "hywe-map-extents.json", content, "application/json") |> ignore
            
        | "terrain", _, true ->
            let elevationsProp = root.GetProperty("elevations")
            let elevationsArr = elevationsProp.EnumerateArray() |> Seq.toArray
            
            match elevationsArr.Length with
            | len when len > 0 ->
                let extents = root.GetProperty("extents")
                let north = extents.GetProperty("north").GetDouble()
                let south = extents.GetProperty("south").GetDouble()
                let east = extents.GetProperty("east").GetDouble()
                let west = extents.GetProperty("west").GetDouble()
                
                let gridSize = int (Math.Sqrt(float len))
                
                let lat0 = south // Bottom-left reference
                let lon0 = west
                
                let csvContent =
                    let header = "X,Y,Z\n"
                    let body =
                        Array.init len (fun idx ->
                            let i = idx / gridSize
                            let j = idx % gridSize
                            let lat = north - (north - south) * (float i / float (gridSize - 1))
                            let lon = west + (east - west) * (float j / float (gridSize - 1))
                            let ele = elevationsArr.[idx].GetDouble()
                            
                            let x = (lon - lon0) * 111320.0 * Math.Cos(lat0 * Math.PI / 180.0)
                            let y = (lat - lat0) * 111320.0
                            sprintf "%.2f,%.2f,%.2f" x y ele
                        )
                        |> String.concat "\n"
                    header + body + "\n"
                
                js.InvokeVoidAsync("downloadFile", "hywe-terrain-grid.csv", csvContent, "text/csv") |> ignore
            | _ -> ()
            
        | _ ->
            let fileName = match exportType with | "extents" -> "hywe-map-extents.json" | _ -> "hywe-terrain-grid.json"
            js.InvokeVoidAsync("downloadFile", fileName, topoJson, "application/json") |> ignore
    | None -> ()

/// Exports the current map view as a PNG image using Leaflet.
let exportMapImage (js: IJSRuntime) =
    js.InvokeVoidAsync("Hymap.exportMapImage") |> ignore

/// Traditional import
let importFile (js: IJSRuntime) (inputId: string) =
    js.InvokeAsync<string>("readHywFile", inputId)

/// <summary> Parses .hyw content and updates a PolygonEditorModel. </summary>
let importFromHyw (content: string) (current: PolygonEditorModel) : EditorState =
    let parsed = processFullString content |> List.truncate 1
    
    let processSegment state segment =
        let attrs = match segment with | Level l -> l.Attributes | Nest n -> n.Attributes
        let multiplier = 10.0
        
        let state1 = 
            { state with
                LogicalWidth = attrs.Width |> Option.map (fun num -> (max 10.0 num) * multiplier) |> Option.defaultValue state.LogicalWidth
                LogicalHeight = attrs.Height |> Option.map (fun num -> (max 10.0 num) * multiplier) |> Option.defaultValue state.LogicalHeight
                Elevation = attrs.Level
                UseAbsolute = (attrs.Scale = 1.0)
                UseBoundary = (attrs.Scale <> 1.0)
                UseMapBase = (attrs.Scale = 2.0)
            }

        let state2 = match parsePoint multiplier attrs.Entry with | Ok pt -> { state1 with EntryPoint = pt } | _ -> state1
        let state3 = match parsePoly multiplier attrs.OuterBoundary with | Ok pts -> { state2 with Outer = pts } | _ -> state2
        let state4 = match parseIslands multiplier attrs.Islands with | Ok pts -> { state3 with Islands = pts } | _ -> state3
        state4

    let baseState = 
        match List.tryHead parsed with
        | Some segment -> processSegment current segment
        | None -> current
    
    let isZeroBoundary = baseState.LogicalWidth <= 0.0 || baseState.LogicalHeight <= 0.0
    let isBoundary = not baseState.UseAbsolute && not isZeroBoundary
    
    let finalStateWithBoundary = 
        { baseState with 
            Outer = match Array.isEmpty baseState.Outer with | true -> initOuter | false -> baseState.Outer
            LogicalWidth = match baseState.LogicalWidth <= 0.0 with | true -> 300.0 | false -> baseState.LogicalWidth
            LogicalHeight = match baseState.LogicalHeight <= 0.0 with | true -> 300.0 | false -> baseState.LogicalHeight
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
    let header = "Orientation,Level,Rooms (ID Name Base Coordinates...)\n"
    let rows = 
        data |> Array.map (fun (sqn, elv, cxls) ->
            let roomStrings =
                cxls |> Array.map (fun cxl ->
                    let id = prpVlu cxl.Rfid
                    let name = prpVlu cxl.Name
                    let baseCoord = getBaseCoordStringDec cxl
                    let coords = getCxlCoordsStringDec cxl
                    sprintf "%s %s %s %s" id name baseCoord coords
                )
            sprintf "%s,%d,%s" sqn elv (String.concat "," roomStrings)
        )
    header + (String.concat "\n" rows) + "\n"

/// Generates a CSV of area metrics
let generateAreaMetricsCsv (data: (string * int * Cxl[])[]) =
    let hxlAreaX = 1
    let header = "Orientation,Level,CoxelID,CoxelName,Required,Achieved,TargetMet\n"
    let rows = 
        data |> Array.collect (fun (sqn, elv, cxls) ->
            cxls |> Array.map (fun cxl ->
                let reqSz = (prpVlu cxl.Size |> int) * hxlAreaX
                let achSz = (Array.length cxl.Hxls) * hxlAreaX
                let id = prpVlu cxl.Rfid
                let name = prpVlu cxl.Name
                let targetMet = match achSz >= reqSz with | true -> "Yes" | false -> "No"
                sprintf "%s,%d,%s,%s,%d,%d,%s" sqn elv id name reqSz achSz targetMet
            )
        )
    header + (String.concat "\n" rows) + "\n"

/// Generates a CSV for adjacency matrices
let generateAdjacencyCsv (data: (string * int * (string[] * bool[][]))[]) =
    data 
    |> Array.choose (fun (sqn, elv, (names, matrix)) ->
        match Array.isEmpty names with
        | false ->
            let header1 = sprintf "--- %s | Level %d ---" sqn elv
            let header2 = "Room," + String.concat "," names
            let rows =
                [0 .. matrix.Length - 1] 
                |> List.map (fun i ->
                    let row = matrix.[i]
                    names.[i] + "," + String.concat "," (row |> Array.map (fun adj -> match adj with | true -> "1" | false -> "0"))
                )
            Some (String.concat "\n" (header1 :: header2 :: rows) + "\n")
        | true -> None
    )
    |> String.concat "\n"

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
                match p = c with
                | true -> false
                | false ->
                    let pCoords = parentCoordsMap.[p]
                    let (cx, cy, _) = hxlCrd c.Base
                    pCoords.Contains(cx, cy) && (Array.append [|p.Base|] p.Hxls).Length > (Array.append [|c.Base|] c.Hxls).Length
            )

        let findHost (c: Cxl) =
            levelCxls
            |> Array.tryFind (fun p ->
                match p = c with
                | true -> false
                | false ->
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
    let hexScale = 1.0
    cxls 
    |> Array.choose (fun cxl ->
        let prm = cxlPrm cxl 0
        match prm.Length > 2 with
        | true ->
            let header = [
                "0"; "LWPOLYLINE"
                "8"; "Rooms"
                "90"; string prm.Length
                "70"; "1"
            ]
            let points = 
                prm |> Array.collect (fun (x, y) ->
                    let q = float x
                    let r = float y
                    let cx = (hexScale * q) + offsetX
                    let cy = (hexScale * r) + offsetY
                    [| "10"; string cx; "20"; string cy |]
                ) |> Array.toList
            Some (String.concat "\n" (header @ points))
        | false -> None
    )
    |> function
        | [||] -> ""
        | arr -> (String.concat "\n" arr) + "\n"

let generateDxfBatch (batch: Cxl[] list) =
    let header = [
        "0"; "SECTION"; "2"; "HEADER"; "0"; "ENDSEC"
        "0"; "SECTION"; "2"; "TABLES"; "0"; "TABLE"; "2"; "LAYER"; "70"; "1"
        "0"; "LAYER"; "2"; "Rooms"; "70"; "0"; "62"; "7"; "0"; "ENDTAB"; "0"; "ENDSEC"
        "0"; "SECTION"; "2"; "BLOCKS"; "0"; "ENDSEC"
        "0"; "SECTION"; "2"; "ENTITIES"
    ]
    let cols = 4
    let bodies = 
        batch |> List.mapi (fun i cxls ->
            let r = i / cols
            let c = i % cols
            let ox = float c * 100.0
            let oy = float r * -100.0
            generateDxf cxls ox oy
        )
    let footer = [ "0"; "ENDSEC"; "0"; "EOF" ]
    
    String.concat "\n" (header @ bodies @ footer) + "\n"

/// OBJ Export (3D Geometry)
let generateObj (cxls: Cxl[]) (elevations: float[]) (offsetX: float) (offsetY: float) (vOffset: int) =
    let foldObj (currentOffset: int, accStrings: string list) (cxl: Cxl) =
        let (_, _, zInt) = Hexel.hxlCrd cxl.Base
        let zBottom = match zInt < elevations.Length with | true -> elevations.[zInt] | false -> float zInt * 3.0
        let zTop = match zInt + 1 < elevations.Length with | true -> elevations.[zInt + 1] | false -> zBottom + 3.0
        
        let prm = cxlPrm cxl zInt
        let n = prm.Length
        match n > 2 with
        | true ->
            let vBottoms = prm |> Array.map (fun (x, y) -> sprintf "v %f %f %f" (float x + offsetX) zBottom (float y + offsetY)) |> Array.toList
            let vTops = prm |> Array.map (fun (x, y) -> sprintf "v %f %f %f" (float x + offsetX) zTop (float y + offsetY)) |> Array.toList
            
            let faces = 
                [0 .. n - 1] |> List.collect (fun i ->
                    let nextI = (i + 1) % n
                    let b1 = currentOffset + i
                    let b2 = currentOffset + nextI
                    let t1 = currentOffset + n + i
                    let t2 = currentOffset + n + nextI
                    [ sprintf "f %d %d %d" b1 b2 t1; sprintf "f %d %d %d" b2 t2 t1 ]
                )
            
            let topFace = "f " + ([0 .. n - 1] |> List.map (fun i -> string (currentOffset + n + i)) |> String.concat " ")
            let bottomFace = "f " + ([n - 1 .. -1 .. 0] |> List.map (fun i -> string (currentOffset + i)) |> String.concat " ")
            
            let newLines = vBottoms @ vTops @ faces @ [topFace; bottomFace]
            (currentOffset + 2 * n, accStrings @ newLines)
        | false -> (currentOffset, accStrings)
        
    let finalOffset, lines = ((vOffset, []), cxls) ||> Array.fold foldObj
    (finalOffset, match lines with | [] -> "" | _ -> String.concat "\n" lines + "\n")

let generateObjBatch (batch: (Cxl[] * float[]) list) =
    let header = "# Hywe 3D Batch Export\ng Batch\n"
    
    let cols = 4
    let _, bodyStrings = 
        ((1, []), batch |> List.indexed)
        ||> List.fold (fun (vOff, acc) (i, (cxls, elvs)) ->
            let r = i / cols
            let c = i % cols
            let ox = float c * 100.0
            let oy = float r * -100.0
            let nextOff, str = generateObj cxls elvs ox oy vOff
            (nextOff, acc @ [str])
        )
        
    header + String.concat "" bodyStrings


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
        match String.IsNullOrWhiteSpace content with
        | false ->
            setBackup js content
            let p = panelToString panel
            let hash = sprintf "%s|P=%s" content p
            setUrlHash js hash
        | true ->
            setUrlHash js ""

    /// Parses a raw hash string (already decoded) into (content, panel option, isFromUrl).
    /// Used synchronously by HandleHashChange when the URL changes in an already-running app.
    let resolveHashChange (rawHash: string) : string * ActivePanel option * bool =
        match String.IsNullOrWhiteSpace rawHash with
        | true -> "", None, false
        | false ->
            let upperHash = rawHash.ToUpperInvariant()
            let pIdx = 
                match upperHash.LastIndexOf("|P=") with
                | -1 -> 
                    match upperHash.LastIndexOf("%7CP=") with
                    | -1 -> -1
                    | i2 -> i2
                | i1 -> i1
            match pIdx with
            | -1 -> rawHash, None, true
            | _ ->
                let isStandardDelim = upperHash.Substring(pIdx).StartsWith("|P=")
                let delimLength = match isStandardDelim with | true -> 3 | false -> 5
                
                match pIdx >= 0 && pIdx + delimLength <= rawHash.Length with
                | true ->
                    let c = rawHash.Substring(0, pIdx)
                    let pName = rawHash.Substring(pIdx + delimLength)
                    c, stringToPanel pName, true
                | false -> rawHash, None, true

    /// Orchestrates the startup state resolution.
    let resolveStartupState (js: IJSRuntime) =
        async {
            let! hashAttempt = Async.Catch ((getUrlHash js).AsTask() |> Async.AwaitTask)
            match hashAttempt with
            | Choice1Of2 urlHash when not (String.IsNullOrWhiteSpace urlHash) ->
                let upperHash = urlHash.ToUpperInvariant()
                let pIdx = 
                    match upperHash.LastIndexOf("|P=") with
                    | -1 -> 
                        match upperHash.LastIndexOf("%7CP=") with
                        | -1 -> -1
                        | i2 -> i2
                    | i1 -> i1
                
                match pIdx with
                | -1 -> return urlHash, None, true // Source: URL
                | _ ->
                    let isStandardDelim = upperHash.Substring(pIdx).StartsWith("|P=")
                    let delimLength = match isStandardDelim with | true -> 3 | false -> 5
                    
                    match pIdx >= 0 && pIdx + delimLength <= urlHash.Length with
                    | true ->
                        let c = urlHash.Substring(0, pIdx)
                        let pName = urlHash.Substring(pIdx + delimLength)
                        return c, stringToPanel pName, true
                    | false -> return urlHash, None, true
            | _ ->
                let! backupAttempt = Async.Catch ((getBackup js).AsTask() |> Async.AwaitTask)
                match backupAttempt with
                | Choice1Of2 backup when not (isNull backup) && not (String.IsNullOrWhiteSpace backup) ->
                    return backup, None, false // Source: Local
                | _ -> return "", None, false
        }

    /// Clears the local backup safely.
    let purgeLocalBackup (js: IJSRuntime) =
        clearBackup js
