/// <summary>
/// Provides file import/export, map visualization export, CSV metrics generation,
/// state persistence, and URL hash synchronization for the Hywe web application.
/// </summary>
module FileManager

open System
open System.Text.Json
open Microsoft.JSInterop
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Lexel
open Types
open State
open ModelTypes

// --- FILE IMPORT/EXPORT ---

/// <summary>
/// Generates a timestamped filename (e.g., YYMMDDHHmm.hyw) and triggers a file download in the browser via JS interop.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <param name="content">The text content of the file to download.</param>
let saveFile (js: IJSRuntime) (content: string) =
    let timestamp = DateTime.Now.ToString("yyMMddHHmm")
    let fileName = sprintf "%s.hyw" timestamp
    js.InvokeVoidAsync("downloadFile", fileName, content, "application/octet-stream") |> ignore

/// <summary>
/// Safely attempts to parse a JSON string into a <see cref="JsonDocument"/>.
/// </summary>
/// <param name="json">The raw JSON string to parse.</param>
/// <returns><c>Some JsonDocument</c> if parsing succeeds; otherwise <c>None</c>.</returns>
let private tryParseJson (json: string) =
    try
        Some (JsonDocument.Parse(json))
    with _ ->
        None

/// <summary>
/// Safely extracts a JSON property from a <see cref="JsonElement"/> as an option.
/// </summary>
let private tryGetProperty (propertyName: string) (element: JsonElement) =
    match element.TryGetProperty(propertyName) with
    | true, prop -> Some prop
    | false, _ -> None

/// <summary>
/// Purely transforms topography extents and elevation elements into a CSV grid string.
/// </summary>
let private tryBuildTerrainCsv (extents: JsonElement) (elevationsProp: JsonElement) =
    let elevations = elevationsProp.EnumerateArray() |> Seq.map (fun e -> e.GetDouble()) |> Seq.toArray
    if Array.isEmpty elevations then None
    else
        let north = extents.GetProperty("north").GetDouble()
        let south = extents.GetProperty("south").GetDouble()
        let east = extents.GetProperty("east").GetDouble()
        let west = extents.GetProperty("west").GetDouble()
        
        let gridSize = int (Math.Sqrt(float elevations.Length))
        let gridSizeDiv = float (max 1 (gridSize - 1))
        
        let lat0 = south
        let lon0 = west
        
        let formatPoint idx ele =
            let i = idx / gridSize
            let j = idx % gridSize
            let lat = north - (north - south) * (float i / gridSizeDiv)
            let lon = west + (east - west) * (float j / gridSizeDiv)
            let x = (lon - lon0) * 111320.0 * Math.Cos(lat0 * Math.PI / 180.0)
            let y = (lat - lat0) * 111320.0
            sprintf "%.2f,%.2f,%.2f" x y ele

        let rows = elevations |> Array.mapi formatPoint |> String.concat "\n"
        Some ("X,Y,Z\n" + rows + "\n")

/// <summary>
/// Purely resolves export filename, content payload, and MIME type based on Topography JSON and export type.
/// </summary>
let private resolveExportPayload (topoJson: string) (exportType: string) =
    match tryParseJson topoJson with
    | Some doc ->
        use doc = doc
        let root = doc.RootElement
        match exportType with
        | "extents" ->
            match tryGetProperty "extents" root with
            | Some extentsProp -> ("hywe-map-extents.json", extentsProp.GetRawText(), "application/json")
            | None -> ("hywe-map-extents.json", topoJson, "application/json")

        | "terrain" ->
            match tryGetProperty "extents" root, tryGetProperty "elevations" root with
            | Some extents, Some elevationsProp ->
                match tryBuildTerrainCsv extents elevationsProp with
                | Some csvContent -> ("hywe-terrain-grid.csv", csvContent, "text/csv")
                | None -> ("hywe-terrain-grid.json", topoJson, "application/json")
            | _ ->
                match tryGetProperty "points" root with
                | Some pointsProp -> ("hywe-terrain-grid.json", pointsProp.GetRawText(), "application/json")
                | None -> ("hywe-terrain-grid.json", topoJson, "application/json")

        | _ ->
            let fileName = if exportType = "extents" then "hywe-map-extents.json" else "hywe-terrain-grid.json"
            (fileName, topoJson, "application/json")

    | None ->
        let fileName = if exportType = "extents" then "hywe-map-extents.json" else "hywe-terrain-grid.json"
        (fileName, topoJson, "application/json")

/// <summary>
/// Exports Map Extents or Terrain Grid from Topography JSON data to downloadable JSON or CSV files.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <param name="topoJson">The JSON string containing topography data.</param>
/// <param name="exportType">The export mode: "extents" for map bounds, "terrain" for terrain grid CSV/JSON.</param>
let exportMapData (js: IJSRuntime) (topoJson: string) (exportType: string) =
    try
        if not (String.IsNullOrWhiteSpace topoJson) then
            let (fileName, content, mimeType) = resolveExportPayload topoJson exportType
            js.InvokeVoidAsync("downloadFile", fileName, content, mimeType) |> ignore
    with _ -> ()

/// <summary>
/// Exports the current Leaflet map view as a PNG image by invoking JS interop <c>Hymap.exportMapImage</c>.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
let exportMapImage (js: IJSRuntime) =
    js.InvokeVoidAsync("Hymap.exportMapImage") |> ignore

/// <summary>
/// Initiates file reading from an HTML file input element via JS interop <c>readHywFile</c>.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <param name="inputId">The DOM element ID of the file input control.</param>
/// <returns>A <see cref="ValueTask{T}"/> resolving to the raw string content of the uploaded file.</returns>
let importFile (js: IJSRuntime) (inputId: string) =
    js.InvokeAsync<string>("readHywFile", inputId)

// --- HYW IMPORT PARSING ---

let private extractSegmentAttributes = function
    | Level l -> l.Attributes
    | Nest n -> n.Attributes

let private updateEntryPoint multiplier entry (state: PolygonEditorModel) =
    match parsePoint multiplier entry with
    | Ok pt -> { state with EntryPoint = pt }
    | Error _ -> state

let private updateOuterBoundary multiplier outer (state: PolygonEditorModel) =
    match parsePoly multiplier outer with
    | Ok pts when not (Array.isEmpty pts) -> { state with Outer = pts }
    | _ -> state

let private updateIslands multiplier islands (state: PolygonEditorModel) =
    match parseIslands multiplier islands with
    | Ok pts -> { state with Islands = pts }
    | Error _ -> state

let private processSegment multiplier (state: PolygonEditorModel) segment =
    let attrs = extractSegmentAttributes segment
    let hasPolygons = not (String.IsNullOrWhiteSpace attrs.OuterBoundary) || not (String.IsNullOrWhiteSpace attrs.Islands)
    
    { state with
        LogicalWidth = attrs.Width |> Option.map (fun num -> (max 10.0 num) * multiplier) |> Option.defaultValue state.LogicalWidth
        LogicalHeight = attrs.Height |> Option.map (fun num -> (max 10.0 num) * multiplier) |> Option.defaultValue state.LogicalHeight
        Elevation = attrs.Level
        UseAbsolute = if hasPolygons then false else (attrs.Scale = 1.0)
        UseBoundary = if hasPolygons then true else (attrs.Scale <> 1.0)
        UseMapBase = (attrs.Scale = 2.0) }
    |> updateEntryPoint multiplier attrs.Entry
    |> updateOuterBoundary multiplier attrs.OuterBoundary
    |> updateIslands multiplier attrs.Islands

/// <summary>
/// Parses .hyw format content and updates a <see cref="PolygonEditorModel"/> state, returned as <see cref="FreshlyImported"/>.
/// </summary>
/// <param name="content">The raw .hyw structured string content.</param>
/// <param name="current">The current polygon editor model to serve as the default baseline state.</param>
/// <returns>An <see cref="EditorState"/> initialized with the parsed file data.</returns>
let importFromHyw (content: string) (current: PolygonEditorModel) : EditorState =
    let multiplier = 10.0
    let parsedSegments = processFullString content |> List.truncate 1
    
    let (baseState: PolygonEditorModel) = 
        match List.tryHead parsedSegments with
        | Some segment -> processSegment multiplier current segment
        | None -> current
    
    let hasExplicitPolygons =
        parsedSegments
        |> List.tryHead
        |> Option.map (extractSegmentAttributes >> (fun attrs -> not (String.IsNullOrWhiteSpace attrs.OuterBoundary) || not (String.IsNullOrWhiteSpace attrs.Islands)))
        |> Option.defaultValue false

    let isZeroBoundary = baseState.LogicalWidth <= 0.0 || baseState.LogicalHeight <= 0.0
    let isBoundary = (not baseState.UseAbsolute || hasExplicitPolygons) && not isZeroBoundary
    
    { baseState with 
        Outer = if Array.isEmpty baseState.Outer then initOuter else baseState.Outer
        LogicalWidth = if baseState.LogicalWidth <= 0.0 then 300.0 else baseState.LogicalWidth
        LogicalHeight = if baseState.LogicalHeight <= 0.0 then 300.0 else baseState.LogicalHeight
        UseBoundary = isBoundary
        PolygonEnabled = isBoundary }
    |> refreshCachedStrings
    |> FreshlyImported

// --- EXPORT FORMATS ---

/// <summary>
/// Formats all hexel coordinates within a Coxel into space-separated "X.Y" coordinate strings.
/// </summary>
/// <param name="cxl">The Coxel data structure instance.</param>
/// <returns>Space-separated string of hexel coordinates.</returns>
let private getCxlCoordsStringDec (cxl: Cxl) =
    Array.append [|cxl.Base|] cxl.Hxls 
    |> Array.map (fun h -> 
        let (x, y, _) = hxlCrd h
        sprintf "%d.%d" x y)
    |> String.concat " "

/// <summary>
/// Formats the base hexel coordinate of a Coxel as an "X.Y" coordinate string.
/// </summary>
/// <param name="cxl">The Coxel data structure instance.</param>
/// <returns>String formatted as "X.Y".</returns>
let private getBaseCoordStringDec (cxl: Cxl) =
    let (x, y, _) = hxlCrd cxl.Base
    sprintf "%d.%d" x y

/// <summary>
/// Generates a CSV string containing Coxel coordinates categorized by orientation and floor level.
/// </summary>
/// <param name="data">Array of tuples consisting of orientation name, level index, and array of Coxels.</param>
/// <returns>CSV formatted string of layout coordinates.</returns>
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

/// <summary>
/// Generates a CSV string of area metrics (required vs. achieved area) for each Coxel across orientations and levels.
/// </summary>
/// <param name="data">Array of tuples consisting of orientation name, level index, and array of Coxels.</param>
/// <returns>CSV formatted string of area metrics.</returns>
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
                let targetMet = if achSz >= reqSz then "Yes" else "No"
                sprintf "%s,%d,%s,%s,%d,%d,%s" sqn elv id name reqSz achSz targetMet
            )
        )
    header + (String.concat "\n" rows) + "\n"

/// <summary>
/// Generates a CSV string representing adjacency matrices between rooms for each level and orientation.
/// </summary>
/// <param name="data">Array of tuples containing orientation name, level index, room names array, and 2D adjacency matrix.</param>
/// <returns>Formatted CSV string of spatial adjacency data.</returns>
let generateAdjacencyCsv (data: (string * int * (string[] * bool[][]))[]) =
    data 
    |> Array.choose (fun (sqn, elv, (names, matrix)) ->
        if Array.isEmpty names then None
        else
            let header1 = sprintf "--- %s | Level %d ---" sqn elv
            let header2 = "Room," + String.concat "," names
            let matrixRows =
                matrix
                |> Array.mapi (fun i row ->
                    let rowVals = row |> Array.map (fun adj -> if adj then "1" else "0") |> String.concat ","
                    sprintf "%s,%s" names.[i] rowVals
                )
                |> Array.toList
            Some (String.concat "\n" (header1 :: header2 :: matrixRows) + "\n")
    )
    |> String.concat "\n"

/// <summary>
/// Generates a serialized Hynteract payload string encoding room geometries and parent-child nesting hierarchies by elevation level.
/// </summary>
/// <param name="cxls">Array of Coxel instances to serialize.</param>
/// <returns>Pipe-separated (|) levels with semicolon-delimited (;) Coxel boundary coordinates and nesting structures.</returns>
let generateHynteractPayloadFromCxls (cxls: Cxl[]) =
    let getCxlCoords (cxl: Cxl) =
        Array.append [|cxl.Base|] cxl.Hxls 
        |> Array.map (fun h -> let (x, y, _) = hxlCrd h in x, y)

    let getCxlCoordsString (cxl: Cxl) =
        getCxlCoords cxl
        |> Array.map (fun (x, y) -> sprintf "%d,%d" x y)
        |> String.concat " "

    cxls
    |> Array.groupBy (fun cxl -> let (_, _, z) = hxlCrd cxl.Base in z)
    |> Array.sortBy fst
    |> Array.map (fun (_, levelCxls) ->
        let parentCoordsMap =
            levelCxls
            |> Array.map (fun c -> c, getCxlCoords c |> Set.ofArray)
            |> Map.ofArray

        let cxlLengthsMap =
            levelCxls
            |> Array.map (fun c -> c, Array.length c.Hxls + 1)
            |> Map.ofArray

        let findHost (c: Cxl) =
            let (cx, cy, _) = hxlCrd c.Base
            let cLen = cxlLengthsMap.[c]
            levelCxls
            |> Array.tryFind (fun p ->
                p <> c &&
                cxlLengthsMap.[p] > cLen &&
                parentCoordsMap.[p].Contains(cx, cy)
            )

        let isNested (c: Cxl) = findHost c |> Option.isSome

        let topLevels = levelCxls |> Array.filter (not << isNested)
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
                match Map.tryFind (Some host) nestedGroups with
                | Some nestStr -> sprintf "%s;%s" hostStr nestStr
                | None -> hostStr
            )
        
        let orphanNests =
            nestedGroups
            |> Map.toList
            |> List.choose (function (None, nestStr) -> Some nestStr | _ -> None)

        Array.append parts (List.toArray orphanNests)
        |> String.concat ";"
    )
    |> String.concat "|"

// --- PROTOCOL (State Transfer & Persistence) ---

/// <summary>
/// Updates the browser's URL hash fragment via JS interop <c>setUrlHash</c>.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <param name="content">The raw hash string content to set.</param>
let private setUrlHash (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("setUrlHash", content) |> ignore

/// <summary>
/// Retrieves the browser's current URL hash fragment via JS interop <c>getUrlHash</c>.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <returns>A <see cref="ValueTask{T}"/> resolving to the URL hash string.</returns>
let private getUrlHash (js: IJSRuntime) =
    js.InvokeAsync<string>("getUrlHash")

/// <summary>
/// Saves design state data into browser LocalStorage under key <c>hywe_backup</c>.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <param name="content">The state content string to back up.</param>
let private setBackup (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("localStorage.setItem", "hywe_backup", content) |> ignore

/// <summary>
/// Retrieves the design state backup from browser LocalStorage.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <returns>A <see cref="ValueTask{T}"/> resolving to the backup string, or null if empty.</returns>
let private getBackup (js: IJSRuntime) =
    js.InvokeAsync<string>("localStorage.getItem", "hywe_backup")

/// <summary>
/// Removes the design state backup from browser LocalStorage.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
let private clearBackup (js: IJSRuntime) =
    js.InvokeVoidAsync("localStorage.removeItem", "hywe_backup") |> ignore

/// <summary>
/// Converts an <see cref="ActivePanel"/> union case into its corresponding URL panel identifier string.
/// </summary>
/// <param name="panel">The active panel value.</param>
/// <returns>String representation of the panel key (e.g. "boundary", "layout", "analyze", "3d", "batch", "teach", "report").</returns>
let panelToString = function
    | BoundaryPanel -> "boundary"
    | LayoutPanel -> "layout"
    | AnalyzePanel -> "analyze"
    | ViewPanel -> "3d"
    | BatchPanel -> "batch"
    | TeachPanel -> "teach"
    | ReportPanel -> "report"

/// <summary>
/// Converts a URL panel string representation into an <see cref="ActivePanel"/> option.
/// </summary>
/// <param name="s">The panel identifier string.</param>
/// <returns><c>Some ActivePanel</c> if matched; otherwise <c>None</c>.</returns>
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

/// <summary>
/// Synchronizes the current design state to both LocalStorage backup and the browser URL hash.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <param name="content">The encoded design state string.</param>
/// <param name="panel">The currently active panel view.</param>
let sync (js: IJSRuntime) (content: string) (panel: ActivePanel) =
    if String.IsNullOrWhiteSpace content then
        setUrlHash js ""
    else
        setBackup js content
        let p = panelToString panel
        let hash = sprintf "%s|P=%s" content p
        setUrlHash js hash

/// <summary>
/// Purely finds the delimiter index and length for panel state parsing within a URL hash fragment.
/// </summary>
let private findPanelDelimiterIndex (upperHash: string) =
    let idx1 = upperHash.LastIndexOf("|P=")
    if idx1 >= 0 then Some (idx1, 3)
    else
        let idx2 = upperHash.LastIndexOf("%7CP=")
        if idx2 >= 0 then Some (idx2, 5)
        else None

/// <summary>
/// Parses a raw, decoded URL hash fragment into its constituent state content, optional active panel, and URL origin flag.
/// </summary>
/// <param name="rawHash">The raw decoded URL hash string.</param>
/// <returns>Tuple of <c>(content, activePanelOption, isFromUrl)</c>.</returns>
let resolveHashChange (rawHash: string) : string * ActivePanel option * bool =
    if String.IsNullOrWhiteSpace rawHash then "", None, false
    else
        let upperHash = rawHash.ToUpperInvariant()
        match findPanelDelimiterIndex upperHash with
        | Some (pIdx, delimLength) when pIdx + delimLength <= rawHash.Length ->
            let content = rawHash.Substring(0, pIdx)
            let panelName = rawHash.Substring(pIdx + delimLength)
            content, stringToPanel panelName, true
        | _ ->
            rawHash, None, true

/// <summary>
/// Asynchronously resolves initial startup state by checking the browser URL hash first, falling back to LocalStorage backup.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
/// <returns>An async workflow returning <c>(content, activePanelOption, isFromUrl)</c>.</returns>
let resolveStartupState (js: IJSRuntime) =
    async {
        let! hashAttempt = Async.Catch ((getUrlHash js).AsTask() |> Async.AwaitTask)
        match hashAttempt with
        | Choice1Of2 urlHash when not (String.IsNullOrWhiteSpace urlHash) ->
            return resolveHashChange urlHash
        | _ ->
            let! backupAttempt = Async.Catch ((getBackup js).AsTask() |> Async.AwaitTask)
            match backupAttempt with
            | Choice1Of2 backup when not (isNull backup) && not (String.IsNullOrWhiteSpace backup) ->
                return backup, None, false // Source: Local
            | _ -> return "", None, false
    }

/// <summary>
/// Safely purges local backup state stored in browser LocalStorage.
/// </summary>
/// <param name="js">The JS interop runtime instance.</param>
let purgeLocalBackup (js: IJSRuntime) =
    clearBackup js
