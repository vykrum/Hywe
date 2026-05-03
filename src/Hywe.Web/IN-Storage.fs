module Storage

open System
open Microsoft.JSInterop
open Hywe.Core

// Shadow save for browser refresh persistence
let autoSave (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("localStorage.setItem", "hywe_backup", content) |> ignore

let clearBackup (js: IJSRuntime) =
    js.InvokeVoidAsync("localStorage.removeItem", "hywe_backup") |> ignore

// Retrieve shadow save from LocalStorage
let getBackup (js: IJSRuntime) =
    async {
        let! local = js.InvokeAsync<string>("localStorage.getItem", "hywe_backup").AsTask() |> Async.AwaitTask
        return local
    }

// Generates timestamped filename and triggers download
let saveFile (js: IJSRuntime) (content: string) =
    let timestamp = DateTime.Now.ToString("yyMMddHHmm")
    let fileName = sprintf "%s.hyw" timestamp
    js.InvokeVoidAsync("downloadFile", fileName, content, "application/octet-stream") |> ignore

// Traditional import
let importFile (js: IJSRuntime) (inputId: string) =
    js.InvokeAsync<string>("readHywFile", inputId)

/// <summary> Parses .hyw content and updates a PolygonEditorModel. </summary>
let importFromHyw (content: string) (current: PolygonEditor.PolygonEditorModel) : PolygonEditor.EditorState =
    let cleanStr = content.Replace("\n","").Replace("\t","")
    let sortedLevels = 
        cleanStr.Split(';', StringSplitOptions.RemoveEmptyEntries) |> Array.truncate 1
    
    let mutable finalState = current
    for lvl in sortedLevels do
        let attrs = Parsing.extractAttrsFromHyw lvl
        finalState <- attrs |> Map.fold (fun (m: PolygonEditor.PolygonEditorModel) key v ->
            match key with
            | "W" -> match Parsing.tryParseFloat v with | Some num -> { m with LogicalWidth = num * 10.0 } | None -> m
            | "H" -> match Parsing.tryParseFloat v with | Some num -> { m with LogicalHeight = num * 10.0 } | None -> m
            | "L" -> match Parsing.tryParseFloat v with | Some num -> { m with Elevation = int num } | None -> m
            | "S" -> { m with BaseStr = v }
            | "X" -> { m with UseAbsolute = (v = "1") }
            | "E" -> match Parsing.parseCoords v with | pts when pts.Length = 1 -> { m with EntryPoint = pts.[0] } | _ -> m
            | "O" -> match Parsing.parseCoords v with | pts when pts.Length > 0 -> { m with Outer = pts } | _ -> m
            | "I" -> { m with Islands = Parsing.parseIslands v }
            | _ -> m
        ) finalState
    
    let isZeroBoundary = finalState.LogicalWidth <= 0.0 || finalState.LogicalHeight <= 0.0
    
    let finalStateWithBoundary = 
        { finalState with 
            LogicalWidth = if finalState.LogicalWidth <= 0.0 then 300.0 else finalState.LogicalWidth
            LogicalHeight = if finalState.LogicalHeight <= 0.0 then 300.0 else finalState.LogicalHeight
            UseBoundary = not finalState.UseAbsolute && not isZeroBoundary
            PolygonEnabled = not finalState.UseAbsolute && not isZeroBoundary }
    PolygonEditor.FreshlyImported finalStateWithBoundary
