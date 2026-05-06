module Storage

open System
open Microsoft.JSInterop
open Hywe.Core.Parse

// Shadow save for browser refresh persistence
let autoSave (js: IJSRuntime) (content: string) =
    if not (String.IsNullOrWhiteSpace content) then
        js.InvokeVoidAsync("localStorage.setItem", "hywe_backup", content) |> ignore

let clearBackup (js: IJSRuntime) =
    js.InvokeVoidAsync("localStorage.removeItem", "hywe_backup") |> ignore

// Retrieve shadow save from LocalStorage
let getBackup (js: IJSRuntime) =
    async {
        try
            let! content = js.InvokeAsync<string>("localStorage.getItem", "hywe_backup").AsTask() |> Async.AwaitTask
            return if isNull content then "" else content
        with _ -> return ""
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
    let sortedLevels = splitIntoLevels content |> Array.truncate 1
    
    let mutable finalState = current
    for lvl in sortedLevels do
        let attrs, _ = processLevel lvl
        finalState <- attrs |> Map.fold (fun (m: PolygonEditor.PolygonEditorModel) key v ->
            match key with
            | "W" -> match v with | Float num -> { m with LogicalWidth = (max 10.0 num) * 10.0 } | _ -> m
            | "H" -> match v with | Float num -> { m with LogicalHeight = (max 10.0 num) * 10.0 } | _ -> m
            | "L" -> match v with | Float num -> { m with Elevation = int num } | _ -> m
            | "S" -> { m with BaseStr = v }
            | "X" -> { m with UseAbsolute = (v = "1") }
            | "E" -> 
                match PolygonEditor.parsePoint v with 
                | Ok pt -> { m with EntryPoint = pt } 
                | _ -> m
            | "O" -> 
                match PolygonEditor.parsePoly v with 
                | Ok pts -> { m with Outer = pts } 
                | _ -> m
            | "I" -> 
                match PolygonEditor.parseIslands v with 
                | Ok pts -> { m with Islands = pts } 
                | _ -> m
            | _ -> m
        ) finalState
    
    let isZeroBoundary = finalState.LogicalWidth <= 0.0 || finalState.LogicalHeight <= 0.0
    
    let finalStateWithBoundary = 
        let isBoundary = not finalState.UseAbsolute && not isZeroBoundary
        { finalState with 
            Outer = if Array.isEmpty finalState.Outer then PolygonEditor.initOuter else finalState.Outer
            LogicalWidth = if finalState.LogicalWidth <= 0.0 then 300.0 else finalState.LogicalWidth
            LogicalHeight = if finalState.LogicalHeight <= 0.0 then 300.0 else finalState.LogicalHeight
            UseBoundary = isBoundary
            PolygonEnabled = isBoundary }
        |> PolygonEditor.refreshCachedStrings

    PolygonEditor.FreshlyImported finalStateWithBoundary
