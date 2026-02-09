module Storage

open System
open Microsoft.JSInterop

// Shadow save for browser refresh persistence
let autoSave (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("saveToBrowser", "hywe_backup", content)

// Generates timestamped filename and triggers download
let saveFile (js: IJSRuntime) (content: string) =
    let timestamp = DateTime.Now.ToString("yyMMddHHmm")
    let fileName = sprintf "%s.hyw" timestamp
    js.InvokeVoidAsync("downloadHywFile", fileName, content)

// Traditional import
let importFile (js: IJSRuntime) (inputId: string) =
    js.InvokeAsync<string>("readHywFile", inputId)