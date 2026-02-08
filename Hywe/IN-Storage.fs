module Storage
    
open Microsoft.JSInterop
open System.Threading.Tasks

// Triggered on every change to prevent work loss
let autoSave (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("saveToBrowser", "last_session", content)

// Triggered by "Export" button
let exportFile (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("downloadHywFile", "design.hyw", content)

// Triggered by "Import" button
let importFile (js: IJSRuntime) (inputId: string) : Task<string> =
    js.InvokeAsync<string>("readHywFile", inputId).AsTask()