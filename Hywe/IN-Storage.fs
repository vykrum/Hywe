module Storage
open Microsoft.JSInterop

// Saves to browser internal memory (Shadow Save)
let shadowSave (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("saveToBrowser", "hywe_backup", content)

// Triggers the session-based "Save" (File System Access)
let saveToDisk (js: IJSRuntime) (content: string) =
    js.InvokeAsync<bool>("saveHywFile", content)

// Traditional file import (via hidden input)
let importFile (js: IJSRuntime) (id: string) =
    js.InvokeAsync<string>("readHywFile", id)