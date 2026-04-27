module Storage

open System
open Microsoft.JSInterop

// Shadow save for browser refresh persistence
let autoSave (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("localStorage.setItem", "hywe_backup", content)

// Generates timestamped filename and triggers download
let saveFile (js: IJSRuntime) (content: string) =
    let timestamp = DateTime.Now.ToString("yyMMddHHmm")
    let fileName = sprintf "%s.hyw" timestamp
    js.InvokeVoidAsync("eval", sprintf """
        (function() {
            const blob = new Blob([`%s`], { type: 'text/plain' });
            const url = URL.createObjectURL(blob);
            const anchor = document.createElement('a');
            anchor.href = url;
            anchor.download = '%s';
            document.body.appendChild(anchor);
            anchor.click();
            document.body.removeChild(anchor);
            URL.revokeObjectURL(url);
        })()
    """ content fileName)

// Traditional import
let importFile (js: IJSRuntime) (inputId: string) =
    js.InvokeAsync<string>("readHywFile", inputId)