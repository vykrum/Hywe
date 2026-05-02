module Storage

open System
open Microsoft.JSInterop

// Shadow save for browser refresh persistence
let autoSave (js: IJSRuntime) (content: string) =
    js.InvokeVoidAsync("localStorage.setItem", "hywe_backup", content) |> ignore
    js.InvokeVoidAsync("hyweSaveToUrl", content) |> ignore

let clearBackup (js: IJSRuntime) =
    js.InvokeVoidAsync("localStorage.removeItem", "hywe_backup") |> ignore
    js.InvokeVoidAsync("eval", "window.history.replaceState(null, '', ' ');") |> ignore

// Retrieve shadow save (prioritizes URL Hash over LocalStorage)
let getBackup (js: IJSRuntime) =
    async {
        let! urlHash = js.InvokeAsync<string>("hyweLoadFromUrl").AsTask() |> Async.AwaitTask
        if not (String.IsNullOrEmpty urlHash) then
            return urlHash
        else
            let! local = js.InvokeAsync<string>("localStorage.getItem", "hywe_backup").AsTask() |> Async.AwaitTask
            return local
    }

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