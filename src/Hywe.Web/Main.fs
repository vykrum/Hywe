module Hywe.Main

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open ModelTypes
open ModelHelpers
open Hywe
open AppState
open FileManager

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    let mutable _dotnetRef: DotNetObjectReference<MyApp> option = None

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    [<JSInvokable>]
    member this.HandleUndo() = this.Dispatch Undo

    [<JSInvokable>]
    member this.HandleRedo() = this.Dispatch Redo

    [<JSInvokable>]
    member this.HandleHashChange(rawHash: string) =
        let (content, panel, isFromUrl) = Protocol.resolveHashChange rawHash
        this.Dispatch (LoadState (content, panel, isFromUrl))

    [<JSInvokable>]
    member this.SetInstallPromptAvailable(available: bool) = this.Dispatch (SetInstallPromptAvailable available)

    [<JSInvokable>]
    member this.SetPrivacyAlert(show: bool) = this.Dispatch (SetPrivacyAlert show)

    [<JSInvokable>]
    member this.SetIsStandalone(isS: bool) = this.Dispatch (SetIsStandalone isS)

    override this.OnAfterRenderAsync(firstRender) =
        let t = base.OnAfterRenderAsync(firstRender)
        match firstRender with
        | true ->
            let ref = DotNetObjectReference.Create(this)
            _dotnetRef <- Some ref
            this.JSRuntime.InvokeVoidAsync("registerUndoRedo", ref).AsTask() |> ignore
            this.JSRuntime.InvokeVoidAsync("registerPwaInstall", ref).AsTask() |> ignore
            this.JSRuntime.InvokeVoidAsync("registerHashChange", ref).AsTask() |> ignore
        | false -> ()
        t

    interface System.IDisposable with
        member _.Dispose() =
            _dotnetRef |> Option.iter (fun r -> r.Dispose())

    override this.OnInitialized() =
        base.OnInitialized()

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.batch [
                Cmd.OfAsync.perform (fun () -> Protocol.resolveStartupState this.JSRuntime) () (fun (res, panel, isFromUrl) -> 
                        LoadState (res, panel, isFromUrl))
                Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 1000 }) () (fun _ -> TransitionToIntro)
                Cmd.OfAsync.perform (fun () -> async { do! Async.Sleep 3000 }) () (fun _ -> TransitionToMain)
                Cmd.OfAsync.perform (fun () -> async { updateMetadata this.JSRuntime; return () }) () (fun _ -> NoOp)
            ])
            (fun msg model -> update this.JSRuntime msg model)
            (fun model dispatch -> 
                concat {
                    match model.Onboarding.IsActive && model.CurrentScreen = MainScreen with
                    | true -> Page.Help.viewHelp model.Onboarding dispatch
                    | false -> empty()

                    Index.coreScript
                    Index.siteHeader
                    div {
                        attr.id "page-content"
                        match model.CurrentScreen <> LoadingScreen with
                        | true -> attr.``class`` "fade-container fade-in"
                        | false -> attr.``class`` "fade-container"
                        
                        Index.introSplash model.CurrentScreen dispatch

                        div {
                            attr.id "main"
                            match model.CurrentScreen = MainScreen with
                            | true ->
                                attr.``class`` "fade-in"
                                attr.style "display: block; opacity: 1;"
                            | false ->
                                attr.style "display: none; opacity: 0;"

                            view model dispatch this.JSRuntime
                        }

                        match model.ShowPrivacyAlert && model.CurrentScreen = MainScreen with
                        | true ->
                            div {
                                attr.style "position: fixed; top: 80px; left: 50%; transform: translateX(-50%); z-index: 5000; background: #363636; color: white; padding: 15px 18px; border-radius: 6px; box-shadow: 0 8px 24px rgba(0,0,0,0.3); font-size: 13px; max-width: 260px; display: flex; flex-direction: column; gap: 10px; border: 1px solid rgba(255,255,255,0.1); animation: fadeIn 0.3s ease-out;"
                                div {
                                    attr.style "font-weight: 500; display: flex; align-items: center; gap: 8px;"
                                    rawHtml """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="#f0ad4e" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"></path><line x1="12" y1="9" x2="12" y2="13"></line><line x1="12" y1="17" x2="12" y2="17.01"></line></svg>"""
                                    span { attr.style "font-size: 11px; letter-spacing: 0.5px; text-transform: uppercase;"; text "Persistence" }
                                }
                                div {
                                    attr.style "opacity: 0.7; line-height: 1.4; font-size: 11px;"
                                    text "Privacy browsers may clear local work. Install as an app to ensure data is saved."
                                }
                                div {
                                    attr.style "display: flex; flex-direction: column; gap: 6px; margin-top: 4px;"
                                    match model.InstallPromptAvailable with
                                    | true ->
                                        button {
                                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                                            attr.style "width: 100%; font-size: 10px; font-weight: 600;"
                                            on.click (fun _ -> dispatch InstallRequested)
                                            text "INSTALL"
                                        }
                                    | false -> empty()
                                    button {
                                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-ghost"
                                        attr.style "width: 100%; color: white; opacity: 0.5; font-size: 10px;"
                                        on.click (fun _ -> dispatch (SetPrivacyAlert false))
                                        text "DISMISS"
                                    }
                                }
                            }
                        | false -> empty()

                        Index.siteFooter model.CurrentScreen
                    }
                    Index.loadingScreen model.CurrentScreen
                }
            )
