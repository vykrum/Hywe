module Help

open Bolero.Html
open ModelTypes
open Page

type TooltipDef = {
    Title: string
    Content: string
    TargetId: string
    Position: string
}

let getOnboardingStepData step =
    match step with
    | Welcome ->
        { Title = "Welcome to Hywe"
          Content = "Grow the interactive tree on the right to outline the flow of your spaces."
          TargetId = "hywe-save-btn"
          Position = "top-left-header" }
    | BoundaryGuide ->
        { Title = "Unbound by Default"
          Content = "Activate Boundary to confine the layout and Relative to reproportion sizes."
          TargetId = "hywe-polygon-editor"
          Position = "lower-left-slider" }
    | NodeGuide ->
        { Title = "The Main Blueprint"
          Content = "Click + to add a node and - to delete a node and its descendents"
          TargetId = "hywe-input-interactive"
          Position = "top-left-header" }
    | LayoutGuide ->
        { Title = "Procedural Options"
          Content = "Slide through the 24 configurations generated for every tree defined above."
          TargetId = "hywe-sequence-selector"
          Position = "lower-left-slider" }
    | Finish ->
        { Title = "End of the Beginning"
          Content = "Remember to click 'hyWEAVE' to regenerate abstract layouts for an updated tree."
          TargetId = "hywe-hyweave"
          Position = "top-left-header" }

let renderHelpButton (dispatch: Message -> unit) =
    button {
        attr.id "hywe-help-trigger"
        attr.``class`` "help-trigger-btn"
        on.click (fun _ -> dispatch RestartOnboarding)
        text "?"
    }

/// Helper to render text with colored + and -/- symbols
let renderFormattedContent (content: string) =
    let words = content.Split(' ')
    span {
        for i, word in words |> Seq.indexed do
            let cleanWord = word.TrimEnd([| '.'; ','; ';'; ':' |])
            if cleanWord = "Boundary" || cleanWord = "Relative" then
                strong { text word }
            else
                for c in word do
                    match c with
                    | '+' -> span { attr.``class`` "onb-green"; text "+" }
                    | '-' | '×' -> span { attr.``class`` "onb-orange"; text (string c) }
                    | _ -> text (string c)
            if i < words.Length - 1 then text " "
    }

let viewHelp (state: OnboardingState) (dispatch: Message -> unit) =
    let data = getOnboardingStepData state.CurrentStep
    
    div {
        attr.``class`` "onboarding-overlay"
        
        div {
            attr.id (sprintf "onboarding-step-%A" state.CurrentStep)
            attr.``class`` ("onboarding-tooltip " + data.Position)
            attr.style "position: absolute;"
            
            div {
                attr.``class`` "tooltip-glass"
                
                if state.IsAutoSimulating then
                    div {
                        attr.``class`` "auto-tour-indicator"
                        div { attr.``class`` "progress-bar-fill" }
                    }

                if not (System.String.IsNullOrWhiteSpace data.Title) then
                    h4 {
                        attr.style "margin: 0 0 4px 0; font-size: 0.75rem; font-weight: 700; color: #363636; letter-spacing: -0.2px;"
                        text data.Title
                    }

                p { 
                    attr.style "margin: 0 0 8px 0; font-size: 0.7rem; line-height: 1.3; color: #555;"
                    renderFormattedContent data.Content
                }

                div {
                    attr.style "display: flex; justify-content: space-between; align-items: center;"
                    
                    button {
                        attr.``class`` "onboarding-skip-link"
                        on.click (fun _ -> 
                            if state.IsAutoSimulating then dispatch StopAutoSimulation
                            dispatch SkipOnboarding)
                        text "dismiss"
                    }
                    
                    div {
                        attr.style "display: flex; gap: 6px;"
                        
                        button {
                            attr.``class`` "onboarding-link"
                            on.click (fun _ -> 
                                if state.IsAutoSimulating then dispatch StopAutoSimulation
                                dispatch NextOnboardingStep)
                            text (if state.CurrentStep = Finish then "start" else "next")
                        }
                    }
                }
            }
        }
    }
