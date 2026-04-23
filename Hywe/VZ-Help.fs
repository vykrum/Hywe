module Help

open Bolero.Html
open ModelTypes

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
          Content = "Activate Boundary to activate the interactive polygon editor. Activate Relative to reproportion sizes to fit inside."
          TargetId = "hywe-polygon-editor"
          Position = "lower-left-slider" }
    | NodeGuide ->
        { Title = "The Main Blueprint"
          Content = "Change size values inline, click + to add a child node and x to delete a node and all of its descendents"
          TargetId = "hywe-input-interactive"
          Position = "top-left-header" }
    | LayoutGuide ->
        { Title = "Procedural Options"
          Content = "Slide through the 24 alternate configurations generated for every tree defined above."
          TargetId = "hywe-sequence-selector"
          Position = "lower-left-slider" }
    | Finish ->
        { Title = "End of the Beginning"
          Content = "Now add a node or alter a size. Remember to click 'hyWEAVE' to update the layout generated below."
          TargetId = "hywe-hyweave"
          Position = "top-left-header" }

let renderHelpButton (dispatch: Message -> unit) =
    span {
        attr.id "hywe-help-trigger"
        attr.style "cursor: pointer; color: #888; font-size: 14px; margin-left: 10px;"
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
                    | 'x' | '×' when cleanWord.Length = 1 -> span { attr.``class`` "onb-orange"; text (string c) }
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
                

                if not (System.String.IsNullOrWhiteSpace data.Title) then
                    h4 {
                        attr.style "margin: 0 0 4px 0; font-size: 0.75rem; font-weight: 700; color: #2E86C1; letter-spacing: -0.2px; text-align: center;"
                        text data.Title
                    }

                p { 
                    attr.style "margin: 0 0 8px 0; font-size: 0.7rem; line-height: 1.3; color: #555; text-align: center;"
                    renderFormattedContent data.Content
                }

                div {
                    attr.style "display: flex; justify-content: space-between; align-items: center; padding: 0 4px;"
                    
                    button {
                        attr.``class`` "onboarding-link"
                        attr.style (if state.CurrentStep = Welcome then "opacity: 0; pointer-events: none;" else "font-weight: bold;")
                        on.click (fun _ -> dispatch PreviousOnboardingStep)
                        text "<"
                    }

                    button {
                        attr.``class`` "onboarding-skip-link"
                        attr.style "font-weight: bold; font-size: 0.8rem; color: #e67e22;"
                        on.click (fun _ -> dispatch SkipOnboarding)
                        text "x"
                    }

                    button {
                        attr.``class`` "onboarding-link"
                        attr.style "font-weight: bold;"
                        on.click (fun _ -> dispatch NextOnboardingStep)
                        text ">"
                    }
                }
            }
        }
    }
