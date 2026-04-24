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
        { Title = "Welcome to HYWE"
          Content = "The interactive tree on the right is the primary interface. This is where you will design space hierarchies."
          TargetId = "hywe-save-btn"
          Position = "top-left-header" }
    | BoundaryGuide ->
        { Title = "Unbound by Default"
          Content = "Activate Boundary to manipulate the interactive editor and specify the Entry zone. Activate Relative to reproportion sizes to fit inside."
          TargetId = "hywe-polygon-editor"
          Position = "lower-left-slider" }
    | NodeGuide ->
        { Title = "Chart your Design Intent"
          Content = "Change labels and sizes inline, click + to Add a Child Node and x to Delete a Node and all of its descendents"
          TargetId = "hywe-input-interactive"
          Position = "top-left-header" }
    | LayoutGuide ->
        { Title = "The Procedural Variations"
          Content = "Slide to explore the 24 alternate configurations generated for the tree structure defined above."
          TargetId = "hywe-sequence-selector"
          Position = "lower-left-slider" }
    | Finish ->
        { Title = "Now let's give HYWE a try!"
          Content = "Add Nodes, Alter Sizes or Change Boundaries. Click 'hyWEAVE' to generate the updated configuration."
          TargetId = "hywe-hyweave"
          Position = "top-left-header" }

/// Helper to render text with colored + and -/- symbols
let renderFormattedContent (content: string) =
    let words = content.Split([| ' ' |], System.StringSplitOptions.None)
    span {
        for i = 0 to words.Length - 1 do
            let word = words.[i]
            let cleanWord = word.TrimEnd([| '.'; ','; ';'; ':' |])
            if cleanWord = "Boundary" || cleanWord = "Relative" then
                strong { text word }
            else
                for j = 0 to word.Length - 1 do
                    let c = word.[j]
                    match c with
                    | '+' -> span { attr.``class`` "onb-green"; text "+" }
                    | 'x' | '×' when cleanWord.Length = 1 -> span { attr.``class`` "onb-orange"; text (string c) }
                    | 'E' when cleanWord = "Entry" -> strong { text "E" }
                    | _ -> text (string c)
            if i < words.Length - 1 then text " "
    }

let viewHelp (state: OnboardingState) (dispatch: Message -> unit) =
    let data = getOnboardingStepData state.CurrentStep
    let stepId = 
        match state.CurrentStep with
        | Welcome -> "onboarding-step-Welcome"
        | BoundaryGuide -> "onboarding-step-BoundaryGuide"
        | NodeGuide -> "onboarding-step-NodeGuide"
        | LayoutGuide -> "onboarding-step-LayoutGuide"
        | Finish -> "onboarding-step-Finish"

    div {
        attr.``class`` "onboarding-overlay"
        
        div {
            attr.id stepId
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
