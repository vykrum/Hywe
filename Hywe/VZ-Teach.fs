module Hywe.Teach

open System
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html
open ModelTypes

let startTranscription (js: IJSRuntime) (textAreaId: string) =
    async {
        do! js.InvokeVoidAsync("eval", sprintf """
            (function() {
                return new Promise((resolve) => {
                    const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
                    if (!SpeechRecognition) { 
                        alert("Speech recognition is not supported."); 
                        resolve(); 
                        return; 
                    }
                    const recognition = new SpeechRecognition();
                    const textArea = document.getElementById('%s');
                    recognition.onresult = (event) => {
                        let finalTranscript = '';
                        for (let i = event.resultIndex; i < event.results.length; ++i) {
                            if (event.results[i].isFinal) {
                                finalTranscript += event.results[i][0].transcript;
                            }
                        }
                        if (finalTranscript) {
                            const currentVal = textArea.value.trim();
                            textArea.value = currentVal ? `${currentVal} ${finalTranscript}` : finalTranscript;
                            textArea.dispatchEvent(new Event('input', { bubbles: true }));
                        }
                    };
                    recognition.onend = () => resolve();
                    recognition.onerror = () => resolve();
                    recognition.start();
                });
            })()
        """ textAreaId).AsTask() |> Async.AwaitTask
    }

let private selectField (model: Model) dispatch (label: string) (current: string) (options: string list) (descriptions: Map<string, string>) updater =
    let isPredefined = options |> List.contains current
    let rowTips = descriptions |> Map.toSeq |> Seq.map snd |> Set.ofSeq
    
    let currentTip = 
        match model.HoveredInfo with
        | Some (tip: string) when rowTips.Contains tip -> Some tip
        | Some (tip: string) when tip.Contains(label.ToLower()) -> Some tip
        | _ -> 
            if isPredefined then descriptions |> Map.tryFind current
            else Some $"Custom {label.ToLower()} tag applied."

    div {
        attr.``class`` "teach-select-row"
        span { attr.``class`` "teach-field-label"; text label }
        div {
            attr.``class`` "teach-option-group"
            for opt in options do
                button {
                    attr.``class`` (if current = opt then "teach-option active" else "teach-option")
                    on.mouseover (fun _ -> dispatch (SetHoveredInfo (descriptions |> Map.tryFind opt)))
                    on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                    on.click (fun _ -> dispatch (UpdateMetadata (fun m -> updater m opt)))
                    text opt
                }
            button {
                attr.``class`` (if not isPredefined then "teach-option active" else "teach-option")
                on.mouseover (fun _ -> dispatch (SetHoveredInfo (Some $"Enter a custom {label.ToLower()} tag.")))
                on.mouseout (fun _ -> dispatch (SetHoveredInfo None))
                on.click (fun _ -> dispatch (UpdateMetadata (fun m -> updater m "")))
                text "Other..."
            }
        }
        
        match currentTip with
        | Some tip -> div { attr.``class`` "teach-row-tip"; text tip }
        | None -> ()

        if not isPredefined then
            input {
                attr.``class`` "teach-custom-input"
                attr.placeholder (sprintf "Enter custom %s..." (label.ToLower()))
                attr.value current
                on.input (fun e -> dispatch (UpdateMetadata (fun m -> updater m (unbox<string> e.Value))))
            }
    }

let view model dispatch =
    let flowDescs = Map [
        "Sequential", "A 'deep' flow where spaces lead into one another in a chain."
        "Radial", "A 'shallow' flow where most spaces branch directly from a single central hub."
        "Hierarchical", "A multi-level tree where primary spaces lead to secondary clusters."
    ]

    let ambiDescs = Map [
        "Open", "Minimizes walls to maximize visual and spatial continuity."
        "Modular", "Highly partitioned with distinct, repeatable, or enclosed spaces."
        "Minimal", "Stripped back to essential connections and clean flow."
    ]

    let stageDescs = Map [
        "Ideation", "Initial loose clustering and spatial relationship mapping."
        "Zoning", "Structured grouping of distinct functional areas."
        "Massing", "Defined volumetric proportions and 3D stacking logic."
    ]

    let scaleDescs = Map [
        "Layout", "Single-level spatial arrangement or individual unit logic."
        "Building", "Multi-level structure with vertical hierarchical dependencies."
        "Masterplan", "Large-scale arrangement or multi-building planning."
    ]

    let typoDescs = Map [
        "Residential", "Homes, apartments, or private living quarters."
        "Commercial", "Workspaces, retail, or corporate environments."
        "Institutional", "Healthcare, educational, or civic facilities."
    ]

    div {
        attr.``class`` "teach-panel-container"
        
        div {
            attr.``class`` "teach-intro-section"
            h2 { attr.``class`` "teach-intro-title"; text "Architectural Data Collection" }
            p { 
                attr.``class`` "teach-intro-text"
                text "Help generate a robust architectural dataset by tagging your design intent. Your input directly trains the underlying spatial logic to better understand complex hierarchical layouts."
            }
        }
        
        div {
            attr.``class`` "teach-objective-section"
            
            selectField model dispatch "Scale" model.TeachMetadata.Scale [ "Layout"; "Building"; "Masterplan" ] scaleDescs (fun m v -> { m with Scale = v })
            selectField model dispatch "Typology" model.TeachMetadata.Typology [ "Residential"; "Commercial"; "Institutional" ] typoDescs (fun m v -> { m with Typology = v })
            selectField model dispatch "Flow" model.TeachMetadata.Flow [ "Sequential"; "Radial"; "Hierarchical" ] flowDescs (fun m v -> { m with Flow = v })
            selectField model dispatch "Ambience" model.TeachMetadata.Ambience [ "Open"; "Modular"; "Minimal" ] ambiDescs (fun m v -> { m with Ambience = v })
            selectField model dispatch "Stage" model.TeachMetadata.Stage [ "Ideation"; "Zoning"; "Massing" ] stageDescs (fun m v -> { m with Stage = v })
        }

        div {
            attr.style "width: 100%; margin-top: 0.8rem; display: flex; flex-direction: column; gap: 0.3rem;"
            
            div {
                attr.style "display: grid; grid-template-columns: 40px 1fr 40px; align-items: center; gap: 0.5rem; width: 100%;"
                
                button {
                    attr.``class`` (match model.IsRecording with | true -> "mic-button recording" | false -> "mic-button")
                    attr.style "justify-self: start;"
                    attr.title "Start Voice Capture"
                    on.click (fun _ -> dispatch StartVoiceCapture)
                    rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z"></path><path d="M19 10v2a7 7 0 0 1-14 0v-2"></path><line x1="12" y1="19" x2="12" y2="23"></line><line x1="8" y1="23" x2="16" y2="23"></line></svg>"""
                }

                button {
                    attr.style "justify-self: center; background: transparent; border: 1.5px solid #7f8c8d; color: #7f8c8d; padding: 4px 18px; border-radius: 6px; font-size: 0.8rem; font-weight: 600; cursor: pointer; transition: all 0.2s ease; display: flex; align-items: center;"
                    attr.title "Generate Summary"
                    on.click (fun _ -> dispatch SuggestDescription)
                    text "Generate Summary"
                }

                button {
                    attr.``class`` "mic-button"
                    attr.style "justify-self: end; color: #e74c3c;"
                    attr.title "Clear Description"
                    on.click (fun _ -> dispatch (SetDescription ""))
                    rawHtml """<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"></polyline><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path><line x1="10" y1="11" x2="10" y2="17"></line><line x1="14" y1="11" x2="14" y2="17"></line></svg>"""
                }
            }

            textarea {
                attr.id "hynteract-desc-input"
                attr.``class`` "teach-textarea small"
                attr.placeholder "Enter spatial narrative or unique design nuances..."
                attr.value model.UserDescription
                on.input (fun e -> dispatch (SetDescription (unbox<string> e.Value)))
            }
        }

        div {
            attr.style "width: 100%; display: flex; flex-direction: column; align-items: center; gap: 0.5rem; margin-top: 0.8rem;"
            
            let hasSummary = not (String.IsNullOrWhiteSpace model.UserDescription)
            let isBusy = model.IsSavingToHynteract

            p { 
                attr.style "font-size: 0.85em; color: #7f8c8d; font-style: italic; text-align: center; margin: 0; max-width: 80%;"
                if hasSummary then 
                    text "Review summary and add any relevant input/details"
                else
                    text "A spatial summary is required to enable commitment"
            }

            button {
                attr.``class`` (
                    if isBusy || not hasSummary then "record-submit-btn disabled" 
                    else "record-submit-btn active"
                )
                attr.style (if not hasSummary then "opacity: 0.5; cursor: not-allowed;" else "")
                attr.title (if not hasSummary then "Please generate or enter a summary first" else "Commit this intent to the dataset")
                attr.disabled (isBusy || not hasSummary)
                on.click (fun _ -> dispatch RecordToHynteract)
                
                match isBusy with 
                | true -> text "Committing..." 
                | false -> text "Commit to Dataset"
            }
        }
    }
