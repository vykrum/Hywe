module Hywe.Report

open Bolero
open Bolero.Html
open ModelTypes
open System

let viewReport (model: Model) dispatch =
    let opts = model.ReportOptions
    
    let updateOpts f = dispatch (UpdateReportOptions f)
    
    let renderToggleRow textLabel isChecked onChange =
        elt "label" {
            attr.``class`` "report-toggle-row"
            input {
                attr.``type`` "checkbox"
                attr.``checked`` isChecked
                on.change (fun _ -> onChange (not isChecked))
            }
            text textLabel
        }

    let renderLevelToggles (level: int) (sections: LevelReportSections) =
        div {
            attr.``class`` "report-toggle-tree"
            div {
                attr.``class`` "report-toggle-row level-header"
                text (sprintf "Level %d" level)
            }
            
            elt "label" {
                attr.``class`` "report-toggle-row sub-row"
                input {
                    attr.``type`` "checkbox"
                    attr.``checked`` sections.FlowChart
                    on.change (fun _ -> 
                        let s = { sections with FlowChart = not sections.FlowChart }
                        updateOpts (fun o -> { o with LevelSections = Map.add level s o.LevelSections })
                    )
                }
                text "Flow Chart"
            }
            
            elt "label" {
                attr.``class`` "report-toggle-row sub-row"
                input {
                    attr.``type`` "checkbox"
                    attr.``checked`` sections.BatchOverview
                    on.change (fun _ -> 
                        let s = { sections with BatchOverview = not sections.BatchOverview }
                        updateOpts (fun o -> { o with LevelSections = Map.add level s o.LevelSections })
                    )
                }
                text "Batch Overview (24 variations)"
            }
            
            elt "label" {
                attr.``class`` "report-toggle-row sub-row"
                input {
                    attr.``type`` "checkbox"
                    attr.``checked`` sections.Variations
                    on.change (fun _ -> 
                        let s = { sections with Variations = not sections.Variations }
                        updateOpts (fun o -> { o with LevelSections = Map.add level s o.LevelSections })
                    )
                }
                text "Variation Pages (24 per level)"
            }
            
            if sections.Variations then
                div {
                    attr.style "display: grid; grid-template-columns: repeat(4, 1fr); gap: 5px; padding-left: 40px; margin-bottom: 10px;"
                    forEach [0..23] <| fun i ->
                        let isSel = sections.SelectedVariations.Contains(i)
                        elt "label" {
                            attr.style "display: flex; align-items: center; font-size: 11px; color: #666; cursor: pointer;"
                            input {
                                attr.``type`` "checkbox"
                                attr.style "margin-right: 5px;"
                                attr.``checked`` isSel
                                on.change (fun _ ->
                                    let newSet = if isSel then Set.remove i sections.SelectedVariations else Set.add i sections.SelectedVariations
                                    let s = { sections with SelectedVariations = newSet }
                                    updateOpts (fun o -> { o with LevelSections = Map.add level s o.LevelSections })
                                )
                            }
                            text (Page.indexToSqn i)
                        }
                }
        }

    div {
        attr.``class`` "report-panel fade-in"
        
        div {
            attr.``class`` "report-section-title"
            text "1. Project Details"
        }
        
        div {
            attr.style "width: 100%; max-width: 800px; display: flex; flex-direction: column; gap: 15px;"
            
            div {
                attr.``class`` "report-field"
                elt "label" { text "Project Title" }
                input {
                    attr.``class`` "hywe-input"
                    attr.value opts.ProjectTitle
                    on.input (fun e -> updateOpts (fun o -> { o with ProjectTitle = e.Value :?> string }))
                }
            }
            
            div {
                attr.``class`` "report-field"
                elt "label" { text "Project Number" }
                input {
                    attr.``class`` "hywe-input"
                    attr.value opts.ProjectNumber
                    on.input (fun e -> updateOpts (fun o -> { o with ProjectNumber = e.Value :?> string }))
                }
            }
            
            div {
                attr.``class`` "report-field"
                elt "label" { text "Author" }
                input {
                    attr.``class`` "hywe-input"
                    attr.value opts.Author
                    on.input (fun e -> updateOpts (fun o -> { o with Author = e.Value :?> string }))
                }
            }
            
            div {
                attr.``class`` "report-field"
                elt "label" { text "Client Name" }
                input {
                    attr.``class`` "hywe-input"
                    attr.value opts.ClientName
                    on.input (fun e -> updateOpts (fun o -> { o with ClientName = e.Value :?> string }))
                }
            }
            
            div {
                attr.``class`` "report-field"
                elt "label" { text "Description" }
                textarea {
                    attr.``class`` "hywe-input"
                    attr.rows 3
                    attr.value opts.Description
                    on.input (fun e -> updateOpts (fun o -> { o with Description = e.Value :?> string }))
                }
            }
        }
        
        div {
            attr.``class`` "report-section-title"
            text "2. Project Content"
        }
        
        renderToggleRow "Cover Page" opts.IncludeCover (fun v -> updateOpts (fun o -> { o with IncludeCover = v }))
        
        if opts.IncludeCover then
            div {
                attr.style "font-size: 11px; color: #888; margin: -5px 0 10px 25px; font-style: italic;"
                if model.ViewLocked then 
                    text "(3D view locked: will be included in cover page)"
                else 
                    text "Please lock 3D view for inclusion in cover page"
            }

        
        forEach (Map.toList model.Tree.Levels |> List.sortBy fst) <| fun (level, _) ->
            let s = 
                match Map.tryFind level opts.LevelSections with
                | Some sections -> sections
                | None -> { FlowChart = true; BatchOverview = true; Variations = true; SelectedVariations = Set.ofList [0..23] }
            
            div {
                attr.``class`` "report-level-card"
                div {
                    attr.``class`` "report-level-header"
                    text (sprintf "Level %d" level)
                }
                
                renderToggleRow "Flow Chart" s.FlowChart (fun v -> 
                    updateOpts (fun o -> { o with LevelSections = Map.add level { s with FlowChart = v } o.LevelSections }))
                
                renderToggleRow "Batch Overview (Grid)" s.BatchOverview (fun v -> 
                    updateOpts (fun o -> { o with LevelSections = Map.add level { s with BatchOverview = v } o.LevelSections }))
                
                renderToggleRow "Individual Variations" s.Variations (fun v -> 
                    updateOpts (fun o -> { o with LevelSections = Map.add level { s with Variations = v } o.LevelSections }))
                
                if s.Variations then
                    div {
                        attr.``class`` "variation-grid-controls"
                        button {
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-light report-mini-btn"
                            on.click (fun _ -> updateOpts (fun o -> { o with LevelSections = Map.add level { s with SelectedVariations = Set.ofList [0..23] } o.LevelSections }))
                            text "All"
                        }
                        button {
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-light report-mini-btn"
                            on.click (fun _ -> updateOpts (fun o -> { o with LevelSections = Map.add level { s with SelectedVariations = Set.empty } o.LevelSections }))
                            text "None"
                        }
                    }
                    div {
                        attr.``class`` "variation-selection-grid"
                        forEach [0..23] <| fun i ->
                            let isSelected = s.SelectedVariations.Contains(i)
                            div {
                                attr.``class`` (if isSelected then "var-chip selected" else "var-chip")
                                on.click (fun _ ->
                                    let newSet = if isSelected then Set.remove i s.SelectedVariations else Set.add i s.SelectedVariations
                                    updateOpts (fun o -> { o with LevelSections = Map.add level { s with SelectedVariations = newSet } o.LevelSections }))
                                text (Page.indexToSqn i)
                            }
                    }
            }
            
        div {
            attr.``class`` "report-section-title"
            text "3. Generate"
        }
        
        let reportPages = Hywe.ReportGenerator.buildPageManifest opts (model.Tree.Levels.Keys |> Seq.toList |> List.sort)
        div {
            attr.``class`` "report-page-count"
            text (sprintf "Report ready — %d pages" reportPages.Length)
        }
        
        button {
            attr.``class`` "hywe-btn hywe-btn-dark report-generate-btn"
            attr.disabled model.IsGeneratingReport
            on.click (fun _ -> dispatch GenerateReport)
            text (if model.IsGeneratingReport then "Generating..." else "Generate Report (PDF)")
        }
        
        if model.IsGeneratingReport then
            div {
                attr.``class`` "report-status"
                span { attr.``class`` "report-spinner" }
                text "Processing layouts and compiling report..."
            }
    }
