module Overlays

open System
open Bolero.Html
open Hywe.Core
open Types
open ModelTypes

let viewConfirmOverlay (model: Model) (dispatch: Message -> unit) =
    match model.PendingConfirm with
    | None -> empty()
    | Some action ->
        let title, msg, confirmMsg, onConfirm =
            match action with
            | ConfirmAction.ResetWorkspace ->
                "Reset Layout?", ["Current layout will be replaced."], "Reset", HardReset
            | ConfirmAction.LoadPreset (name, label) ->
                (sprintf "Load %s preset?" label), ["Current layout will be replaced."], "Load", SelectPreset name
            | ConfirmAction.LoadGallery (name, rowId, author) ->
                (sprintf "Load %s?" name), ["Current layout will be replaced."], "Load", LoadGalleryDefinition (name, rowId, author)
            | ConfirmAction.SwitchTo tab ->
                "Switch View", ["Switch to this view?"], "Switch", SetActivePanel (match tab with Boundary -> BoundaryPanel | _ -> LayoutPanel)
            | ConfirmAction.ResetBoundaryAction ->
                "Reset Boundary?", [ "Boundary will be reset to default rectangle."; "All cutouts will be cleared." ], "Reset", PolygonEditorMsg ResetBoundary

        div {
            attr.style "position: fixed; inset: 0; background: rgba(255,255,255,0.7); backdrop-filter: blur(4px); z-index: 10000; display: flex; align-items: center; justify-content: center; animation: fadeIn 0.3s ease;"
            on.pointerdown (fun _ -> dispatch (ToggleConfirm None))
            
            div {
                attr.style "background: #fff; border: 1px solid #eee; padding: 20px 24px; border-radius: 8px; width: 220px; box-shadow: 0 10px 30px rgba(0,0,0,0.05); display: flex; flex-direction: column; gap: 12px; text-align: center; pointer-events: auto; justify-content: center;"
                "onclick:stopPropagation" => true
                
                let baseTitle, suffix =
                    match title.EndsWith("?") with
                    | true -> title.Substring(0, title.Length - 1), "?"
                    | false -> title, ""
                
                div {
                    attr.style "display: flex; justify-content: center; width: 100%; font-weight: 600; font-size: 1.1rem; color: #333;"
                    div {
                        attr.style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0;"
                        text baseTitle
                    }
                    match suffix with
                    | "" -> empty()
                    | s ->
                        div {
                            attr.style "flex-shrink: 0;"
                            text s
                        }
                }
                div {
                    attr.style "font-size: 0.9rem; color: #666; line-height: 1.3; display: flex; flex-direction: column; gap: 4px;"
                    for line in msg do
                        div { text line }
                }
                div {
                    attr.style "display: flex; flex-direction: column; gap: 8px; margin-top: 8px;"
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-light"
                        on.pointerdown (fun _ -> dispatch (ToggleConfirm None))
                        text "Cancel"
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-fillet hywe-btn-dark"
                        on.pointerdown (fun _ -> dispatch onConfirm)
                        text confirmMsg
                    }
                }
            }
        }

let viewGalleryModal (model: Model) (dispatch: Message -> unit) =
    match model.ShowGallery with
    | false -> empty()
    | true ->
        div {
            attr.style "position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 10000; display: flex; align-items: center; justify-content: center;"
            
            // Sibling 1: Backdrop
            div {
                attr.style "position: absolute; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.8);"
                on.click (fun _ -> dispatch ToggleGallery)
            }
            
            // Sibling 2: Modal Content
            div {
                attr.style "position: relative; background: #fff; width: 90%; max-width: 1100px; max-height: 80vh; border-radius: 8px; display: flex; flex-direction: column; overflow: hidden; box-shadow: 0 10px 30px rgba(0,0,0,0.5);"
                
                div {
                    attr.style "padding: 15px 20px; border-bottom: 1px solid #eee; display: flex; justify-content: space-between; align-items: center; background: #fafafa;"
                    h2 { 
                        attr.style "margin: 0; font-size: 1.2rem; color: #333; font-weight: 600;"
                        text "Community Gallery" 
                    }
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-ghost"
                        attr.style "font-size: 1.2rem; padding: 0 5px; line-height: 1;"
                        on.click (fun _ -> dispatch ToggleGallery)
                        text "×"
                    }
                }
                
                div {
                    attr.style "flex: 1; overflow-y: auto; padding: 15px 20px; display: flex; flex-direction: column; gap: 10px; background: #fdfdfd;"
                    
                    match model.IsLoadingGallery with
                    | true ->
                        div {
                            attr.style "text-align: center; padding: 30px; color: #777; font-style: italic;"
                            text "Loading latest HYWE configurations..."
                        }
                    | false ->
                        match model.GalleryEntries with
                        | None | Some [] -> 
                            div {
                                attr.style "text-align: center; padding: 30px; color: #777;"
                                text "Gallery is syncing... Please check back in a few minutes."
                            }
                        | Some entries ->
                            let filterText = 
                                match System.String.IsNullOrWhiteSpace(model.GalleryFilter) with
                                | true -> ""
                                | false -> model.GalleryFilter.ToLower()
                            let filteredEntries = 
                                match filterText with
                                | "" -> entries
                                | _ -> entries |> List.filter (fun e -> 
                                    (e.IsFeatured && ("featured".Contains(filterText) || filterText.Contains("featured") || filterText = "is:featured")) ||
                                    (not (System.String.IsNullOrWhiteSpace e.ExplorationDescription) && e.ExplorationDescription.ToLower().Contains(filterText)) || 
                                    (not (System.String.IsNullOrWhiteSpace e.Author) && e.Author.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Description) && e.Description.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Typology) && e.Typology.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Flow) && e.Flow.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Stage) && e.Stage.ToLower().Contains(filterText)) ||
                                    (not (System.String.IsNullOrWhiteSpace e.Scale) && e.Scale.ToLower().Contains(filterText)))
                                    
                            let pagedEntries = 
                                filteredEntries 
                                |> List.skip (min model.GalleryOffset (max 0 (filteredEntries.Length - 1))) 
                                |> List.truncate GALLERY_PAGE_SIZE

                            div {
                                attr.style "width: 100%; display: flex; flex-direction: column;"
                                
                                input {
                                    attr.``class`` "hywe-input"
                                    attr.style "margin-bottom: 12px; width: 100%; padding: 8px 12px; border-radius: 4px; border: 1px solid #ddd;"
                                    "onclick:stopPropagation" => true
                                    "onpointerdown:stopPropagation" => true
                                    attr.placeholder "Search by exploration, author, typology, flow, featured..."
                                    attr.value model.GalleryFilter
                                    on.input (fun e -> dispatch (UpdateGalleryFilter (unbox<string> e.Value)))
                                }
                                
                                match pagedEntries.IsEmpty with
                                | true ->
                                    div {
                                        attr.style "text-align: center; padding: 30px; color: #777;"
                                        text "No configurations match your search."
                                    }
                                | false ->
                                    div {
                                        attr.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(min(100%, 340px), 1fr)); gap: 10px; margin-bottom: 8px;"
                                        for entry in pagedEntries do
                                            div {
                                                attr.style "display: flex; align-items: stretch; border-radius: 8px; border: 1px solid #e9ecef; background: #ffffff; box-shadow: 0 1px 3px rgba(0,0,0,0.03); overflow: hidden; transition: border-color 0.15s ease;"
                                                
                                                // Content Container (Thumbnail + Information)
                                                div {
                                                    attr.style "flex: 1; display: flex; gap: 10px; align-items: center; padding: 8px 10px; min-width: 0;"

                                                    // Left: 60x60 SVG Thumbnail
                                                    div {
                                                        attr.style "width: 60px; height: 60px; min-width: 60px; border-radius: 6px; overflow: hidden; background: #f8f9fa; border: 1px solid #dee2e6; display: flex; align-items: center; justify-content: center; padding: 2px;"
                                                        match String.IsNullOrWhiteSpace entry.SvgThumbnail with
                                                        | false -> rawHtml entry.SvgThumbnail
                                                        | true -> rawHtml """<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#adb5bd" stroke-width="1.5"><rect x="3" y="3" width="18" height="18" rx="2"/><path d="M3 9h18M9 21V9"/></svg>"""
                                                    }

                                                    // Middle: Content Column
                                                    div {
                                                        attr.style "flex: 1; display: flex; flex-direction: column; gap: 4px; overflow: hidden; min-width: 0;"
                                                        
                                                        // Exploration Description (Title)
                                                        div {
                                                            attr.style "font-weight: 600; color: #1a1a1a; font-size: 0.92rem; line-height: 1.25; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
                                                            attr.title entry.ExplorationDescription
                                                            let desc = match String.IsNullOrWhiteSpace entry.ExplorationDescription with true -> "Untitled Exploration" | false -> entry.ExplorationDescription
                                                            text desc
                                                        }

                                                        // Author and Badges row
                                                        div {
                                                            attr.style "display: flex; align-items: center; flex-wrap: wrap; gap: 4px; font-size: 0.75rem;"
                                                            let dateSuffix =
                                                                match String.IsNullOrWhiteSpace entry.CreatedAt with
                                                                | true -> ""
                                                                | false ->
                                                                    match DateTime.TryParse entry.CreatedAt with
                                                                    | true, dt ->
                                                                        let span = DateTime.UtcNow - dt.ToUniversalTime()
                                                                        match span.TotalMinutes < 1.0 with
                                                                        | true -> " • just now"
                                                                        | false ->
                                                                            match span.TotalHours < 1.0 with
                                                                            | true -> sprintf " • %dm ago" (int span.TotalMinutes)
                                                                            | false ->
                                                                                match span.TotalDays < 1.0 with
                                                                                | true -> sprintf " • %dh ago" (int span.TotalHours)
                                                                                | false ->
                                                                                    match span.TotalDays < 30.0 with
                                                                                    | true -> sprintf " • %dd ago" (int span.TotalDays)
                                                                                    | false -> sprintf " • %s" (dt.ToString("MMM d"))
                                                                    | false, _ -> ""
                                                            span {
                                                                attr.style "color: #6c757d; white-space: nowrap; margin-right: 2px;"
                                                                let authorName = match String.IsNullOrWhiteSpace entry.Author with true -> "Anonymous" | false -> entry.Author
                                                                text (sprintf "by %s%s" authorName dateSuffix)
                                                            }
                                                            match entry.IsFeatured with
                                                            | true ->
                                                                span { 
                                                                    attr.style "background: #fef3c7; color: #92400e; border: 1px solid #fde68a; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 600;"
                                                                    text "Featured" 
                                                                }
                                                            | false -> empty()
                                                            
                                                            match entry.LevelsCount > 0 with
                                                            | true ->
                                                                let lvlLabel = match entry.LevelsCount with 1 -> "Level" | _ -> "Levels"
                                                                span { attr.style "background: #f1f3f5; color: #495057; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text (sprintf "%d %s" entry.LevelsCount lvlLabel) }
                                                            | false -> empty()
                                                            
                                                            match entry.SpacesCount > 0 with
                                                            | true ->
                                                                let spcLabel = match entry.SpacesCount with 1 -> "Node" | _ -> "Nodes"
                                                                span { attr.style "background: #f1f3f5; color: #495057; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text (sprintf "%d %s" entry.SpacesCount spcLabel) }
                                                            | false -> empty()

                                                            match not (String.IsNullOrWhiteSpace entry.Typology) && entry.Typology <> "N/A" with
                                                            | true -> span { attr.style "background: #e7f1ff; color: #0d6efd; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text entry.Typology }
                                                            | false -> empty()

                                                            match not (String.IsNullOrWhiteSpace entry.Flow) && entry.Flow <> "N/A" with
                                                            | true -> span { attr.style "background: #f1f3f5; color: #495057; padding: 1px 5px; border-radius: 3px; font-size: 0.7rem; font-weight: 500;"; text entry.Flow }
                                                            | false -> empty()
                                                        }
                                                    }
                                                }

                                                // Right: Vertical Full-Height Load Button
                                                button {
                                                    attr.``class`` "hywe-btn hywe-btn-dark"
                                                    attr.style "align-self: stretch; width: 18px; min-width: 18px; border: none; border-left: 1px solid #dee2e6; border-radius: 0; display: flex; align-items: center; justify-content: center; padding: 0; cursor: pointer; transition: background 0.15s ease; box-sizing: border-box;"
                                                    attr.title "Load this configuration into workspace"
                                                    let loadLabel = match String.IsNullOrWhiteSpace entry.ExplorationDescription with true -> "configuration" | false -> entry.ExplorationDescription
                                                    "aria-label" => sprintf "Load %s into workspace" (match String.IsNullOrWhiteSpace entry.ExplorationDescription with true -> "configuration" | false -> entry.ExplorationDescription)
                                                    on.click (fun _ -> dispatch (ToggleConfirm (Some (ConfirmAction.LoadGallery (entry.ExplorationDescription, entry.Id, entry.Author)))))
                                                    span {
                                                        attr.style "writing-mode: vertical-rl; transform: rotate(180deg); font-size: 8px; font-weight: 600; letter-spacing: 1.2px; text-transform: uppercase;"
                                                        text "LOAD ↵"
                                                    }
                                                }
                                            }
                                    }
                            }
                            
                            // Pagination Footer
                            div {
                                attr.style "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; padding-top: 14px; margin-top: auto; border-top: 1px solid #f1f3f5;"
                                
                                let totalItems = filteredEntries.Length
                                let totalPages = max 1 ((totalItems + GALLERY_PAGE_SIZE - 1) / GALLERY_PAGE_SIZE)
                                let currentPage = min totalPages ((model.GalleryOffset / GALLERY_PAGE_SIZE) + 1)
                                
                                // Left: Item & Page Summary
                                div {
                                    attr.style "font-size: 0.82rem; color: #6c757d; display: flex; align-items: center; gap: 6px;"
                                    let currentStart = match filteredEntries.IsEmpty with true -> 0 | false -> model.GalleryOffset + 1
                                    let currentEnd = min totalItems (model.GalleryOffset + pagedEntries.Length)
                                    text (sprintf "Showing %d - %d of %d" currentStart currentEnd totalItems)
                                    match totalPages > 1 with
                                    | true ->
                                        span {
                                            attr.style "color: #adb5bd;"
                                            text "•"
                                        }
                                        span {
                                            text (sprintf "Page %d of %d" currentPage totalPages)
                                        }
                                    | false -> empty()
                                }

                                // Right: Numbered Navigation Controls
                                div {
                                    attr.style "display: flex; align-items: center; gap: 4px;"

                                    // First Page Button
                                    button {
                                        attr.``class`` "hywe-btn hywe-btn-sm"
                                        attr.title "First page"
                                        match currentPage <= 1 with
                                        | true ->
                                            attr.disabled true
                                            attr.style "opacity: 0.35; cursor: not-allowed; background: #f8f9fa; border: 1px solid #dee2e6; color: #adb5bd; min-width: 30px; height: 30px; padding: 0 6px; border-radius: 5px; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                        | false ->
                                            attr.style "background: #ffffff; border: 1px solid #dee2e6; color: #495057; min-width: 30px; height: 30px; padding: 0 6px; border-radius: 5px; cursor: pointer; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                            on.click (fun _ -> dispatch (GoToGalleryPage 1))
                                        text "«"
                                    }

                                    // Previous Page Button
                                    button {
                                        attr.``class`` "hywe-btn hywe-btn-sm"
                                        attr.title "Previous page"
                                        match currentPage <= 1 with
                                        | true ->
                                            attr.disabled true
                                            attr.style "opacity: 0.35; cursor: not-allowed; background: #f8f9fa; border: 1px solid #dee2e6; color: #adb5bd; min-width: 30px; height: 30px; padding: 0 8px; border-radius: 5px; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                        | false ->
                                            attr.style "background: #ffffff; border: 1px solid #dee2e6; color: #495057; min-width: 30px; height: 30px; padding: 0 8px; border-radius: 5px; cursor: pointer; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                            on.click (fun _ -> dispatch (GoToGalleryPage (currentPage - 1)))
                                        text "‹"
                                    }

                                    // Page Number Buttons with Ellipses
                                    let paginationItems =
                                        match totalPages <= 7 with
                                        | true -> [ 1 .. totalPages ] |> List.map Some
                                        | false ->
                                            match currentPage <= 4 with
                                            | true -> ([ 1 .. 5 ] |> List.map Some) @ [ None; Some totalPages ]
                                            | false ->
                                                match currentPage >= totalPages - 3 with
                                                | true -> [ Some 1; None ] @ ([ totalPages - 4 .. totalPages ] |> List.map Some)
                                                | false -> [ Some 1; None; Some (currentPage - 1); Some currentPage; Some (currentPage + 1); None; Some totalPages ]

                                    for item in paginationItems do
                                        match item with
                                        | Some pageNum ->
                                            button {
                                                attr.``class`` "hywe-btn hywe-btn-sm"
                                                match pageNum = currentPage with
                                                | true ->
                                                    attr.disabled true
                                                    attr.style "background: #212529; border: 1px solid #212529; color: #ffffff; min-width: 30px; height: 30px; padding: 0 6px; border-radius: 5px; font-weight: 600; cursor: default; display: inline-flex; align-items: center; justify-content: center; box-shadow: 0 1px 2px rgba(0,0,0,0.1);"
                                                | false ->
                                                    attr.style "background: #ffffff; border: 1px solid #dee2e6; color: #495057; min-width: 30px; height: 30px; padding: 0 6px; border-radius: 5px; cursor: pointer; font-weight: 500; display: inline-flex; align-items: center; justify-content: center;"
                                                    on.click (fun _ -> dispatch (GoToGalleryPage pageNum))
                                                text (string pageNum)
                                            }
                                        | None ->
                                            span {
                                                attr.style "min-width: 22px; height: 30px; display: inline-flex; align-items: center; justify-content: center; color: #868e96; font-size: 0.85rem; user-select: none;"
                                                text "…"
                                            }

                                    // Next Page Button
                                    button {
                                        attr.``class`` "hywe-btn hywe-btn-sm"
                                        attr.title "Next page"
                                        match currentPage >= totalPages with
                                        | true ->
                                            attr.disabled true
                                            attr.style "opacity: 0.35; cursor: not-allowed; background: #f8f9fa; border: 1px solid #dee2e6; color: #adb5bd; min-width: 30px; height: 30px; padding: 0 8px; border-radius: 5px; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                        | false ->
                                            attr.style "background: #ffffff; border: 1px solid #dee2e6; color: #495057; min-width: 30px; height: 30px; padding: 0 8px; border-radius: 5px; cursor: pointer; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                            on.click (fun _ -> dispatch (GoToGalleryPage (currentPage + 1)))
                                        text "›"
                                    }

                                    // Last Page Button
                                    button {
                                        attr.``class`` "hywe-btn hywe-btn-sm"
                                        attr.title "Last page"
                                        match currentPage >= totalPages with
                                        | true ->
                                            attr.disabled true
                                            attr.style "opacity: 0.35; cursor: not-allowed; background: #f8f9fa; border: 1px solid #dee2e6; color: #adb5bd; min-width: 30px; height: 30px; padding: 0 6px; border-radius: 5px; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                        | false ->
                                            attr.style "background: #ffffff; border: 1px solid #dee2e6; color: #495057; min-width: 30px; height: 30px; padding: 0 6px; border-radius: 5px; cursor: pointer; font-weight: bold; display: inline-flex; align-items: center; justify-content: center;"
                                            on.click (fun _ -> dispatch (GoToGalleryPage totalPages))
                                        text "»"
                                    }
                                }
                            }
                }
            }
        }
