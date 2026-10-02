module NodeElement

open System
open Elmish
open Bolero
open Bolero.Html
open TreeTypes
open TreeOps
open NodeActions

// --------------------
// Domain Helpers
// --------------------

/// <summary> Resolves the active working tree (nest or level root) for the model. </summary>
let getCurrentTree (model: SubModel) : TreeNode = 
    match model.ActiveNest with
    | Some nId -> 
        model.Nests 
        |> Map.tryFind nId 
        |> Option.defaultValue (model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0])
    | None -> 
        model.Levels 
        |> Map.tryFind model.ActiveLevel 
        |> Option.defaultValue model.Levels.[0]

// --------------------
// Node State & Style Computation
// --------------------

type private NodeVisualState = {
    IsRoot: bool
    IsSelected: bool
    IsConfirming: bool
    IsDropTarget: bool
    IsAnchorForThisView: bool
    IsElevated: bool
    NestIdOpt: int option
    IsNestAnchor: bool
    OuterClasses: string
    OuterStyle: string
    InnerStyle: string
}

let private computeVisualState (model: SubModel) (node: TreeNode) (isAffected: bool) : NodeVisualState =
    let currentTree = getCurrentTree model
    let isRoot = node.Id = currentTree.Id
    let isSelected = model.SelectedNodeId = Some node.Id
    let isConfirming = model.ConfirmingId = Some node.Id
    let isDropTarget = model.DropTargetId = Some node.Id
    
    let isAnchorForThisView = 
        model.ActiveLevel > 0 && 
        (model.LevelAnchors |> Map.tryFind model.ActiveLevel = Some node.Id)
        
    let isElevatedAnchor = 
        model.LevelAnchors |> Map.exists (fun lvl anchorId -> lvl > model.ActiveLevel && anchorId = node.Id)
        
    let isElevated = isElevatedAnchor || (node.Color = Some "#3498db")
    let nestIdOpt = model.NestAnchors |> Map.tryPick (fun k v -> if v = node.Id then Some k else None)
    let isNestAnchor = nestIdOpt.IsSome || (node.Color = Some "#2ecc71")

    let outerClasses = 
        [ yield "node-outer"
          if isSelected then yield "is-selected"
          if isAffected && model.ActiveActionId = ActionIds.Delete && not isRoot then yield "is-affected"
          if isConfirming then
              match model.ActiveActionId with
              | ActionIds.Delete -> yield "is-confirming"
              | ActionIds.Elevate -> yield "is-elevating is-elevated"
              | ActionIds.Nest -> yield "is-nesting is-nested"
              | _ -> ()
          if isNestAnchor then yield "is-nesting is-nested"
          if model.DraggingId = Some node.Id && not isRoot then yield "is-dragging"
          if isDropTarget then
              match model.DropTargetMode with
              | Some DropAsChild -> yield "is-drop-target drop-child"
              | Some DropBefore -> yield "is-drop-target drop-before"
              | Some DropAfter -> yield "is-drop-target drop-after"
              | None -> yield "is-drop-target"
          if isElevated then yield "is-elevated is-elevating" ]
        |> String.concat " "

    let outerStyle =
        if isElevated then "pointer-events:auto; background-color: #3498db !important; filter: drop-shadow(0 0 4px rgba(52, 152, 219, 0.5));"
        elif isNestAnchor then "pointer-events:auto; background-color: #2ecc71 !important; filter: drop-shadow(0 0 4px rgba(46, 204, 113, 0.5));"
        elif isConfirming && model.ActiveActionId = ActionIds.Delete then "pointer-events:auto; background-color: #e74c3c !important; filter: drop-shadow(0 0 4px rgba(231, 76, 60, 0.5));"
        elif isAffected && model.ActiveActionId = ActionIds.Delete && not isRoot then "pointer-events:auto; background-color: #E67E22 !important; filter: drop-shadow(0 0 4px rgba(230, 126, 34, 0.5));"
        else "pointer-events:auto;"

    let innerStyle =
        if isElevated then "background-color: #ebf5fb !important;"
        elif isNestAnchor then "background-color: #eafaf1 !important;"
        elif isConfirming && model.ActiveActionId = ActionIds.Delete then "background-color: #fdedec !important;"
        elif isAffected && model.ActiveActionId = ActionIds.Delete && not isRoot then "background-color: #fef5ee !important;"
        else "background-color: white;"

    { IsRoot = isRoot
      IsSelected = isSelected
      IsConfirming = isConfirming
      IsDropTarget = isDropTarget
      IsAnchorForThisView = isAnchorForThisView
      IsElevated = isElevated
      NestIdOpt = nestIdOpt
      IsNestAnchor = isNestAnchor
      OuterClasses = outerClasses
      OuterStyle = outerStyle
      InnerStyle = innerStyle }

// --------------------
// Corner SVG Icon Templates
// --------------------

type SvgMove = Template<"""<svg viewBox="0 0 24 24" class="corner-svg" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><polyline points="5 9 2 12 5 15"/><polyline points="9 5 12 2 15 5"/><polyline points="15 19 12 22 9 19"/><polyline points="19 9 22 12 19 15"/><line x1="2" y1="12" x2="22" y2="12"/><line x1="12" y1="2" x2="12" y2="22"/></svg>""">

type SvgDelete = Template<"""<svg viewBox="0 0 24 24" class="corner-svg" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"/></svg>""">

type SvgElevate = Template<"""<svg viewBox="0 0 24 24" class="corner-svg" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>""">

type SvgNest = Template<"""<svg viewBox="0 0 24 24" class="corner-svg" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2"/><rect x="8" y="8" width="8" height="8" rx="1"/></svg>""">

type SvgAdd = Template<"""<svg viewBox="0 0 24 24" class="corner-svg" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round"><line x1="12" y1="5" x2="12" y2="19"/><line x1="5" y1="12" x2="19" y2="12"/></svg>""">

// --------------------
// Five Corner Badges (Hexagon Vertices Except Top)
// --------------------

/// <summary> Top-Left vertex: Move node four-sided arrow drag handle. </summary>
let private renderMoveBadge (nodeId: Guid) (dispatch: SubMsg -> unit) : Node =
    div {
        attr.``class`` "node-corner-badge badge-move"
        attr.title "Move node"
        on.stopPropagation "pointerdown" true
        on.stopPropagation "click" true
        on.pointerdown (fun ev -> 
            dispatch (NodePointerDown (nodeId, { ClientX = float ev.ClientX; ClientY = float ev.ClientY; Buttons = int ev.Buttons }))
        )
        SvgMove().Elt()
    }

/// <summary> Bottom-Left vertex: Delete node action. </summary>
let private renderDeleteBadge (nodeId: Guid) (dispatch: SubMsg -> unit) : Node =
    div {
        attr.``class`` "node-corner-badge badge-delete"
        attr.title "Delete node"
        on.stopPropagation "pointerdown" true
        on.stopPropagation "click" true
        on.pointerdown (fun _ -> dispatch (PrepareAction (nodeId, ActionIds.Delete)))
        SvgDelete().Elt()
    }

/// <summary> Top-Right vertex: Elevate node action. </summary>
let private renderElevateBadge (nodeId: Guid) (dispatch: SubMsg -> unit) : Node =
    div {
        attr.``class`` "node-corner-badge badge-elevate"
        attr.title "Elevate node"
        on.stopPropagation "pointerdown" true
        on.stopPropagation "click" true
        on.pointerdown (fun _ -> dispatch (PrepareAction (nodeId, ActionIds.Elevate)))
        SvgElevate().Elt()
    }

/// <summary> Bottom-Right vertex: Nest node action or jump to existing nest. </summary>
let private renderNestBadge (node: TreeNode) (nestIdOpt: int option) (dispatch: SubMsg -> unit) : Node =
    match nestIdOpt with
    | Some nId ->
        div {
            attr.``class`` "node-corner-badge badge-nest"
            attr.title $"Jump to Nest N{nId}"
            on.stopPropagation "pointerdown" true
            on.stopPropagation "click" true
            on.pointerdown (fun _ -> dispatch (SetNest nId))
            text $"N{nId}"
        }
    | None ->
        div {
            attr.``class`` "node-corner-badge badge-nest"
            attr.title "Nest node"
            on.stopPropagation "pointerdown" true
            on.stopPropagation "click" true
            on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, ActionIds.Nest)))
            SvgNest().Elt()
        }

/// <summary> Bottom vertex: Add child node action. </summary>
let private renderAddBadge (nodeId: Guid) (dispatch: SubMsg -> unit) : Node =
    div {
        attr.``class`` "node-corner-badge badge-add"
        attr.title "Add child node"
        on.stopPropagation "pointerdown" true
        on.stopPropagation "click" true
        on.pointerdown (fun _ -> dispatch (AddChild nodeId))
        SvgAdd().Elt()
    }

let private renderDropIndicators (dropTargetMode: DropMode option) : Node =
    match dropTargetMode with
    | Some DropAsChild ->
        div {
            attr.``class`` "drop-badge-child"
            text "↳ child"
        }
    | Some DropBefore ->
        div { attr.``class`` "drop-line-before" }
    | Some DropAfter ->
        div { attr.``class`` "drop-line-after" }
    | None -> 
        empty()

let private renderInlineControls 
    (node: TreeNode) 
    (state: NodeVisualState) 
    (dispatch: SubMsg -> unit) : Node =
    concat {
        // Inline Name / Label Input
        input {
            attr.``class`` "nodename"
            attr.value node.Name
            "readonly" => (state.IsAnchorForThisView || not state.IsSelected)
            on.stopPropagation "pointerdown" true
            on.stopPropagation "click" true
            on.pointerdown (fun _ -> 
                if not state.IsSelected then
                    dispatch (SelectNode (Some node.Id))
            )
            on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
        }

        // Inline Weight / Area Input
        input {
            attr.``class`` "nodeweight"
            attr.value node.Weight
            "readonly" => (state.IsAnchorForThisView || not state.IsSelected)
            on.stopPropagation "pointerdown" true
            on.stopPropagation "click" true
            on.pointerdown (fun _ -> 
                if not state.IsSelected then
                    dispatch (SelectNode (Some node.Id))
            )
            on.input (fun e -> dispatch (UpdateWeight (node.Id, string e.Value)))
        }
    }

// --------------------
// Public Views
// --------------------

/// <summary> Renders the floating ghost representation during node dragging. </summary>
let renderDragGhost (draggedNode: TreeNode) (pt: SvgPoint) : Node =
    div {
        attr.``class`` "node-drag-ghost"
        attr.style $"position:absolute; left:{pt.SvgX - 30.0}px; top:{pt.SvgY - 35.0}px; width:60px; height:60px; pointer-events:none; z-index:1000;"
        div {
            attr.``class`` "node-outer ghost-outer"
            div {
                attr.``class`` "node-inner ghost-inner"
                div {
                    attr.``class`` "nodename"
                    attr.style "border:none; text-align:center; font-weight:bold; color:#2980b9; pointer-events:none; margin:auto;"
                    text draggedNode.Name
                }
                div {
                    attr.``class`` "nodeweight"
                    attr.style "border:none; text-align:center; font-size:10px; color:#555; pointer-events:none; margin:auto;"
                    text draggedNode.Weight
                }
            }
        }
    }

/// <summary>
/// Renders an individual hexagonal tree node with 5 corner badges (Move, Delete on left;
/// Elevate, Nest on right; Add at bottom) and inline name/weight inputs.
/// </summary>
let renderNode 
    (node: TreeNode) 
    (prefix: string) 
    (model: SubModel) 
    (isAffected: bool) 
    (colorList: string[]) 
    (allNodes: TreeNode list) 
    (dispatch: SubMsg -> unit) : Node =

    let state = computeVisualState model node isAffected

    div {
        attr.style $"position:absolute; left:{node.X - 30.0}px; top:{node.Y - 35.0}px; width:60px; height:60px; pointer-events:none;"
        
        // Five Corner Badges (only visible when selected and not confirming)
        if state.IsSelected && not state.IsConfirming then
            concat {
                // Left corners: Move (top-left) & Delete (bottom-left)
                if not state.IsRoot then
                    renderMoveBadge node.Id dispatch

                if Actions.deleteActionLogic.IsApplicable model node then
                    renderDeleteBadge node.Id dispatch

                // Right corner: Elevate (top-right)
                if Actions.elevateActionLogic.IsApplicable model node then
                    renderElevateBadge node.Id dispatch

                // Right corner: Nest (bottom-right)
                match state.NestIdOpt with
                | Some nId ->
                    renderNestBadge node (Some nId) dispatch
                | None when Actions.nestActionLogic.IsApplicable model node ->
                    renderNestBadge node None dispatch
                | None -> ()

                // Bottom corner: Add (+)
                if not state.IsNestAnchor then
                    renderAddBadge node.Id dispatch
            }

        // Hexagon Node Body
        div {
            attr.``class`` state.OuterClasses
            attr.style state.OuterStyle 
            on.stopPropagation "pointerdown" true
            on.stopPropagation "click" true
            on.pointerdown (fun _ -> 
                dispatch (SelectNode (Some node.Id))
            )
            
            div {
                attr.``class`` "node-inner"
                attr.style state.InnerStyle

                if state.IsConfirming then
                    match NodeActions.findAction model.ActiveActionId with
                    | Some action -> action.RenderConfirm dispatch model node
                    | None -> empty()
                else
                    renderInlineControls node state dispatch
            }

            // Drop Target Visual Indicators
            if state.IsDropTarget then
                renderDropIndicators model.DropTargetMode
        }
    }

/// <summary> Renders the properties dock bar for precision editing of the currently selected node. </summary>
let renderPropertiesBar (model: SubModel) (dispatch: SubMsg -> unit) : Node =
    let currentTree = getCurrentTree model
    let selectedNodeOpt = model.SelectedNodeId |> Option.bind (fun id -> TreeOps.findNodeById id currentTree)

    match selectedNodeOpt with
    | Some node ->
        let isRoot = node.Id = currentTree.Id
        let isAnchorForThisView = 
            model.ActiveLevel > 0 && 
            (model.LevelAnchors |> Map.tryFind model.ActiveLevel = Some node.Id)
        let nestIdOpt = model.NestAnchors |> Map.tryPick (fun k v -> if v = node.Id then Some k else None)
        
        div {
            attr.``class`` "node-properties-bar"
            
            div {
                attr.``class`` "prop-section-info"
                match model.ActiveNest with
                | Some nId ->
                    span { attr.``class`` "prop-badge prop-badge-level"; text $"L{node.Level}" }
                    span { attr.``class`` "prop-badge prop-badge-nest"; text $"N{nId}" }
                | None ->
                    if isRoot && model.ActiveLevel = 0 then
                        span { attr.``class`` "prop-badge prop-badge-root"; text "ROOT" }
                    elif isRoot then
                        span { attr.``class`` "prop-badge prop-badge-level"; text $"L{model.ActiveLevel}" }
                    else
                        span { attr.``class`` "prop-badge prop-badge-level"; text $"L{node.Level}" }
            }

            div {
                attr.``class`` "prop-field"
                span { attr.``class`` "prop-label"; text "Label" }
                input {
                    attr.``class`` "prop-input"
                    attr.value node.Name
                    attr.placeholder "Name"
                    "readonly" => isAnchorForThisView
                    on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
                }
            }

            div {
                attr.``class`` "prop-field prop-field-sm"
                span { attr.``class`` "prop-label"; text "Area" }
                div {
                    attr.``class`` "prop-stepper"
                    button {
                        attr.``class`` "prop-step-btn"
                        attr.title "Decrease Area (-1)"
                        "disabled" => isAnchorForThisView
                        "onpointerdown:stopPropagation" => true
                        on.pointerdown (fun _ ->
                            if not isAnchorForThisView then
                                let currentVal = match Double.TryParse node.Weight with true, v -> int (round v) | _ -> 10
                                let nextVal = max 1 (currentVal - 1)
                                dispatch (UpdateWeight (node.Id, string nextVal))
                        )
                        text "−"
                    }
                    input {
                        attr.``class`` "prop-input prop-input-stepper"
                        attr.value node.Weight
                        attr.placeholder "Area"
                        "readonly" => isAnchorForThisView
                        on.input (fun e -> dispatch (UpdateWeight (node.Id, string e.Value)))
                    }
                    button {
                        attr.``class`` "prop-step-btn"
                        attr.title "Increase Area (+1)"
                        "disabled" => isAnchorForThisView
                        "onpointerdown:stopPropagation" => true
                        on.pointerdown (fun _ ->
                            if not isAnchorForThisView then
                                let currentVal = match Double.TryParse node.Weight with true, v -> int (round v) | _ -> 10
                                let nextVal = currentVal + 1
                                dispatch (UpdateWeight (node.Id, string nextVal))
                        )
                        text "+"
                    }
                }
            }

            div {
                attr.``class`` "prop-actions"
                if nestIdOpt.IsNone then
                    button {
                        attr.``class`` "prop-action-btn"
                        attr.title "Add Child"
                        on.pointerdown (fun _ -> dispatch (AddChild node.Id))
                        SvgAdd().Elt()
                        span { text "Child" }
                    }
                if Actions.deleteActionLogic.IsApplicable model node then
                    button {
                        attr.``class`` "prop-action-btn prop-btn-delete"
                        attr.title (if isRoot && model.ActiveNest.IsSome then "Delete Nest" else "Delete Node")
                        on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, ActionIds.Delete)))
                        SvgDelete().Elt()
                        span { text (if isRoot && model.ActiveNest.IsSome then "Delete Nest" else "Delete") }
                    }
                if Actions.elevateActionLogic.IsApplicable model node then
                    button {
                        attr.``class`` "prop-action-btn prop-btn-elevate"
                        attr.title "Elevate Node"
                        on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, ActionIds.Elevate)))
                        SvgElevate().Elt()
                        span { text "Elevate" }
                    }
                match nestIdOpt with
                | Some nId ->
                    button {
                        attr.``class`` "prop-action-btn prop-btn-nest"
                        attr.title $"Jump to Nest N{nId}"
                        on.pointerdown (fun _ -> dispatch (SetNest nId))
                        text $"N{nId}"
                    }
                | None when Actions.nestActionLogic.IsApplicable model node ->
                    button {
                        attr.``class`` "prop-action-btn prop-btn-nest"
                        attr.title "Nest Node"
                        on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, ActionIds.Nest)))
                        SvgNest().Elt()
                        span { text "Nest" }
                    }
                | _ -> ()

                button {
                    attr.``class`` "prop-action-btn prop-close-btn"
                    attr.title "Deselect"
                    on.pointerdown (fun _ -> dispatch (SelectNode None))
                    text "✕"
                }
            }
        }
    | None ->
        empty()
