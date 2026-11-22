module NodeActions

open System
open Elmish
open Bolero
open Bolero.Html
open TreeTypes

// --------------------
// UI Action Registry
// --------------------

type NodeActionUI = {
    Logic: NodeActionLogic
    RenderConfirm: (SubMsg -> unit) -> SubModel -> TreeNode -> Node
}

let private cancelButton dispatch =
    button {
        attr.``class`` "nodebutton2"
        on.stopPropagation "pointerdown" true
        on.stopPropagation "click" true
        on.pointerdown (fun _ -> dispatch CancelAction)
        text "↺"
    }

let deleteAction = {
    Logic = Actions.deleteActionLogic
    RenderConfirm = fun dispatch _ node ->
        concat {
            button {
                attr.``class`` "nodeweight node-action-btn delete"
                on.stopPropagation "pointerdown" true
                on.stopPropagation "click" true
                on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Delete)))
                text "DELETE"
            }
            cancelButton dispatch
        }
}

let elevateAction = {
    Logic = Actions.elevateActionLogic
    RenderConfirm = fun dispatch _ node ->
        concat {
            input {
                attr.``class`` "nodeweight node-action-input"
                attr.``type`` "text"
                attr.value (string node.Extrusion)
                on.input (fun ev -> dispatch (ActionInput (node.Id, ActionIds.Elevate, string ev.Value)))
                on.stopPropagation "pointerdown" true
                on.stopPropagation "click" true
            }
            button {
                attr.``class`` "nodename node-action-btn elevate"
                on.stopPropagation "pointerdown" true
                on.stopPropagation "click" true
                on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Elevate)))
                text "ELEVATE"
            }
            cancelButton dispatch
        }
}

let nestAction = {
    Logic = Actions.nestActionLogic
    RenderConfirm = fun dispatch model node ->
        let nextNestId = match model.Nests.IsEmpty with true -> 1 | false -> (model.Nests.Keys |> Seq.max) + 1
        concat {
            div {
                attr.``class`` "nodeweight node-action-input node-action-tag"
                text $"N{nextNestId}"
            }
            button {
                attr.``class`` "nodename node-action-btn nest"
                on.stopPropagation "pointerdown" true
                on.stopPropagation "click" true
                on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Nest)))
                text "NEST"
            }
            cancelButton dispatch
        }
}

let uiRegistry = [
    deleteAction
    elevateAction
    nestAction
]

let findAction id = uiRegistry |> List.tryFind (fun a -> a.Logic.LogicId = id)
