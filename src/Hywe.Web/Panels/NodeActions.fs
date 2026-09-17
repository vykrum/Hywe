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

let deleteAction = {
    Logic = Actions.deleteActionLogic
    RenderConfirm = fun dispatch _ node ->
        concat {
            button {
                attr.``class`` "nodeweight"
                attr.style "color: #e67e22; border: none !important; width: 54px; margin: auto; cursor: pointer; padding: 0; background: none;"
                "onpointerdown:stopPropagation" => true
                on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Delete)))
                text "DELETE"
            }
            button {
                attr.``class`` "nodebutton2"
                attr.style "color: #999; font-size: 10px;"
                "onpointerdown:stopPropagation" => true
                on.pointerdown (fun _ -> dispatch CancelAction)
                text "↺"
            }
        }
}

let elevateAction = {
    Logic = Actions.elevateActionLogic
    RenderConfirm = fun dispatch _ node ->
        concat {
            input {
                attr.``class`` "nodeweight"
                attr.``type`` "text"
                attr.style "border: none !important; width: 54px; position: absolute; top: 8px; left: 3px; background: transparent;"
                attr.value (string node.Extrusion)
                on.input (fun ev -> dispatch (ActionInput (node.Id, ActionIds.Elevate, string ev.Value)))
                "onpointerdown:stopPropagation" => true
            }
            button {
                attr.``class`` "nodename"
                attr.style "color: #3498db; font-weight: bold; cursor: pointer; border: none !important; width: 54px; padding: 0; margin: auto; transform: translateY(5px); background: none;"
                "onpointerdown:stopPropagation" => true
                on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Elevate)))
                text "ELEVATE"
            }
            button {
                attr.``class`` "nodebutton2"
                attr.style "color: #999; font-size: 10px;"
                "onpointerdown:stopPropagation" => true
                on.pointerdown (fun _ -> dispatch CancelAction)
                text "↺"
            }
        }
}

let nestAction = {
    Logic = Actions.nestActionLogic
    RenderConfirm = fun dispatch model node ->
        let nextNestId = match model.Nests.IsEmpty with true -> 1 | false -> (model.Nests.Keys |> Seq.max) + 1
        concat {
            div {
                attr.``class`` "nodeweight"
                attr.style "border: none !important; width: 54px; position: absolute; top: 8px; left: 3px; background: transparent; text-align: center; color: #2ecc71; pointer-events: none;"
                text $"N{nextNestId}"
            }
            button {
                attr.``class`` "nodename"
                attr.style "color: #2ecc71; font-weight: normal; font-size: 10px; cursor: pointer; border: none !important; width: 54px; padding: 0; margin: auto; transform: translateY(5px); background: none;"
                "onpointerdown:stopPropagation" => true
                on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Nest)))
                text "NEST"
            }
            button {
                attr.``class`` "nodebutton2"
                attr.style "color: #999; font-size: 10px;"
                "onpointerdown:stopPropagation" => true
                on.pointerdown (fun _ -> dispatch CancelAction)
                text "↺"
            }
        }
}

let uiRegistry = [
    deleteAction
    elevateAction
    nestAction
]

let findAction id = uiRegistry |> List.tryFind (fun a -> a.Logic.LogicId = id)
