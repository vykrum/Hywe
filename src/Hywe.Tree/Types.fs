namespace Hywe.Node

open System
open Elmish

type TreeNode =
    { Id: Guid
      Name: string
      Weight: string
      X: float
      Y: float
      Children: TreeNode list
      Level: int
      Extrusion: float
      Base: string option }

type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

type SvgPoint = { SvgX: float; SvgY: float }

type ActionId = string

[<RequireQualifiedAccess>]
module ActionIds =
    [<Literal>]
    let Delete = "Delete"
    [<Literal>]
    let Elevate = "Elevate"
    [<Literal>]
    let Nest = "Nest"
    [<Literal>]
    let NoAction = "NoAction"

/// Abstract pointer event data to decouple from AspNetCore.Components.Web
type PointerEventData = { ClientX: float; ClientY: float }

type SubModel = 
    { Levels: Map<int, TreeNode>
      Nests: Map<int, TreeNode>
      ActiveLevel: int
      ActiveNest: int option
      LevelAnchors: Map<int, Guid>
      NestAnchors: Map<int, Guid>
      ConfirmingId: Guid option
      ActiveActionId: ActionId
      ActiveMenuId: Guid option
      DraggingId: Guid option
      PendingDragId: Guid option
      DropTargetId: Guid option
      SvgInfo: SvgInfo option
      PointerDownPos: SvgPoint option
      LastMoveMs: float option
      TopExtrusion: float }

type SubMsg =
    | OpenMenu of Guid
    | CloseMenu
    | SetLevel of int
    | SetNest of int
    | SetTopExtrusion of string
    | PrepareAction of Guid * ActionId
    | ExecuteAction of Guid * ActionId
    | ActionInput of Guid * ActionId * string
    | CancelAction
    | AddChild of Guid
    | UpdateName of Guid * string
    | UpdateWeight of Guid * string
    | UpdateExtrusion of Guid * string
    | PointerDown of PointerEventData
    | PointerMove of PointerEventData
    | PointerUp
    | DragStartInternal of Guid * SvgInfo * SvgPoint
    | PointerUpInternal

/// Logical definition of a Node Action
type NodeActionLogic = {
    LogicId: ActionId
    LogicLabel: string
    IsApplicable: SubModel -> TreeNode -> bool
    IsDisabled: SubModel -> TreeNode -> bool
    Execute: SubModel -> TreeNode -> SubModel * Cmd<SubMsg>
    HandleInput: (SubModel -> TreeNode -> string -> SubModel * Cmd<SubMsg>) option
}
