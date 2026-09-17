[<AutoOpen>]
module TreeTypes

open System
open Elmish

/// <summary>
/// Hierarchical node representing a spatial room/bubble with position, weight,
/// extrusion, base shape, and child sub-nodes.
/// </summary>
type TreeNode =
    { Id: Guid
      Name: string
      Weight: string
      X: float
      Y: float
      Children: TreeNode list
      Level: int
      Extrusion: float
      Base: string option
      Color: string option }

/// <summary>
/// Cached SVG viewBox and client rect metrics for coordinate transformations.
/// </summary>
type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

/// <summary> 2D coordinates in SVG viewport space. </summary>
type SvgPoint = { SvgX: float; SvgY: float }

/// <summary> Unique identifier for a node action command. </summary>
type ActionId = string

/// <summary> Standard action identifier constants for node manipulation. </summary>
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

/// <summary>
/// Abstract pointer event data to decouple the tree model from AspNetCore.Components.Web.
/// </summary>
type PointerEventData = { ClientX: float; ClientY: float }

/// <summary>
/// State model for the node hierarchy tree, tracking levels, nests, dragging, and menus.
/// </summary>
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

/// <summary> Messages representing user actions and updates within the tree view. </summary>
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

/// <summary>
/// Encapsulates the execution logic, input handling, and applicability rules for a node action.
/// </summary>
type NodeActionLogic = {
    LogicId: ActionId
    LogicLabel: string
    IsApplicable: SubModel -> TreeNode -> bool
    IsDisabled: SubModel -> TreeNode -> bool
    Execute: SubModel -> TreeNode -> SubModel * Cmd<SubMsg>
    HandleInput: (SubModel -> TreeNode -> string -> SubModel * Cmd<SubMsg>) option
}
