namespace Hywe.Node

open System
open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components.Web
open Hywe.Core.Lexel

// --------------------
// UI Action Registry
// --------------------

type NodeActionUI = {
    Logic: NodeActionLogic
    RenderConfirm: (SubMsg -> unit) -> SubModel -> TreeNode -> Node
}

module NodeActionsUI =

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
                button {
                    attr.``class`` "nodename"
                    attr.style "color: #3498db; font-weight: bold; cursor: pointer; border: none !important; width: 54px; padding: 0; margin: auto 0 2px 0; background: none;"
                    "onpointerdown:stopPropagation" => true
                    on.pointerdown (fun _ -> dispatch (ExecuteAction (node.Id, ActionIds.Elevate)))
                    text "ELEVATE"
                }
                input {
                    attr.``class`` "nodeweight"
                    attr.``type`` "text"
                    attr.style "border: none !important; margin: 2px 0 auto 0; width: 54px;"
                    attr.value (string node.Extrusion)
                    on.input (fun ev -> dispatch (ActionInput (node.Id, ActionIds.Elevate, string ev.Value)))
                    "onpointerdown:stopPropagation" => true
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
        RenderConfirm = fun dispatch _ node ->
            concat {
                button {
                    attr.``class`` "nodename"
                    attr.style "color: #2ecc71; font-weight: bold; cursor: pointer; border: none !important; width: 54px; padding: 0; margin: auto; background: none;"
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

// --------------------
// Rendering Engine
// --------------------

module NodeTree =

    type svLn = Template<"""<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${color}" stroke-width="${width}"/>""">

    let getSvgInfo (js: IJSRuntime) =
        async {
            let! el = js.InvokeAsync<System.Text.Json.JsonElement>("getSvgInfo", [| box "tree-canvas-svg" |]).AsTask() |> Async.AwaitTask
            return { 
                ViewBoxX = el.GetProperty("viewBoxX").GetDouble()
                ViewBoxY = el.GetProperty("viewBoxY").GetDouble()
                ViewBoxW = el.GetProperty("viewBoxW").GetDouble()
                ViewBoxH = el.GetProperty("viewBoxH").GetDouble()
                ClientLeft = el.GetProperty("left").GetDouble()
                ClientTop = el.GetProperty("top").GetDouble()
                ClientW = el.GetProperty("width").GetDouble()
                ClientH = el.GetProperty("height").GetDouble()
            }
        }

    let toSvgCoords (info: SvgInfo) (clientX: float) (clientY: float) : SvgPoint =
        { SvgX = info.ViewBoxX + (clientX - info.ClientLeft) * info.ViewBoxW / info.ClientW
          SvgY = info.ViewBoxY + (clientY - info.ClientTop) * info.ViewBoxH / info.ClientH }

    let getCurrentTree model = 
        match model.ActiveNest with
        | Some nId -> model.Nests |> Map.tryFind nId |> Option.defaultValue (model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0])
        | None -> model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]

    let updateCurrentTree model newTree =
        match model.ActiveNest with
        | Some nId -> { model with Nests = model.Nests |> Map.add nId newTree }
        | None -> 
            let finalLevels = TreeOps.syncHierarchy (model.Levels |> Map.add model.ActiveLevel newTree) model.LevelAnchors 0
            { model with Levels = finalLevels }

    let handleExecuteAction id actionId model =
        match NodeActionsUI.findAction actionId with
        | Some action ->
            let currentTree = getCurrentTree model
            match TreeOps.findNodeById id currentTree with
            | Some node -> action.Logic.Execute model node
            | None -> model, Cmd.none
        | None -> model, Cmd.none

    let handleNodeUpdate msg model =
        let currentTree = getCurrentTree model
        match msg with
        | UpdateName (id, name) ->
            let newRoot = TreeOps.updateNodeById id (fun n -> { n with Name = name }) currentTree
            updateCurrentTree model newRoot, Cmd.none
        | UpdateWeight (id, weight) ->
            let sanitizedWeight = match Double.TryParse weight with true, v -> (int (round v)).ToString() | _ -> weight
            let newRoot = TreeOps.updateNodeById id (fun n -> { n with Weight = sanitizedWeight }) currentTree
            updateCurrentTree model newRoot, Cmd.none
        | UpdateExtrusion (id, newVal) ->
            let extrusion = match Double.TryParse newVal with true, v -> max 0.1 v | _ -> currentTree.Extrusion
            let updatedRoot = TreeOps.updateNodeById id (fun n -> { n with Extrusion = extrusion }) currentTree
            updateCurrentTree model updatedRoot, Cmd.none
        | ActionInput (id, actionId, value) ->
            match NodeActionsUI.findAction actionId with
            | Some action ->
                match action.Logic.HandleInput with
                | Some handler ->
                    let currentTree = getCurrentTree model
                    match TreeOps.findNodeById id currentTree with
                    | Some node -> handler model node value
                    | None -> model, Cmd.none
                | None -> model, Cmd.none
            | None -> model, Cmd.none
        | _ -> model, Cmd.none

    let handlePointerEvent msg js model =
        match msg with
        | PointerDown data ->
            let currentTree = getCurrentTree model
            { model with PointerDownPos = None; PendingDragId = None }, Cmd.OfAsync.perform (fun _ -> getSvgInfo js) () (fun info -> 
                let pt = toSvgCoords info (float data.ClientX) (float data.ClientY)
                let rec findNode node =
                    match pt.SvgX >= node.X - 30.0 && pt.SvgX <= node.X + 30.0 &&
                          pt.SvgY >= node.Y - 35.0 && pt.SvgY <= node.Y + 25.0 with
                    | true -> Some node.Id
                    | false -> node.Children |> List.tryPick findNode
                match findNode currentTree with
                | Some id when id <> currentTree.Id -> DragStartInternal (id, info, pt)
                | _ -> PointerUpInternal
            )
        | PointerMove data ->
            let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
            match model.LastMoveMs with
            | Some last when nowMs - last < 16.0 -> model, Cmd.none
            | _ ->
                match model.DraggingId, model.SvgInfo with
                | Some _, Some info ->
                    let currentTree = getCurrentTree model
                    let pt = toSvgCoords info (float data.ClientX) (float data.ClientY)
                    let rec findNode node =
                        match pt.SvgX >= node.X - 30.0 && pt.SvgX <= node.X + 30.0 &&
                              pt.SvgY >= node.Y - 35.0 && pt.SvgY <= node.Y + 25.0 with
                        | true -> Some node.Id
                        | false -> node.Children |> List.tryPick findNode
                    { model with DropTargetId = findNode currentTree; LastMoveMs = Some nowMs }, Cmd.none
                | None, Some info -> 
                    match model.PendingDragId, model.PointerDownPos with
                    | Some pendingId, Some startPt ->
                        let pt = toSvgCoords info (float data.ClientX) (float data.ClientY)
                        let dist = sqrt ((pt.SvgX - startPt.SvgX)**2.0 + (pt.SvgY - startPt.SvgY)**2.0)
                        match dist > 5.0 with
                        | true -> { model with DraggingId = Some pendingId; PendingDragId = None; LastMoveMs = Some nowMs }, Cmd.none
                        | false -> { model with LastMoveMs = Some nowMs }, Cmd.none
                    | _ -> { model with LastMoveMs = Some nowMs }, Cmd.none
                | _ -> { model with LastMoveMs = Some nowMs }, Cmd.none
        | PointerUp ->
            let m = { model with PendingDragId = None; PointerDownPos = None }
            let currentTree = getCurrentTree m
            match m.DraggingId, m.DropTargetId with
            | Some sourceId, Some targetId when sourceId <> targetId ->
                let sourceNode = TreeOps.findNodeById sourceId currentTree
                match sourceNode with
                | Some sn when not (TreeOps.isDescendant targetId sn) ->
                    let (rootWithoutSource, extracted) = TreeOps.extractNode sourceId currentTree
                    match rootWithoutSource, extracted with
                    | Some rs, Some ex ->
                        let laidOut = fst (TreeOps.layoutTree (TreeOps.insertBefore targetId ex rs) 0 50.0)
                        let updatedModel = updateCurrentTree m laidOut
                        { updatedModel with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
                    | _ -> { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
                | _ -> { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
            | _ -> { m with DraggingId = None; DropTargetId = None; SvgInfo = None }, Cmd.none
        | DragStartInternal (id, info, pt) -> 
            { model with PendingDragId = Some id; SvgInfo = Some info; PointerDownPos = Some pt; DropTargetId = None }, Cmd.none
        | PointerUpInternal ->
            { model with DraggingId = None; PendingDragId = None; DropTargetId = None; SvgInfo = None; PointerDownPos = None }, Cmd.none
        | _ -> model, Cmd.none

    let updateSub (js: IJSRuntime) msg model =
        match msg with
        | OpenMenu id -> { model with ActiveMenuId = Some id; ConfirmingId = None }, Cmd.none
        | CloseMenu -> { model with ActiveMenuId = None }, Cmd.none
        | SetLevel lvl -> { model with ActiveLevel = lvl; ActiveNest = None; ActiveMenuId = None }, Cmd.none
        | SetNest nId -> { model with ActiveNest = Some nId; ActiveMenuId = None }, Cmd.none
        | SetTopExtrusion newVal ->
            let extr = match Double.TryParse newVal with true, v -> max 0.1 v | _ -> model.TopExtrusion
            { model with TopExtrusion = extr }, Cmd.none
        | PrepareAction (id, actionId) -> 
            { model with ConfirmingId = Some id; ActiveActionId = actionId; ActiveMenuId = None }, Cmd.none
        | CancelAction -> { model with ConfirmingId = None; ActiveActionId = ActionIds.NoAction }, Cmd.none
        | ExecuteAction (id, actionId) -> handleExecuteAction id actionId model
        | AddChild parentId ->
            let currentTree = getCurrentTree model
            let newChild = { Id = Guid.NewGuid(); Name = TreeOps.getRandomName(); Weight = TreeOps.getRandomWeight(); X = 0.0; Y = 0.0; Children = []; Level = model.ActiveLevel; Extrusion = 3.0; Base = None }
            let newRoot = TreeOps.updateNodeById parentId (fun n -> { n with Children = n.Children @ [newChild] }) currentTree
            let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
            updateCurrentTree model laidOut, Cmd.none
        | UpdateName _ | UpdateWeight _ | UpdateExtrusion _ | ActionInput _ -> handleNodeUpdate msg model
        | PointerDown _ | PointerMove _ | PointerUp | DragStartInternal _ | PointerUpInternal -> handlePointerEvent msg js model

    // --------------------
    // View
    // --------------------
    let renderNode (node: TreeNode) (prefix: string) (model: SubModel) (isAffected: bool) (colorList: string[]) (allNodes: TreeNode list) (dispatch: SubMsg -> unit) =
        let currentTree = getCurrentTree model
        let isRoot = node.Id = currentTree.Id
        let isConfirmingThis = model.ConfirmingId = Some node.Id
        let isMenuOpen = model.ActiveMenuId = Some node.Id
        let isDropTarget = model.DropTargetId = Some node.Id
        let hasHalo = node.Level > model.ActiveLevel
        
        let isNestAnchor = model.NestAnchors |> Map.exists (fun _ anchorId -> anchorId = node.Id)
        
        let outerClasses = 
            [ "node-outer"
              match isAffected && model.ActiveActionId = ActionIds.Delete && not isRoot with true -> "is-affected" | false -> ""
              match isConfirmingThis && not isRoot with
              | true -> 
                    match model.ActiveActionId with
                    | ActionIds.Delete -> "is-confirming"
                    | ActionIds.Elevate -> "is-elevating"
                    | ActionIds.Nest -> "is-nesting"
                    | _ -> ""
              | false -> ""
              match isNestAnchor && not isRoot with true -> "is-nesting" | false -> ""
              match model.DraggingId = Some node.Id && not isRoot with true -> "is-dragging" | false -> ""
              match isDropTarget && not isRoot with true -> "is-drop-target" | false -> ""
              match hasHalo with true -> "is-elevated" | false -> "" ]
            |> List.filter (fun s -> s <> "")
            |> String.concat " "

        div {
            attr.style $"position:absolute; left:{node.X - 30.0}px; top:{node.Y - 35.0}px; width:60px; height:60px; pointer-events:none;"
            
            div {
                attr.``class`` outerClasses
                attr.style "pointer-events:auto;" 
                
                div {
                    attr.``class`` "node-inner"
                    
                    let nodeIndex = allNodes |> List.tryFindIndex (fun n -> n.Id = node.Id) |> Option.defaultValue -1
                    let nodeColor = 
                        match nodeIndex >= 0 && nodeIndex < colorList.Length with
                        | true -> colorList.[nodeIndex]
                        | false -> "white"
                    
                    let nodeStyle = match nodeColor <> "white" with true -> sprintf "background-color: %s !important;" nodeColor | false -> ""
                    attr.style nodeStyle


                    match isConfirmingThis with
                    | true ->
                        match NodeActionsUI.findAction model.ActiveActionId with
                        | Some action -> action.RenderConfirm dispatch model node
                        | None -> empty()
                    | false ->
                        concat {
                            div {
                                attr.``class`` "node-menu-container"
                                "onpointerdown:stopPropagation" => true
                                on.pointerdown (fun _ -> dispatch (OpenMenu node.Id))
                                
                                text "☰"
                            }

                            let isAnchorForThisView = 
                                model.ActiveLevel > 0 && 
                                (model.LevelAnchors |> Map.tryFind model.ActiveLevel = Some node.Id)

                            input {
                                attr.``class`` "nodename"
                                attr.value node.Name
                                attr.disabled isAnchorForThisView
                                "onpointerdown:stopPropagation" => true
                                on.input (fun e -> dispatch (UpdateName (node.Id, string e.Value)))
                            }
                            input {
                                attr.``class`` "nodeweight"
                                attr.value node.Weight
                                attr.disabled isAnchorForThisView
                                "onpointerdown:stopPropagation" => true
                                on.input (fun e -> dispatch (UpdateWeight (node.Id, string e.Value)))
                            }
                            
                            button { 
                                attr.``class`` "nodebutton2"
                                "onpointerdown:stopPropagation" => true
                                on.pointerdown (fun _ -> dispatch (AddChild node.Id))
                                text "+" 
                            }
                        }
                }
            }

            match isMenuOpen with
            | true ->
                div {
                    attr.``class`` "node-menu-popup"
                    attr.style "pointer-events:auto;"
                    "onpointerdown:stopPropagation" => true
                    
                    forEach NodeActionsUI.uiRegistry (fun action ->
                        match action.Logic.IsApplicable model node with
                        | true ->
                            match action.Logic.IsDisabled model node with
                            | true ->
                                div {
                                    attr.``class`` "node-menu-item disabled"
                                    text action.Logic.LogicLabel
                                }
                            | false ->
                                div {
                                    attr.``class`` "node-menu-item"
                                    "onpointerdown:stopPropagation" => true
                                    on.pointerdown (fun _ -> dispatch (PrepareAction (node.Id, action.Logic.LogicId)))
                                    text action.Logic.LogicLabel
                                }
                        | false -> empty()
                    )
                }
            | false -> empty()
        }


    let viewTreeEditor (model: SubModel) (colorList: string[]) (dispatch: SubMsg -> unit) : Node =      

        let currentLvlRoot = getCurrentTree model
        let displayTree = currentLvlRoot
        let laidOutDisplayTree = displayTree

        let confirmingSubtree = model.ConfirmingId |> Option.bind (fun id -> TreeOps.findNodeById id currentLvlRoot)
        let isAffected nodeId =
            match confirmingSubtree with
            | Some root -> root.Id = nodeId || TreeOps.isDescendant nodeId root
            | None -> false

        let renderConnection (parent: TreeNode) (child: TreeNode) : Node =
            let affected = isAffected child.Id && model.ActiveActionId = ActionIds.Delete
            let color = if affected then "#E67E22" else "#888"
            let width = if affected then "1.5" else "1"
            svLn().x1($"{parent.X}").y1($"{parent.Y + 5.0}").x2($"{child.X}").y2($"{child.Y - 35.0}")
                 .color(color).width(width).Elt()

        let rec flattenTree node = node :: (node.Children |> List.collect flattenTree)
        let nodes = flattenTree laidOutDisplayTree
        let lines = laidOutDisplayTree.Children |> List.collect (fun c -> 
            let rec collect n = n.Children |> List.collect (fun child -> renderConnection n child :: collect child)
            renderConnection laidOutDisplayTree c :: collect c)
        
        let maxX = if nodes.IsEmpty then 60.0 else nodes |> List.map (fun n -> n.X) |> List.max
        let maxY = if nodes.IsEmpty then 60.0 else nodes |> List.map (fun n -> n.Y + 35.0) |> List.max
        let canvasWidth = maxX + 60.0
        let canvasHeight = maxY + 30.0

        let rec renderAll (node: TreeNode) (prefix: string) (colorList: string[]) (allNodes: TreeNode list) : Node =
            concat {
                renderNode node prefix model (isAffected node.Id) colorList allNodes dispatch
                for i, child in node.Children |> List.indexed do
                    renderAll child $"{prefix}.{i + 1}" colorList allNodes
            }


        let allNodes = flattenTree currentLvlRoot
        let elevations = Serialization.getElevations model
        let maxLevel = match model.Levels.IsEmpty with true -> 0 | false -> model.Levels.Keys |> Seq.max
        let containerClasses = 
            [ "tree-container"
              match model.DraggingId.IsSome with true -> "is-dragging-any" | false -> "" ]
            |> List.filter (fun s -> s <> "")
            |> String.concat " "

        let touchAction = match model.DraggingId.IsSome || model.PendingDragId.IsSome with true -> "none" | false -> "pan-x pan-y pinch-zoom"

        concat {
            // Level Nav
            div {
                attr.``class`` "level-controls-container"
                attr.style "width: fit-content; margin-bottom: 5px; z-index: 1000;"
                span { attr.``class`` "level-label"; text "LEVELS:" }
                forEach (List.init (maxLevel + 1) id) (fun i ->
                    let elv = match i < elevations.Length with true -> elevations.[i] | false -> 0.0
                    let elvStr = match elv = floor elv with true -> string (int elv) | false -> string elv
                    let levelTab =
                        button {
                            attr.``class`` (match model.ActiveLevel = i && model.ActiveNest.IsNone with true -> "level-tab active" | false -> "level-tab")
                            on.pointerdown (fun _ -> dispatch (SetLevel i))
                            text elvStr
                        }
                    
                    let nestTabs = 
                        model.Nests 
                        |> Map.toList 
                        |> List.filter (fun (_, tree) -> tree.Level = i)
                        |> List.map (fun (nId, _) ->
                            button {
                                attr.``class`` (match model.ActiveNest = Some nId with true -> "level-tab active" | false -> "level-tab")
                                attr.style "margin-left: 2px; color: #2ecc71;"
                                on.pointerdown (fun _ -> dispatch (SetNest nId))
                                text $"N{nId}"
                            }
                        )
                    
                    concat {
                        levelTab
                        for t in nestTabs do t
                    }
                )
                
                // Terminal Level Height Input
                let topElv = match elevations.Length > 0 with true -> elevations.[elevations.Length - 1] | false -> model.TopExtrusion
                let baseOfTop = match elevations.Length > 1 with true -> elevations.[elevations.Length - 2] | false -> 0.0
                
                div {
                    attr.style "display: inline-flex; align-items: center; margin-left: 4px;"
                    input {
                        attr.``type`` "text"
                        attr.``class`` "level-tab"
                        attr.style "width: 40px; text-align: center; outline: none; padding: 2px 0;"
                        attr.value (match topElv = floor topElv with true -> string (int topElv) | false -> string topElv)
                        on.input (fun ev -> 
                            let newVal = string ev.Value
                            match Double.TryParse newVal with
                            | true, v -> 
                                let relative = max 0.1 (v - baseOfTop)
                                dispatch (SetTopExtrusion (string relative))
                            | _ -> ()
                        )
                    }
                }
            }

            // Tree Container
            div {
                attr.``class`` containerClasses
                on.pointermove (fun ev -> dispatch (PointerMove { ClientX = float ev.ClientX; ClientY = float ev.ClientY }))
                on.pointerup (fun _ -> dispatch PointerUp)

                match model.ActiveMenuId.IsSome with
                | true ->
                    div {
                        attr.style "position:fixed; inset:0; z-index:90; background:transparent;"
                        on.click (fun _ -> dispatch CloseMenu)
                    }
                | false -> empty()

                div {
                    attr.id "tree-canvas-svg"
                    attr.``class`` "tree-canvas"
                    attr.style $"width:{canvasWidth}px; height:{max 150.0 canvasHeight}px; touch-action:{touchAction};"
                    on.pointerdown (fun ev -> dispatch (PointerDown { ClientX = float ev.ClientX; ClientY = float ev.ClientY }))
                    
                    svg {
                        attr.``class`` "tree-svg"
                        attr.style $"width:{canvasWidth}px; height:{canvasHeight}px;"
                        forEach lines (fun line -> line)
                    }
                    renderAll laidOutDisplayTree "1" colorList nodes
                }
            }
        }
