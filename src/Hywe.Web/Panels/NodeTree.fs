module NodeTree

open System
open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open TreeTypes
open NodeActions
open NodeElement

// --------------------
// Rendering Engine
// --------------------

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
    match NodeActions.findAction actionId with
    | Some action ->
        let currentTree = getCurrentTree model
        match TreeOps.findNodeById id currentTree with
        | Some node when action.Logic.IsApplicable model node -> action.Logic.Execute model node
        | _ -> model, Cmd.none
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
        match NodeActions.findAction actionId with
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
    | NodePointerDown (nodeId, data) ->
        let currentTree = getCurrentTree model
        if nodeId = currentTree.Id then 
            { model with SelectedNodeId = Some nodeId; ActiveMenuId = None }, Cmd.none
        else
            { model with 
                SelectedNodeId = Some nodeId
                ActiveMenuId = None
                PointerDownPos = None
                PendingDragId = None
                DraggingId = None
                DropTargetId = None
                DropTargetMode = None
                DragPos = None },
            Cmd.OfAsync.perform (fun _ -> getSvgInfo js) () (fun info -> 
                let pt = toSvgCoords info (float data.ClientX) (float data.ClientY)
                DragStartInternal (nodeId, info, pt)
            )
    | PointerDown _ ->
        { model with 
            SelectedNodeId = None
            ActiveMenuId = None
            ConfirmingId = None
            ActiveActionId = ActionIds.NoAction
            PointerDownPos = None
            PendingDragId = None
            DraggingId = None
            DropTargetId = None
            DropTargetMode = None
            DragPos = None }, Cmd.none
    | DragStartInternal (id, info, pt) -> 
        { model with 
            SelectedNodeId = Some id
            PendingDragId = Some id
            SvgInfo = Some info
            PointerDownPos = Some pt
            DragPos = Some pt
            DropTargetId = None
            DropTargetMode = None }, Cmd.none
    | PointerMove data ->
        if data.Buttons = 0 then
            if model.DraggingId.IsSome || model.PendingDragId.IsSome then
                { model with 
                    DraggingId = None
                    PendingDragId = None
                    DropTargetId = None
                    DropTargetMode = None
                    DragPos = None
                    PointerDownPos = None }, Cmd.none
            else
                model, Cmd.none
        else
            let nowMs = DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalMilliseconds
            match model.LastMoveMs with
            | Some last when nowMs - last < 16.0 -> model, Cmd.none
            | _ ->
                match model.DraggingId, model.SvgInfo with
                | Some draggingId, Some info ->
                    let currentTree = getCurrentTree model
                    let pt = toSvgCoords info (float data.ClientX) (float data.ClientY)
                    let draggedNodeOpt = TreeOps.findNodeById draggingId currentTree
                    
                    let rec findTarget node =
                        let isSelfOrDescendant = 
                            match draggedNodeOpt with
                            | Some dn -> node.Id = dn.Id || TreeOps.isDescendant node.Id dn
                            | None -> node.Id = draggingId
                        if isSelfOrDescendant then None
                        else
                            let hit = 
                                pt.SvgX >= node.X - 35.0 && pt.SvgX <= node.X + 35.0 &&
                                pt.SvgY >= node.Y - 40.0 && pt.SvgY <= node.Y + 30.0
                            if hit then Some node
                            else node.Children |> List.tryPick findTarget
                    
                    let targetOpt = findTarget currentTree
                    let dropModeOpt = 
                        targetOpt |> Option.map (fun target ->
                            let isNestAnchor = model.NestAnchors |> Map.exists (fun _ anchorId -> anchorId = target.Id)
                            if target.Id = currentTree.Id then DropAsChild
                            elif pt.SvgY > target.Y + 5.0 && not isNestAnchor then DropAsChild
                            elif pt.SvgX < target.X then DropBefore
                            else DropAfter
                        )
                    
                    { model with 
                        DropTargetId = targetOpt |> Option.map (fun t -> t.Id)
                        DropTargetMode = dropModeOpt
                        DragPos = Some pt
                        LastMoveMs = Some nowMs }, Cmd.none

                | None, Some info -> 
                    match model.PendingDragId, model.PointerDownPos with
                    | Some pendingId, Some startPt ->
                        let pt = toSvgCoords info (float data.ClientX) (float data.ClientY)
                        let dist = sqrt ((pt.SvgX - startPt.SvgX)**2.0 + (pt.SvgY - startPt.SvgY)**2.0)
                        if dist > 16.0 then 
                            { model with 
                                DraggingId = Some pendingId
                                PendingDragId = None
                                DragPos = Some pt
                                LastMoveMs = Some nowMs }, Cmd.none
                        else
                            { model with LastMoveMs = Some nowMs }, Cmd.none
                    | _ -> { model with LastMoveMs = Some nowMs }, Cmd.none
                | _ -> { model with LastMoveMs = Some nowMs }, Cmd.none
    | PointerUp ->
        let m = { model with PendingDragId = None; PointerDownPos = None }
        let currentTree = getCurrentTree m
        match m.DraggingId, m.DropTargetId, m.DropTargetMode with
        | Some sourceId, Some targetId, Some mode when sourceId <> targetId ->
            let sourceNode = TreeOps.findNodeById sourceId currentTree
            match sourceNode with
            | Some sn when not (TreeOps.isDescendant targetId sn) ->
                let (rootWithoutSource, extracted) = TreeOps.extractNode sourceId currentTree
                match rootWithoutSource, extracted with
                | Some rs, Some ex ->
                    let isTargetNestAnchor = m.NestAnchors |> Map.exists (fun _ anchorId -> anchorId = targetId)
                    let reorderedTree = 
                        match mode with
                        | DropAsChild when not isTargetNestAnchor -> TreeOps.attachChild targetId ex rs
                        | DropAsChild -> rs
                        | DropBefore -> TreeOps.insertBefore targetId ex rs
                        | DropAfter -> TreeOps.insertAfter targetId ex rs
                    let laidOut = fst (TreeOps.layoutTree reorderedTree 0 50.0)
                    let updatedModel = updateCurrentTree m laidOut |> Coloring.colorModel
                    { updatedModel with 
                        SelectedNodeId = Some sourceId
                        DraggingId = None
                        DropTargetId = None
                        DropTargetMode = None
                        DragPos = None
                        SvgInfo = None }, Cmd.none
                | _ -> { m with DraggingId = None; DropTargetId = None; DropTargetMode = None; DragPos = None; SvgInfo = None }, Cmd.none
            | _ -> { m with DraggingId = None; DropTargetId = None; DropTargetMode = None; DragPos = None; SvgInfo = None }, Cmd.none
        | _ -> { m with DraggingId = None; DropTargetId = None; DropTargetMode = None; DragPos = None; SvgInfo = None }, Cmd.none
    | PointerUpInternal ->
        { model with DraggingId = None; PendingDragId = None; DropTargetId = None; DropTargetMode = None; DragPos = None; SvgInfo = None; PointerDownPos = None }, Cmd.none
    | _ -> model, Cmd.none

let updateSub (js: IJSRuntime) msg model =
    match msg with
    | SelectNode idOpt -> 
        { model with 
            SelectedNodeId = idOpt
            ActiveMenuId = None
            ConfirmingId = None
            ActiveActionId = ActionIds.NoAction
            PointerDownPos = None
            PendingDragId = None
            DraggingId = None
            DropTargetId = None
            DropTargetMode = None
            DragPos = None }, Cmd.none
    | OpenMenu id -> { model with ActiveMenuId = Some id; SelectedNodeId = Some id; ConfirmingId = None }, Cmd.none
    | CloseMenu -> { model with ActiveMenuId = None }, Cmd.none
    | SetLevel lvl -> { model with ActiveLevel = lvl; ActiveNest = None; ActiveMenuId = None; SelectedNodeId = None } |> Coloring.colorModel, Cmd.none
    | SetNest nId -> 
        let nestRootOpt = model.Nests |> Map.tryFind nId
        let parentLvl = 
            match model.NestAnchors |> Map.tryFind nId with
            | Some aId ->
                model.Levels 
                |> Map.tryPick (fun lvl root -> 
                    if TreeOps.findNodeById aId root |> Option.isSome then Some lvl else None)
                |> Option.defaultValue (match nestRootOpt with Some n -> n.Level | None -> model.ActiveLevel)
            | None ->
                match nestRootOpt with
                | Some nestNode -> nestNode.Level
                | None -> model.ActiveLevel
        let newModel = { model with ActiveLevel = parentLvl; ActiveNest = Some nId; ActiveMenuId = None; SelectedNodeId = None } |> Coloring.colorModel
        newModel, Cmd.none
    | SetTopExtrusion newVal ->
        let extr = match Double.TryParse newVal with true, v -> max 0.1 v | _ -> model.TopExtrusion
        { model with TopExtrusion = extr }, Cmd.none
    | PrepareAction (id, actionId) -> 
        match NodeActions.findAction actionId with
        | Some action ->
            let currentTree = getCurrentTree model
            match TreeOps.findNodeById id currentTree with
            | Some node when action.Logic.IsApplicable model node ->
                { model with ConfirmingId = Some id; ActiveActionId = actionId; ActiveMenuId = None; SelectedNodeId = Some id }, Cmd.none
            | _ -> model, Cmd.none
        | None -> model, Cmd.none
    | CancelAction -> { model with ConfirmingId = None; ActiveActionId = ActionIds.NoAction }, Cmd.none
    | ExecuteAction (id, actionId) -> handleExecuteAction id actionId model
    | AddChild parentId ->
        let isNestAnchor = model.NestAnchors |> Map.exists (fun _ anchorId -> anchorId = parentId)
        if isNestAnchor then
            model, Cmd.none
        else
            let currentTree = getCurrentTree model
            let newChild = { Id = Guid.NewGuid(); Name = TreeOps.getRandomName(); Weight = TreeOps.getRandomWeight(); X = 0.0; Y = 0.0; Children = []; Level = model.ActiveLevel; Extrusion = 3.0; Base = None; Color = None }
            let newRoot = TreeOps.updateNodeById parentId (fun n -> { n with Children = n.Children @ [newChild] }) currentTree
            let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
            let updated = updateCurrentTree model laidOut |> Coloring.colorModel
            { updated with SelectedNodeId = Some newChild.Id }, Cmd.none
    | UpdateName _ | UpdateWeight _ | UpdateExtrusion _ | ActionInput _ -> handleNodeUpdate msg model
    | PointerDown _ | PointerMove _ | PointerUp | DragStartInternal _ | NodePointerDown _ | PointerUpInternal -> handlePointerEvent msg js model

// --------------------
// View Delegation
// --------------------
let renderNode = NodeElement.renderNode


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
    let maxLevel = if model.Levels.IsEmpty then 0 else model.Levels.Keys |> Seq.max
    let containerClasses = 
        [ yield "tree-container"
          if model.DraggingId.IsSome then yield "is-dragging-any" ]
        |> String.concat " "

    let touchAction = if model.DraggingId.IsSome || model.PendingDragId.IsSome then "none" else "pan-x pan-y pinch-zoom"

    concat {
        // Level Nav
        div {
            attr.``class`` "level-controls-container"
            attr.style "width: fit-content; margin: 4px auto 8px auto; z-index: 1000;"
            span { attr.``class`` "level-label"; text "LEVELS:" }
            forEach (List.init (maxLevel + 1) id) (fun i ->
                let elv = if i < elevations.Length then elevations.[i] else 0.0
                let elvStr = if elv = floor elv then string (int elv) else string elv
                let levelTab =
                    button {
                        attr.``class`` (if model.ActiveLevel = i && model.ActiveNest.IsNone then "level-tab active" else "level-tab")
                        on.pointerdown (fun _ -> dispatch (SetLevel i))
                        text elvStr
                    }
                
                let nestTabs = 
                    model.Nests 
                    |> Map.toList 
                    |> List.filter (fun (_, tree) -> tree.Level = i)
                    |> List.map (fun (nId, _) ->
                        let isActive = model.ActiveNest = Some nId
                        let fw = if isActive then "bold" else "normal"
                        let colorStyle = if isActive then "color: #2ecc71;" else ""
                        button {
                            attr.``class`` (if isActive then "level-tab active" else "level-tab")
                            attr.style $"margin-left: 2px; font-weight: {fw}; {colorStyle}"
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
            let topElv = if elevations.Length > 0 then elevations.[elevations.Length - 1] else model.TopExtrusion
            let baseOfTop = if elevations.Length > 1 then elevations.[elevations.Length - 2] else 0.0
            
            div {
                attr.style "display: inline-flex; align-items: center; margin-left: 4px;"
                input {
                    attr.``type`` "text"
                    attr.``class`` "level-tab"
                    attr.style "width: 40px; text-align: center; outline: none; padding: 2px 0;"
                    attr.value (if topElv = floor topElv then string (int topElv) else string topElv)
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
            on.pointermove (fun ev -> dispatch (PointerMove { ClientX = float ev.ClientX; ClientY = float ev.ClientY; Buttons = int ev.Buttons }))
            on.pointerup (fun _ -> dispatch PointerUp)
            on.pointercancel (fun _ -> dispatch PointerUp)

            div {
                attr.id "tree-canvas-svg"
                attr.``class`` "tree-canvas"
                attr.style $"width:{canvasWidth}px; height:{max 150.0 canvasHeight}px; touch-action:{touchAction};"
                on.pointerdown (fun ev -> dispatch (PointerDown { ClientX = float ev.ClientX; ClientY = float ev.ClientY; Buttons = int ev.Buttons }))
                
                svg {
                    attr.``class`` "tree-svg"
                    attr.style $"width:{canvasWidth}px; height:{canvasHeight}px;"
                    forEach lines (fun line -> line)
                    match model.DraggingId, model.DropTargetId, model.DragPos with
                    | Some _, Some targetId, Some pt ->
                        match TreeOps.findNodeById targetId currentLvlRoot with
                        | Some targetNode ->
                            let strokeColor = match model.DropTargetMode with Some DropAsChild -> "#0d9488" | _ -> "#6366f1"
                            svLn().x1($"{targetNode.X}").y1($"{targetNode.Y + 5.0}").x2($"{pt.SvgX}").y2($"{pt.SvgY}").color(strokeColor).width("1.5").Elt()
                        | None -> empty()
                    | _ -> empty()
                }
                renderAll laidOutDisplayTree "1" colorList nodes

                match model.DraggingId, model.DragPos with
                | Some dragId, Some pt ->
                    match TreeOps.findNodeById dragId currentLvlRoot with
                    | Some draggedNode -> NodeElement.renderDragGhost draggedNode pt
                    | None -> empty()
                | _ -> empty()
            }
        }

        // Selected Node Properties Bar (positioned below the tree)
        NodeElement.renderPropertiesBar model dispatch
    }
