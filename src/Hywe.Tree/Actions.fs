namespace Hywe.Node

open System
open Elmish

module Actions =

    let deleteActionLogic = {
        LogicId = ActionIds.Delete
        LogicLabel = "Delete"
        IsApplicable = fun model node ->
            let currentTree = 
                match model.ActiveNest with
                | Some nId -> model.Nests |> Map.tryFind nId |> Option.defaultValue (model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0])
                | None -> model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
                
            let isRoot = node.Id = currentTree.Id
            match isRoot with
            | true -> 
                match model.ActiveNest with
                | Some _ -> true
                | None -> model.ActiveLevel > 0 && model.Levels.Keys |> Seq.forall (fun k -> k <= model.ActiveLevel)
            | false -> 
                match model.ActiveNest.IsSome with
                | true -> true
                | false ->
                    match node.Level > model.ActiveLevel with
                    | true -> not node.Children.IsEmpty
                    | false -> 
                        let elevatedAnchorIds = 
                            model.LevelAnchors 
                            |> Map.filter (fun k _ -> k > model.ActiveLevel) 
                            |> Map.toSeq 
                            |> Seq.map snd 
                            |> Set.ofSeq
                            
                        let nestAnchorIds =
                            model.NestAnchors
                            |> Map.toSeq
                            |> Seq.map snd
                            |> Set.ofSeq
                            
                        let nodeHasElevatedDescendant =
                            elevatedAnchorIds |> Set.contains node.Id ||
                            elevatedAnchorIds |> Seq.exists (fun anchorId -> TreeOps.isDescendant anchorId node)
                            
                        let nodeHasNestDescendant =
                            nestAnchorIds |> Set.contains node.Id ||
                            nestAnchorIds |> Seq.exists (fun anchorId -> TreeOps.isDescendant anchorId node)
                            
                        not nodeHasElevatedDescendant && not nodeHasNestDescendant
        IsDisabled = fun _ _ -> false
        Execute = fun model node ->
            let currentTree = 
                match model.ActiveNest with
                | Some nId -> model.Nests |> Map.tryFind nId |> Option.defaultValue (model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0])
                | None -> model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
                
            let updateModelWithNewRoot newRoot =
                let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
                match model.ActiveNest with
                | Some nId ->
                    let newNests = model.Nests |> Map.add nId laidOut
                    { model with Nests = newNests; ConfirmingId = None; ActiveActionId = ActionIds.NoAction; ActiveMenuId = None }
                | None ->
                    let newLevels = model.Levels |> Map.add model.ActiveLevel laidOut
                    { model with Levels = newLevels; ConfirmingId = None; ActiveActionId = ActionIds.NoAction; ActiveMenuId = None }
                    
            match node.Id = currentTree.Id with
            | true ->
                match model.ActiveNest with
                | Some nId ->
                    let newNests = model.Nests |> Map.remove nId
                    let newNestAnchors = model.NestAnchors |> Map.remove nId
                    { model with 
                        Nests = newNests
                        NestAnchors = newNestAnchors
                        ActiveNest = None
                        ConfirmingId = None
                        ActiveActionId = ActionIds.NoAction
                        ActiveMenuId = None }, Cmd.none
                | None ->
                    match model.ActiveLevel = 0 with
                    | true -> model, Cmd.none
                    | false ->
                        let hasElevatedDescendants = model.Levels.Keys |> Seq.exists (fun k -> k > model.ActiveLevel)
                        match hasElevatedDescendants with
                        | true -> model, Cmd.none
                        | false ->
                            let parentLvl = model.ActiveLevel - 1
                            let anchorId = model.LevelAnchors |> Map.tryFind model.ActiveLevel
                            match anchorId, model.Levels |> Map.tryFind parentLvl with
                            | Some aId, Some pTree ->
                                let updatedParentTree = TreeOps.updateNodeById aId (fun n -> { n with Level = parentLvl }) pTree
                                let laidOutParent = fst (TreeOps.layoutTree updatedParentTree 0 50.0)
                                let newLevels = 
                                    model.Levels 
                                    |> Map.remove model.ActiveLevel 
                                    |> Map.add parentLvl laidOutParent
                                let newAnchors = model.LevelAnchors |> Map.remove model.ActiveLevel
                                { model with 
                                    Levels = newLevels
                                    LevelAnchors = newAnchors
                                    ActiveLevel = parentLvl
                                    ConfirmingId = None
                                    ActiveActionId = ActionIds.NoAction
                                    ActiveMenuId = None }, Cmd.none
                            | _ -> model, Cmd.none
            | false ->
                match node.Level > model.ActiveLevel && model.ActiveNest.IsNone with
                | true ->
                    let newRoot = TreeOps.updateNodeById node.Id (fun n -> { n with Children = [] }) currentTree
                    updateModelWithNewRoot newRoot, Cmd.none
                | false ->
                    match TreeOps.removeNodeById node.Id currentTree with
                    | Some newRoot -> 
                        updateModelWithNewRoot newRoot, Cmd.none
                    | None -> model, Cmd.none
        HandleInput = None
    }

    let elevateActionLogic = {
        LogicId = ActionIds.Elevate
        LogicLabel = "Elevate"
        IsApplicable = fun model node -> 
            let isNestAnchor = model.NestAnchors |> Map.exists (fun _ anchorId -> anchorId = node.Id)
            node.Level >= model.ActiveLevel && not isNestAnchor
        IsDisabled = fun _ _ -> false
        Execute = fun model node ->
            let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
            let nextLvlForNode = model.ActiveLevel + 1
            let newAnchors = model.LevelAnchors |> Map.add nextLvlForNode node.Id
            let freshRoot = 
                match model.Levels |> Map.tryFind nextLvlForNode with
                | Some existingRoot ->
                    fst (TreeOps.layoutTree { existingRoot with Id = node.Id; Name = node.Name; Weight = node.Weight } 0 50.0)
                | None ->
                    { node with Level = nextLvlForNode; Children = []; X = 50.0; Y = 50.0; Extrusion = 3.0 }
            
            let treeWithResets = TreeOps.resetElevatedNodes model.ActiveLevel currentTree
            let updatedCurrentTree = TreeOps.updateNodeById node.Id (fun n -> { n with Level = nextLvlForNode }) treeWithResets
            let finalLevels = 
                model.Levels 
                |> Map.add nextLvlForNode freshRoot
                |> Map.add model.ActiveLevel updatedCurrentTree
            
            { model with Levels = finalLevels; LevelAnchors = newAnchors; ConfirmingId = None; ActiveActionId = ActionIds.NoAction; ActiveMenuId = None }, Cmd.none
        HandleInput = Some (fun model node newVal ->
            let extrusion = match Double.TryParse newVal with true, v -> max 0.1 v | _ -> node.Extrusion
            let currentTree = model.Levels |> Map.tryFind model.ActiveLevel |> Option.defaultValue model.Levels.[0]
            let updatedRoot = TreeOps.updateNodeById node.Id (fun n -> { n with Extrusion = extrusion }) currentTree
            let finalLevels = TreeOps.syncHierarchy (model.Levels |> Map.add model.ActiveLevel updatedRoot) model.LevelAnchors 0
            { model with Levels = finalLevels }, Cmd.none
        )
    }

    let nestActionLogic = {
        LogicId = ActionIds.Nest
        LogicLabel = "Nest"
        IsApplicable = fun model node -> 
            let isNestAnchor = model.NestAnchors |> Map.exists (fun _ anchorId -> anchorId = node.Id)
            node.Children.IsEmpty && not isNestAnchor
        IsDisabled = fun model node -> node.Level > model.ActiveLevel
        Execute = fun model node ->
            let newNestId = match model.Nests.IsEmpty with true -> 1 | false -> (model.Nests.Keys |> Seq.max) + 1
            let newNestRoot = { Id = Guid.NewGuid(); Name = "<nest>"; Weight = "100"; X = 0.0; Y = 0.0; Children = []; Level = model.ActiveLevel; Extrusion = 3.0; Base = None }
            let laidOut = fst (TreeOps.layoutTree newNestRoot 0 50.0)
            let newNests = model.Nests |> Map.add newNestId laidOut
            let newNestAnchors = model.NestAnchors |> Map.add newNestId node.Id
            { model with 
                Nests = newNests
                NestAnchors = newNestAnchors
                ActiveNest = Some newNestId
                ConfirmingId = None
                ActiveActionId = ActionIds.NoAction
                ActiveMenuId = None }, Cmd.none
        HandleInput = None
    }

    let logicRegistry = [
        deleteActionLogic
        elevateActionLogic
        nestActionLogic
    ]

    let findLogic id = logicRegistry |> List.tryFind (fun a -> a.LogicId = id)
