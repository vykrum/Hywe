/// <summary>
/// Fresh-launch interactive tutorial. Scripted sequence of real SubModel snapshots
/// showing: node selection â†’ all badges visible â†’ specific badge highlighted â†’ action result.
/// Forward/back navigation swaps model.Tree to the corresponding snapshot.
/// </summary>
module Tutorial

open System
open Bolero
open Bolero.Html
open ModelTypes
open Hywe.Core

// ─────────────────────────────────────────────
// Step definitions
// ─────────────────────────────────────────────

/// Which badge/zone to ring-highlight on this step (drives CSS class on wrapper).
type BadgeTarget =
    | NoBadge
    | BadgeMove
    | BadgeAdd
    | BadgeDelete
    | BadgeElevate
    | BadgeNest
    | PropsBar
    | LevelNav
    | EditName
    | EditWeight

type TutorialStepDef = {
    Title      : string
    Body       : string
    Annotation : string      // Short action cue shown as a chip; empty = no chip
    Badge      : BadgeTarget
}

let private def t b a badge = { Title = t; Body = b; Annotation = a; Badge = badge }

let stepDefs = [|
    def "Welcome to HYWE"
        "Start with a single root space on the spatial graph canvas."
        "" NoBadge

    def "Focus Label Input on Root"
        "Select the root node space to activate its inline label input field."
        "Focus label input" EditName

    def "Rename to Root"
        "Type <Root> into the highlighted label input box."
        "Rename node to <Root>" EditName

    def "Add 1st Child Node"
        "Tap the + badge at the bottom of <Root> to create the first child space."
        "Tap + badge on <Root>" BadgeAdd

    def "Add 2nd Child Node"
        "Tap the + badge on <Root> a second time to create the second child space."
        "Tap + badge on <Root>" BadgeAdd

    def "Add 3rd Child Node"
        "Tap the + badge on <Root> a third time to create the third child space."
        "Tap + badge on <Root>" BadgeAdd

    def "Focus Label on 1st Child"
        "Select <Space 1> to highlight its label input box for editing."
        "Focus label input on <Space 1>" EditName

    def "Rename 1st Child"
        "Type <Child 1> to update the label of the first space."
        "Rename to <Child 1>" EditName

    def "Focus Label on 2nd Child"
        "Select <Space 2> to highlight its label input box for editing."
        "Focus label input on <Space 2>" EditName

    def "Rename 2nd Child"
        "Type <Child 2> to update the label of the second space."
        "Rename to <Child 2>" EditName

    def "Focus Label on 3rd Child"
        "Select <Space 3> to highlight its label input box for editing."
        "Focus label input on <Space 3>" EditName

    def "Rename 3rd Child"
        "Type <Child 3> to update the label of the third space."
        "Rename to <Child 3>" EditName

    def "Select Move Badge"
        "Select <Child 3> and tap the Move badge to initiate spatial re-ordering."
        "Tap Move badge on <Child 3>" BadgeMove

    def "Drag Child 3 to Left of Child 2"
        "Dragging <Child 3> to the left edge of <Child 2> displays the ghost node and left drop line indicator (DropBefore)."
        "Dragging to left of <Child 2>" BadgeMove

    def "Child 3 Moved to Left"
        "<Child 3> drops to the left of <Child 2>, placing siblings in order: <Child 1>, <Child 3>, <Child 2>."
        "<Child 3> moved to left" NoBadge

    def "Drag Child 3 over Child 1"
        "Dragging <Child 3> onto <Child 1> displays the drag ghost node and '↳ child' drop indicator (DropAsChild)."
        "Dragging over <Child 1>" BadgeMove

    def "Child 3 Re-parented under Child 1"
        "<Child 3> is now a child space beneath <Child 1> in the spatial hierarchy."
        "<Child 3> moved under <Child 1>" NoBadge

    def "Select Elevate Badge on Child 2"
        "Select <Child 2> and tap the Elevate badge to promote it to a new storey level."
        "Tap Elevate badge on <Child 2>" BadgeElevate

    def "Elevate Confirmation UI"
        "The ELEVATE confirmation panel opens inside <Child 2>, allowing extrusion height configuration."
        "ELEVATE confirmation panel" NoBadge

    def "Confirm Elevate Action"
        "Set extrusion height and confirm ELEVATE to create Storey Level 1 (L1)."
        "Confirm ELEVATE" NoBadge

    def "Focus Level Navigator"
        "Notice Storey Level 1 (L1) is created and active in the top Level Navigator."
        "Focus Level Navigator" LevelNav

    def "Return to Level 0"
        "Tap the L0 tab in the Level Navigator to return to Level 0 main view."
        "Select L0 tab" LevelNav

    def "Select Nest Badge on Child 1"
        "Select node and tap the Nest badge to create an internal sub-tree program."
        "Tap Nest badge on node" BadgeNest

    def "Nest Confirmation UI"
        "The NEST confirmation panel opens inside node, specifying sub-tree program N#."
        "NEST confirmation panel" NoBadge

    def "Confirm Nest Action"
        "Tap NEST inside the node to generate nested sub-tree N# inside node."
        "Confirm NEST" NoBadge

    def "Focus Nests Breadcrumb"
        "Notice nested program N# appears alongside corresponding Level in the breadcrumbs bar."
        "Focus Nests Breadcrumb" LevelNav

    def "Return to Level 0"
        "Tap the in the breadcrumbs bar to return to desired level."
        "Navigate using Levels Navigator" LevelNav

    def "Select Delete Badge on Child 3"
        "Select node and tap the Delete badge at bottom-left."
        "Tap Delete badge on node" BadgeDelete

    def "Delete Confirmation UI"
        "The DELETE confirmation panel opens inside the node, showing the red confirmation button."
        "DELETE confirmation panel" NoBadge

    def "Confirm Delete Action"
        "Tap DELETE inside node to confirm its removal from the tree structure."
        "Confirm DELETE" NoBadge

    def "Focus Area Weight Input"
        "Select node to highlight its inline area weight input field."
        "Focus weight input on node" EditWeight

    def "Alter Area Weight Inline"
        "Edit the area weight input inline to change its target floor area."
        "Alter Area Weight to 150" EditWeight

    def "Focus Properties Bar"
        "The Properties Bar opens at the bottom of the view when a node is selected."
        "Focus Properties Bar" PropsBar

    def "Edit via Properties Bar"
        "Alternately, use the bottom Properties Bar to alter node name and weight."
        "Edit via Properties Bar" PropsBar

    def "Click Hyweave to Execute"
        "Click HYWEAVE to generate spatial layout!"
        "Click HYWEAVE to execute" NoBadge
|]

/// CSS class applied to the tree wrapper to highlight a specific badge.
let badgeCssClass = function
    | NoBadge     -> ""
    | BadgeMove   -> "tutorial-hl-badge-move"
    | BadgeAdd    -> "tutorial-hl-badge-add"
    | BadgeDelete -> "tutorial-hl-badge-delete"
    | BadgeElevate-> "tutorial-hl-badge-elevate"
    | BadgeNest   -> "tutorial-hl-badge-nest"
    | PropsBar    -> "tutorial-hl-props-bar"
    | LevelNav    -> "tutorial-hl-level-nav"
    | EditName    -> "tutorial-hl-edit-name"
    | EditWeight  -> "tutorial-hl-edit-weight"

// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
// Snapshot builder â€” one real SubModel per step
// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

let private initialSingleNodeSyntax = "L0(Q=VRCCNE/L=0/X=1/B=0/E=0)(1/100/Space 1)"

let private makeSingleNodeModel () : SubModel =
    Serialization.initModel initialSingleNodeSyntax

let private rootOf (sm: SubModel) = sm.Levels.[sm.ActiveLevel]
let private sel (sm: SubModel) idOpt = { sm with SelectedNodeId = idOpt }

let private addNamedChild (parentId: Guid) (name: string) (weight: string) (sm: SubModel) : SubModel =
    let r = rootOf sm
    let child = {
        Id = Guid.NewGuid(); Name = name; Weight = weight
        X = 0.0; Y = 0.0; Children = []; Level = sm.ActiveLevel
        Extrusion = 3.0; Base = None; Color = None }
    let newRoot = TreeOps.updateNodeById parentId (fun n -> { n with Children = n.Children @ [child] }) r
    let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
    { sm with Levels = sm.Levels |> Map.add sm.ActiveLevel laidOut } |> Coloring.colorModel

let private buildSnapshots () : SubModel[] =
    // Step 0 – welcome (single initial node, nothing selected)
    let s0 = makeSingleNodeModel ()
    let rootNode0 = rootOf s0
    let rootId = rootNode0.Id

    // Step 1a – Focus label input on root space
    let s1a = sel s0 (Some rootId)

    // Step 1b – Rename single node to <Root>
    let s1base =
        let r = rootOf s0
        let newRoot = TreeOps.updateNodeById rootId (fun n -> { n with Name = "<Root>" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s0 with Levels = s0.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s1b = sel s1base (Some rootId)

    // Step 2a – Add 1st child with default name <Space 1> under <Root>
    let s2a_base = addNamedChild rootId "<Space 1>" "30" s1base
    let rootNode2a = rootOf s2a_base
    let sp1Node = rootNode2a.Children |> List.find (fun n -> n.Name.Contains("Space 1"))
    let sp1Id = sp1Node.Id
    let s2a = sel s2a_base (Some rootId)

    // Step 2b – Add 2nd child with default name <Space 2> under <Root>
    let s2b_base = addNamedChild rootId "<Space 2>" "30" s2a_base
    let rootNode2b = rootOf s2b_base
    let sp2Node = rootNode2b.Children |> List.find (fun n -> n.Name.Contains("Space 2"))
    let sp2Id = sp2Node.Id
    let s2b = sel s2b_base (Some rootId)

    // Step 2c – Add 3rd child with default name <Space 3> under <Root>
    let s2c_base = addNamedChild rootId "<Space 3>" "30" s2b_base
    let rootNode2c = rootOf s2c_base
    let sp3Node = rootNode2c.Children |> List.find (fun n -> n.Name.Contains("Space 3"))
    let sp3Id = sp3Node.Id
    let s2c = sel s2c_base (Some sp3Id)

    // Step 2d1 – Focus label input on 1st child
    let s2d1 = sel s2c_base (Some sp1Id)

    // Step 2d2 – Rename 1st child <Space 1> to <Child 1>
    let s2d2_base =
        let r = rootOf s2c_base
        let newRoot = TreeOps.updateNodeById sp1Id (fun n -> { n with Name = "<Child 1>" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s2c_base with Levels = s2c_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s2d2 = sel s2d2_base (Some sp1Id)

    // Step 2e1 – Focus label input on 2nd child
    let s2e1 = sel s2d2_base (Some sp2Id)

    // Step 2e2 – Rename 2nd child <Space 2> to <Child 2>
    let s2e2_base =
        let r = rootOf s2d2_base
        let newRoot = TreeOps.updateNodeById sp2Id (fun n -> { n with Name = "<Child 2>" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s2d2_base with Levels = s2d2_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s2e2 = sel s2e2_base (Some sp2Id)

    // Step 2f1 – Focus label input on 3rd child
    let s2f1 = sel s2e2_base (Some sp3Id)

    // Step 2f2 – Rename 3rd child <Space 3> to <Child 3>
    let s2f2_base =
        let r = rootOf s2e2_base
        let newRoot = TreeOps.updateNodeById sp3Id (fun n -> { n with Name = "<Child 3>" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s2e2_base with Levels = s2e2_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s2f2 = sel s2f2_base (Some sp3Id)

    // Step 3 – Select move badge on <Child 3>
    let s3 = s2f2

    // Step 4a – Drag <Child 3> to left of <Child 2> (Ghost node & drop line visible)
    let child2Node = TreeOps.findNodeById sp2Id (rootOf s2f2_base) |> Option.get
    let dragPtLeft = { SvgX = child2Node.X - 35.0; SvgY = child2Node.Y }
    let s4a = { s3 with DraggingId = Some sp3Id; DragPos = Some dragPtLeft; DropTargetId = Some sp2Id; DropTargetMode = Some DropBefore }

    // Step 4b – Move <Child 3> to left of <Child 2> executed
    let s4bbase =
        let (rootWithoutSource, extracted) = TreeOps.extractNode sp3Id (rootOf s2f2_base)
        match rootWithoutSource, extracted with
        | Some rs, Some ex ->
            let reordered = TreeOps.insertBefore sp2Id ex rs
            let laidOut = fst (TreeOps.layoutTree reordered 0 50.0)
            { s2f2_base with Levels = s2f2_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
        | _ -> s2f2_base
    let s4b = sel s4bbase (Some sp3Id)

    // Step 5a – Drag <Child 3> over <Child 1> (Ghost node & child drop badge visible)
    let rootNodeS4b = rootOf s4bbase
    let child1NodeS4b = TreeOps.findNodeById sp1Id rootNodeS4b |> Option.get
    let dragPtChild = { SvgX = child1NodeS4b.X; SvgY = child1NodeS4b.Y + 45.0 }
    let s5a = { s4b with DraggingId = Some sp3Id; DragPos = Some dragPtChild; DropTargetId = Some sp1Id; DropTargetMode = Some DropAsChild }

    // Step 5b – Move <Child 3> under <Child 1> executed
    let s5bbase =
        let (rootWithoutSource, extracted) = TreeOps.extractNode sp3Id (rootOf s4bbase)
        match rootWithoutSource, extracted with
        | Some rs, Some ex ->
            let reordered = TreeOps.attachChild sp1Id ex rs
            let laidOut = fst (TreeOps.layoutTree reordered 0 50.0)
            { s4bbase with Levels = s4bbase.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
        | _ -> s4bbase
    let s5b = sel s5bbase (Some sp3Id)

    // Step 6a – Select Elevate badge on <Child 2>
    let s6a = sel s5bbase (Some sp2Id)

    // Step 6b – Elevate confirmation node on <Child 2>
    let s6b = { s6a with ConfirmingId = Some sp2Id; ActiveActionId = ActionIds.Elevate; SelectedNodeId = Some sp2Id }

    // Step 7 – Change Elevate value and confirm Elevate on <Child 2> -> L1 created
    let child2NodeS5b = TreeOps.findNodeById sp2Id (rootOf s5bbase)
    let s7 =
        match child2NodeS5b with
        | Some n -> fst (Actions.elevateActionLogic.Execute s5bbase n)
        | None   -> s5bbase

    // Step 8a – Focus Level Navigator on L1
    let s8a = sel s7 None

    // Step 8b – Navigate back to L0 in Level navigator
    let s8b = { s7 with ActiveLevel = 0; ActiveNest = None; SelectedNodeId = Some sp1Id } |> Coloring.colorModel

    // Step 9a – Select Nest badge on <Child 1>
    let s9a = sel s8b (Some sp1Id)

    // Step 9b – Nest confirmation node on <Child 1>
    let s9b = { s9a with ConfirmingId = Some sp1Id; ActiveActionId = ActionIds.Nest; SelectedNodeId = Some sp1Id }

    // Step 10 – Confirm Nest on <Child 1> -> N1 created
    let child1NodeS8 = TreeOps.findNodeById sp1Id (rootOf s8b)
    let s10 =
        match child1NodeS8 with
        | Some n -> fst (Actions.nestActionLogic.Execute s8b n)
        | None   -> s8b

    // Step 11a – Focus Nests Breadcrumb on N1
    let s11a = sel s10 None

    // Step 11b – Navigate back to L0 in Level navigator
    let s11b = { s10 with ActiveLevel = 0; ActiveNest = None; SelectedNodeId = Some sp3Id } |> Coloring.colorModel

    // Step 12a – Select Delete badge on <Child 3>
    let s12a = sel s11b (Some sp3Id)

    // Step 12b – Delete confirmation node on <Child 3>
    let s12b = { s12a with ConfirmingId = Some sp3Id; ActiveActionId = ActionIds.Delete; SelectedNodeId = Some sp3Id }

    // Step 13 – Confirm Delete on <Child 3>
    let s13base =
        match TreeOps.removeNodeById sp3Id (rootOf s11b) with
        | Some newRoot ->
            let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
            { s11b with Levels = s11b.Levels |> Map.add 0 laidOut; ConfirmingId = None; ActiveActionId = ActionIds.NoAction }
            |> Coloring.colorModel
        | None -> s11b
    let s13 = sel s13base (Some rootId)

    // Step 14a – Focus area weight input on <Root> node
    let s14a = sel s13base (Some rootId)

    // Step 14b – Alter area weight in <Root> node (weight = "150")
    let s14bbase =
        let r = rootOf s13base
        let newRoot = TreeOps.updateNodeById rootId (fun n -> { n with Weight = "150" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s13base with Levels = s13base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s14b = sel s14bbase (Some rootId)

    // Step 15a – Focus Properties Bar on <Root> node
    let s15a = sel s14bbase (Some rootId)

    // Step 15b – Alter Area weight and name of <Root> in Properties bar (<Main Hub>, weight = "200")
    let s15bbase =
        let r = rootOf s14bbase
        let newRoot = TreeOps.updateNodeById rootId (fun n -> { n with Name = "<Main Hub>"; Weight = "200" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s14bbase with Levels = s14bbase.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s15b = sel s15bbase (Some rootId)

    // Step 16 – Click Hyweave to execute (clean tree state)
    let s16 = sel s15bbase None

    [| s0; s1a; s1b; s2a; s2b; s2c; s2d1; s2d2; s2e1; s2e2; s2f1; s2f2; s3; s4a; s4b; s5a; s5b; s6a; s6b; s7; s8a; s8b; s9a; s9b; s10; s11a; s11b; s12a; s12b; s13; s14a; s14b; s15a; s15b; s16 |]

let snapshots : SubModel[] = buildSnapshots ()

let totalSteps = min stepDefs.Length snapshots.Length

// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
// Badge visual helpers for the annotation chip
// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

let private badgeIcon = function
    | BadgeMove   -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="5 9 2 12 5 15"/><polyline points="9 5 12 2 15 5"/><polyline points="15 19 12 22 9 19"/><polyline points="19 9 22 12 19 15"/><line x1="2" y1="12" x2="22" y2="12"/><line x1="12" y1="2" x2="12" y2="22"/></svg>"""
    | BadgeAdd    -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.8" stroke-linecap="round"><line x1="12" y1="5" x2="12" y2="19"/><line x1="5" y1="12" x2="19" y2="12"/></svg>"""
    | BadgeDelete -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"/></svg>"""
    | BadgeElevate-> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>"""
    | BadgeNest   -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2"/><rect x="8" y="8" width="8" height="8" rx="1"/></svg>"""
    | LevelNav    -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><rect x="2" y="7" width="20" height="4" rx="1"/><rect x="2" y="13" width="20" height="4" rx="1"/></svg>"""
    | PropsBar    -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><line x1="3" y1="6" x2="21" y2="6"/><line x1="3" y1="12" x2="15" y2="12"/><line x1="3" y1="18" x2="18" y2="18"/></svg>"""
    | EditName    -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"/><path d="M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"/></svg>"""
    | EditWeight  -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 7V4h16v3"/><path d="M9 20h6"/><path d="M12 4v16"/></svg>"""
    | NoBadge     -> ""

let private badgeColor = function
    | BadgeMove   -> "#e67e22"
    | BadgeAdd    -> "#4ade80"
    | BadgeDelete -> "#f87171"
    | BadgeElevate-> "#60a5fa"
    | BadgeNest   -> "#34d399"
    | LevelNav    -> "#a78bfa"
    | PropsBar    -> "#fbbf24"
    | EditName    -> "#06b6d4"
    | EditWeight  -> "#06b6d4"
    | NoBadge     -> "transparent"

// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
// View
// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

let viewTutorialBanner (tutorialStep: int) (autoPlay: bool) (dispatch: Message -> unit) : Node =
    let step    = min tutorialStep (totalSteps - 1)
    let def     = stepDefs.[step]
    let isFirst = step = 0
    let isLast  = step = totalSteps - 1

    div {
        attr.``class`` (if autoPlay then "tutorial-banner is-autoplay" else "tutorial-banner")

        // Top row: dots + step counter
        div {
            attr.``class`` "tutorial-top-row"
            div {
                attr.``class`` "tutorial-dots"
                for i in 0 .. totalSteps - 1 do
                    span { attr.``class`` (if i = step then "tutorial-dot active" else "tutorial-dot") }
            }
            div {
                attr.``class`` "tutorial-step-info"
                span { attr.``class`` "tutorial-step-label"; text $"Step {step + 1} of {totalSteps}" }
            }
        }

        // Content
        div {
            attr.``class`` "tutorial-content"
            h3 { attr.``class`` "tutorial-title"; text def.Title }
            p  { attr.``class`` "tutorial-body";  text def.Body  }

            // Annotation chip (badge icon + action cue text)
            if def.Annotation <> "" then
                div {
                    attr.``class`` "tutorial-annotation"
                    if def.Badge <> NoBadge then
                        span {
                            attr.``class`` "tutorial-badge-chip"
                            attr.style $"background:{badgeColor def.Badge}18; border-color:{badgeColor def.Badge}44; color:{badgeColor def.Badge};"
                            rawHtml (badgeIcon def.Badge)
                        }
                    span {
                        attr.``class`` "tutorial-annotation-text"
                        text def.Annotation
                    }
                }
        }

        // Auto-play progress bar
        if autoPlay then
            div {
                attr.``class`` "tutorial-progress-bar"
                div { attr.``class`` "tutorial-progress-fill" }
            }

        // Navigation
        div {
            attr.``class`` "tutorial-nav"
            div {
                attr.``class`` "tutorial-left-actions"
                button {
                    attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-ghost tutorial-skip"
                    on.click (fun _ -> dispatch DismissTutorial)
                    text "Skip"
                }
                button {
                    attr.``class`` (if autoPlay then "hywe-btn hywe-btn-sm hywe-btn-ghost tutorial-autoplay active" else "hywe-btn hywe-btn-sm hywe-btn-ghost tutorial-autoplay")
                    attr.title (if autoPlay then "Pause auto advance" else "Auto advance steps")
                    on.click (fun _ -> dispatch ToggleTutorialAutoPlay)
                    if autoPlay then
                        rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor"><rect x="6" y="4" width="4" height="16" rx="1"/><rect x="14" y="4" width="4" height="16" rx="1"/></svg><span> Pause</span>"""
                    else
                        rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor"><polygon points="5 3 19 12 5 21 5 3"/></svg><span> Auto</span>"""
                }
            }
            div {
                attr.``class`` "tutorial-nav-arrows"
                button {
                    attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-dark tutorial-back"
                    attr.disabled isFirst
                    on.click (fun _ -> dispatch TutorialBack)
                    rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="15 18 9 12 15 6"></polyline></svg>"""
                    text " Back"
                }
                button {
                    attr.``class`` (
                        if isLast then "hywe-btn hywe-btn-sm hywe-btn-primary tutorial-next"
                        else           "hywe-btn hywe-btn-sm hywe-btn-dark tutorial-next")
                    on.click (fun _ ->
                        if isLast then dispatch DismissTutorial
                        else           dispatch TutorialNext)
                    if isLast then text "Start designing "
                    else           text "Next "
                    rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="9 18 15 12 9 6"></polyline></svg>"""
                }
            }
        }
    }

