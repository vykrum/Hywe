/// <summary>
/// Onboarding tutorial engine with multi-level modules:
/// Basic Quickstart, Node Operations, and Levels & Nesting.
/// </summary>
module Tutorial

open System
open Bolero
open Bolero.Html
open ModelTypes
open Hywe.Core

// ─────────────────────────────────────────────
// Level definitions
// ─────────────────────────────────────────────

module TutorialLevel =
    let name = function
        | Basic     -> "Basic Quickstart"
        | Hierarchy -> "Hierarchy & Flow"
        | Levels    -> "Multi-Storey Levels"
        | Nests     -> "Program Nesting"

    let shortName = function
        | Basic     -> "Basic"
        | Hierarchy -> "Hierarchy"
        | Levels    -> "Levels"
        | Nests     -> "Nests"

    let nextLevel = function
        | Basic     -> Some Hierarchy
        | Hierarchy -> Some Levels
        | Levels    -> Some Nests
        | Nests     -> None

    let allLevels = [ Basic; Hierarchy; Levels; Nests ]

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
    | HyweaveBtn
    | TabLayoutPanel
    | TabViewPanel
    | TabBatchPanel
    | BadgeSlider

type TutorialStepDef = {
    Title       : string
    Body        : string
    Annotation  : string      // Short action cue shown as a chip; empty = no chip
    Badge       : BadgeTarget
    TargetPanel : ActivePanel option
}

let private def t b a badge panel = { Title = t; Body = b; Annotation = a; Badge = badge; TargetPanel = panel }

// ─── Level 1: Basic Quickstart (5 Steps) ───
let basicStepDefs = [|
    def "Welcome to HYWE"
        "Begin by charting spatial intent. Extend your relational hierarchy by adding child space nodes."
        "Add Child node" BadgeAdd None

    def "Tweak labels and area weights"
        "Alter node label or area weight inline to sculpt spatial intent."
        "Alter labels and area weight inline" EditWeight None

    def "Compile the lattice"
        "Run the engine to synthesize these relational demands into discrete spatial configurations."
        "Click hyWEAVE to compile lattice" HyweaveBtn (Some LayoutPanel)

    def "Explore alternate configurations"
        "This isn't random diffusion but mathematically deterministic configurations. Cycle through the variants."
        "Scrub slider to explore alternate configurations" BadgeSlider (Some LayoutPanel)

    def "Inspect all configurations"
        "Inspect all 24 resolved deterministic configurations at a glance in the Batch view."
        "Inspect all configurations" TabBatchPanel (Some BatchPanel)
|]

// ─── Level 2: Hierarchy & Flow (4 Steps) ───
let hierarchyStepDefs = [|
    def "Initiate Node Relocation"
        "Select a space node and tap the Move badge to initiate spatial re-ordering or structural reparenting within the graph."
        "Tap Move badge on space" BadgeMove None

    def "Re-order Sibling Circulation"
        "Drag a space to the boundary edge of a sibling node to adjust the spatial sequence and adjacency flow."
        "Drag to re-order siblings" BadgeMove None

    def "Re-parent Child Spaces"
        "Drag a space directly onto another space node to re-parent it as a child within a nested functional zone."
        "Drag onto node to nest as child" BadgeMove None

    def "Balance Target Area Allocations"
        "Adjust inline weight inputs on nodes to calibrate relative floor area allocations across spatial zones."
        "Alter area weight inline" EditWeight None
|]

// ─── Level 3: Multi-Storey Levels (4 Steps) ───
let levelsStepDefs = [|
    def "Initiate Vertical Elevation"
        "Select a space node and tap the Elevate badge to promote it to a new elevated floor level."
        "Tap Elevate badge" BadgeElevate None

    def "Confirm Level 1 Creation"
        "Set extrusion parameters and confirm ELEVATE to instantiate Level 1 (L1) in the spatial model."
        "Confirm ELEVATE action" NoBadge None

    def "Inspect Level Navigator"
        "Focus the Level Navigator at top to view active storeys, elevated space distribution, and storey hierarchy."
        "Focus Level Navigator" LevelNav None

    def "Navigate Storey Views"
        "Switch between Level 0 and Level 1 tabs in the Level Navigator to inspect plan adjacencies across storeys."
        "Select L0 / L1 tabs" LevelNav None
|]

// ─── Level 4: Program Nesting (4 Steps) ───
let nestsStepDefs = [|
    def "Initiate Nested Sub-Program"
        "Select a node with no children and tap the Nest badge to embed a secondary spatial sub-tree program."
        "Tap Nest badge on leaf node" BadgeNest None

    def "Confirm Sub-Program Nesting"
        "Confirm NEST inside the node overlay to generate a nested program zone within the spatial structure."
        "Confirm NEST action" NoBadge None

    def "Navigate Breadcrumb Program Trees"
        "Use the program breadcrumbs bar to navigate seamlessly between main level views and nested sub-programs."
        "Focus Nests Breadcrumb" LevelNav None

    def "Prune Obsolete Spaces"
        "Select any redundant space node and tap the Delete badge to clean up and refine the relational graph structure."
        "Tap & confirm Delete action" BadgeDelete None
|]


/// CSS class applied to the tree wrapper to highlight a specific badge or target.
let badgeCssClass = function
    | NoBadge       -> ""
    | BadgeMove     -> "tutorial-hl-badge-move"
    | BadgeAdd      -> "tutorial-hl-badge-add"
    | BadgeDelete   -> "tutorial-hl-badge-delete"
    | BadgeElevate  -> "tutorial-hl-badge-elevate"
    | BadgeNest     -> "tutorial-hl-badge-nest"
    | PropsBar      -> "tutorial-hl-props-bar"
    | LevelNav      -> "tutorial-hl-level-nav"
    | EditName      -> "tutorial-hl-edit-name"
    | EditWeight    -> "tutorial-hl-edit-weight"
    | HyweaveBtn    -> "tutorial-hl-hyweave"
    | TabLayoutPanel-> "tutorial-hl-tab-layout"
    | TabViewPanel  -> "tutorial-hl-tab-view"
    | TabBatchPanel -> "tutorial-hl-tab-batch"
    | BadgeSlider   -> "tutorial-hl-slider"

// ─────────────────────────────────────────────
// Snapshot builder — real SubModel per step
// ─────────────────────────────────────────────

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

let private buildBaseSnapshots () : SubModel[] =
    let s0 = makeSingleNodeModel ()
    let rootNode0 = rootOf s0
    let rootId = rootNode0.Id

    let s1a = sel s0 (Some rootId)

    let s1base =
        let r = rootOf s0
        let newRoot = TreeOps.updateNodeById rootId (fun n -> { n with Name = "Entry"; Weight = "25" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s0 with Levels = s0.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s1b = sel s1base (Some rootId)

    let s2a_base = addNamedChild rootId "Studio" "24" s1base
    let rootNode2a = rootOf s2a_base
    let sp1Node = rootNode2a.Children |> List.find (fun n -> n.Name.Contains("Studio"))
    let sp1Id = sp1Node.Id
    let s2a = sel s2a_base (Some rootId)

    let s2b_base = addNamedChild rootId "Bedroom" "16" s2a_base
    let rootNode2b = rootOf s2b_base
    let sp2Node = rootNode2b.Children |> List.find (fun n -> n.Name.Contains("Bedroom"))
    let sp2Id = sp2Node.Id
    let s2b = sel s2b_base (Some rootId)

    let s2c_base = addNamedChild rootId "Bath" "8" s2b_base
    let rootNode2c = rootOf s2c_base
    let sp3Node = rootNode2c.Children |> List.find (fun n -> n.Name.Contains("Bath"))
    let sp3Id = sp3Node.Id
    let s2c = sel s2c_base (Some sp3Id)

    let s2d1 = sel s2c_base (Some sp1Id)

    let s2d2_base =
        let r = rootOf s2c_base
        let newRoot = TreeOps.updateNodeById sp1Id (fun n -> { n with Weight = "36" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s2c_base with Levels = s2c_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s2d2 = sel s2d2_base (Some sp1Id)

    let s2e1 = sel s2d2_base (Some sp2Id)

    let s2e2_base =
        let r = rootOf s2d2_base
        let newRoot = TreeOps.updateNodeById sp2Id (fun n -> { n with Name = "Bedroom" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s2d2_base with Levels = s2d2_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s2e2 = sel s2e2_base (Some sp2Id)

    let s2f1 = sel s2e2_base (Some sp3Id)

    let s2f2_base =
        let r = rootOf s2e2_base
        let newRoot = TreeOps.updateNodeById sp3Id (fun n -> { n with Name = "Bath" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s2e2_base with Levels = s2e2_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s2f2 = sel s2f2_base (Some sp3Id)

    let s3 = s2f2

    let child2Node = TreeOps.findNodeById sp2Id (rootOf s2f2_base) |> Option.get
    let dragPtLeft = { SvgX = child2Node.X - 35.0; SvgY = child2Node.Y }
    let s4a = { s3 with DraggingId = Some sp3Id; DragPos = Some dragPtLeft; DropTargetId = Some sp2Id; DropTargetMode = Some DropBefore }

    let s4bbase =
        let (rootWithoutSource, extracted) = TreeOps.extractNode sp3Id (rootOf s2f2_base)
        match rootWithoutSource, extracted with
        | Some rs, Some ex ->
            let reordered = TreeOps.insertBefore sp2Id ex rs
            let laidOut = fst (TreeOps.layoutTree reordered 0 50.0)
            { s2f2_base with Levels = s2f2_base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
        | _ -> s2f2_base
    let s4b = sel s4bbase (Some sp3Id)

    let rootNodeS4b = rootOf s4bbase
    let child1NodeS4b = TreeOps.findNodeById sp1Id rootNodeS4b |> Option.get
    let dragPtChild = { SvgX = child1NodeS4b.X; SvgY = child1NodeS4b.Y + 45.0 }
    let s5a = { s4b with DraggingId = Some sp3Id; DragPos = Some dragPtChild; DropTargetId = Some sp1Id; DropTargetMode = Some DropAsChild }

    let s5bbase =
        let (rootWithoutSource, extracted) = TreeOps.extractNode sp3Id (rootOf s4bbase)
        match rootWithoutSource, extracted with
        | Some rs, Some ex ->
            let reordered = TreeOps.attachChild sp1Id ex rs
            let laidOut = fst (TreeOps.layoutTree reordered 0 50.0)
            { s4bbase with Levels = s4bbase.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
        | _ -> s4bbase
    let s5b = sel s5bbase (Some sp3Id)

    let s6a = sel s5bbase (Some sp2Id)
    let s6b = { s6a with ConfirmingId = Some sp2Id; ActiveActionId = ActionIds.Elevate; SelectedNodeId = Some sp2Id }

    let child2NodeS5b = TreeOps.findNodeById sp2Id (rootOf s5bbase)
    let s7 =
        match child2NodeS5b with
        | Some n -> fst (Actions.elevateActionLogic.Execute s5bbase n)
        | None   -> s5bbase

    let s8a = sel s7 None
    let s8b = { s7 with ActiveLevel = 0; ActiveNest = None; SelectedNodeId = Some sp3Id } |> Coloring.colorModel

    let s9a = sel s8b (Some sp3Id)
    let s9b = { s9a with ConfirmingId = Some sp3Id; ActiveActionId = ActionIds.Nest; SelectedNodeId = Some sp3Id }

    let child3NodeS8 = TreeOps.findNodeById sp3Id (rootOf s8b)
    let s10 =
        match child3NodeS8 with
        | Some n -> fst (Actions.nestActionLogic.Execute s8b n)
        | None   -> s8b

    let s11a = sel s10 None
    let s11b = { s10 with ActiveLevel = 0; ActiveNest = None; SelectedNodeId = Some sp3Id } |> Coloring.colorModel

    let s12a = sel s11b (Some sp3Id)
    let s12b = { s12a with ConfirmingId = Some sp3Id; ActiveActionId = ActionIds.Delete; SelectedNodeId = Some sp3Id }

    let s13base =
        match TreeOps.removeNodeById sp3Id (rootOf s11b) with
        | Some newRoot ->
            let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
            { s11b with Levels = s11b.Levels |> Map.add 0 laidOut; ConfirmingId = None; ActiveActionId = ActionIds.NoAction }
            |> Coloring.colorModel
        | None -> s11b
    let s13 = sel s13base (Some rootId)

    let s14a = sel s13base (Some rootId)
    let s14bbase =
        let r = rootOf s13base
        let newRoot = TreeOps.updateNodeById rootId (fun n -> { n with Weight = "150" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s13base with Levels = s13base.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s14b = sel s14bbase (Some rootId)

    let s15a = sel s14bbase (Some rootId)
    let s15bbase =
        let r = rootOf s14bbase
        let newRoot = TreeOps.updateNodeById rootId (fun n -> { n with Name = "Main Hub"; Weight = "200" }) r
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { s14bbase with Levels = s14bbase.Levels |> Map.add 0 laidOut } |> Coloring.colorModel
    let s15b = sel s15bbase (Some rootId)
    let s16 = sel s15bbase None

    [| s0; s1a; s1b; s2a; s2b; s2c; s2d1; s2d2; s2e1; s2e2; s2f1; s2f2; s3; s4a; s4b; s5a; s5b; s6a; s6b; s7; s8a; s8b; s9a; s9b; s10; s11a; s11b; s12a; s12b; s13; s14a; s14b; s15a; s15b; s16; s16; s16 |]

let private baseSnapshots : SubModel[] = buildBaseSnapshots ()

let basicSnapshots : SubModel[] =
    let entryStudioBed = baseSnapshots.[4] // s2b_base (Entry 25, Studio 24, Bedroom 16)
    let rootNode = rootOf entryStudioBed
    let bedNode = rootNode.Children |> List.find (fun n -> n.Name.Contains("Bedroom"))
    let bedId = bedNode.Id

    let bathUnderBedModel = addNamedChild bedId "Bath" "8" entryStudioBed
    let rootNodeBath = rootOf bathUnderBedModel
    let bathNodeOpt = rootNodeBath.Children |> List.tryPick (fun n -> n.Children |> List.tryFind (fun c -> c.Name.Contains("Bath")))
    let bathId = match bathNodeOpt with Some b -> b.Id | None -> bedId

    let step0 = sel entryStudioBed (Some bedId)       // Step 1: Entry 25, Studio 24, Bedroom 16 (Bedroom selected, + badge highlighted, Bath NOT present initially)
    let step1 = sel bathUnderBedModel (Some bathId)   // Step 2: Bath 8 added under Bedroom 16 (Bath selected, EditWeight highlighted)
    let step2 = sel bathUnderBedModel None            // Step 3: Compile lattice on LayoutPanel
    let step3 = sel bathUnderBedModel None            // Step 4: Explore alternate configurations on LayoutPanel
    let step4 = sel bathUnderBedModel None            // Step 5: Inspect all configurations on BatchPanel

    [| step0; step1; step2; step3; step4 |]

let hierarchySnapshots : SubModel[] =
    let cleanS2f2 = baseSnapshots.[11]
    let rootNode = rootOf cleanS2f2
    let sp3Node = rootNode.Children |> List.tryFind (fun n -> n.Name.Contains("Service") || n.Name.Contains("Child 3") || n.Name.Contains("Space 3")) |> Option.defaultValue (List.last rootNode.Children)
    let sp3Id = sp3Node.Id

    let sWeightEdit =
        let newRoot = TreeOps.updateNodeById sp3Id (fun n -> { n with Weight = "150" }) rootNode
        let laidOut = fst (TreeOps.layoutTree newRoot 0 50.0)
        { cleanS2f2 with Levels = cleanS2f2.Levels |> Map.add 0 laidOut } |> Coloring.colorModel

    [|
        baseSnapshots.[12]           // Step 0: Initiate Node Relocation (Select Move Badge)
        baseSnapshots.[13]           // Step 1: Re-order Sibling Circulation (Drag to re-order)
        baseSnapshots.[15]           // Step 2: Re-parent Child Spaces (Drag to re-parent)
        sel sWeightEdit (Some sp3Id) // Step 3: Balance Target Area Allocations (Alter Weight)
    |]

let levelsSnapshots : SubModel[] =
    [|
        baseSnapshots.[17]  // Step 0: Initiate Vertical Elevation (Select Elevate Badge)
        baseSnapshots.[18]  // Step 1: Confirm Level 1 Creation (Elevate Confirmation UI)
        baseSnapshots.[20]  // Step 2: Inspect Level Navigator (Focus Level Navigator)
        baseSnapshots.[21]  // Step 3: Navigate Storey Views (Select L0 / L1 tabs)
    |]

let nestsSnapshots : SubModel[] =
    let cleanS2f2 = baseSnapshots.[11]
    let rootNode = rootOf cleanS2f2
    let sp3Node = rootNode.Children |> List.tryFind (fun n -> n.Name.Contains("Service") || n.Name.Contains("Child 3") || n.Name.Contains("Space 3")) |> Option.defaultValue (List.last rootNode.Children)
    let sp3Id = sp3Node.Id
    let sDelSel = sel cleanS2f2 (Some sp3Id)
    let sDelConfirm = { sDelSel with ConfirmingId = Some sp3Id; ActiveActionId = ActionIds.Delete }

    [|
        baseSnapshots.[22]  // Step 0: Initiate Nested Sub-Program (Select Nest Badge)
        baseSnapshots.[23]  // Step 1: Confirm Sub-Program Nesting (Nest Confirmation UI)
        baseSnapshots.[26]  // Step 2: Navigate Breadcrumb Program Trees (Focus Nests Breadcrumb)
        sDelConfirm         // Step 3: Prune Obsolete Spaces (Confirm Delete Action)
    |]

let getDefsAndSnapshots (level: TutorialLevel) : TutorialStepDef[] * SubModel[] =
    match level with
    | Basic     -> basicStepDefs, basicSnapshots
    | Hierarchy -> hierarchyStepDefs, hierarchySnapshots
    | Levels    -> levelsStepDefs, levelsSnapshots
    | Nests     -> nestsStepDefs, nestsSnapshots


let getStepDef (level: TutorialLevel) (step: int) : TutorialStepDef =
    let defs, _ = getDefsAndSnapshots level
    let idx = Math.Clamp(step, 0, defs.Length - 1)
    defs.[idx]

let getSnapshot (level: TutorialLevel) (step: int) : SubModel =
    let _, snaps = getDefsAndSnapshots level
    let idx = Math.Clamp(step, 0, snaps.Length - 1)
    snaps.[idx]

let getTotalSteps (level: TutorialLevel) : int =
    let defs, snaps = getDefsAndSnapshots level
    min defs.Length snaps.Length

// Legacy helpers for back-compat
let stepDefs = basicStepDefs
let snapshots = basicSnapshots
let totalSteps = basicStepDefs.Length

// ─────────────────────────────────────────────
// Badge visual helpers
// ─────────────────────────────────────────────

let private badgeIcon = function
    | BadgeMove     -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="5 9 2 12 5 15"/><polyline points="9 5 12 2 15 5"/><polyline points="15 19 12 22 9 19"/><polyline points="19 9 22 12 19 15"/><line x1="2" y1="12" x2="22" y2="12"/><line x1="12" y1="2" x2="12" y2="22"/></svg>"""
    | BadgeAdd      -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.8" stroke-linecap="round"><line x1="12" y1="5" x2="12" y2="19"/><line x1="5" y1="12" x2="19" y2="12"/></svg>"""
    | BadgeDelete   -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"/></svg>"""
    | BadgeElevate  -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>"""
    | BadgeNest     -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2"/><rect x="8" y="8" width="8" height="8" rx="1"/></svg>"""
    | LevelNav      -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><rect x="2" y="7" width="20" height="4" rx="1"/><rect x="2" y="13" width="20" height="4" rx="1"/></svg>"""
    | PropsBar      -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><line x1="3" y1="6" x2="21" y2="6"/><line x1="3" y1="12" x2="15" y2="12"/><line x1="3" y1="18" x2="18" y2="18"/></svg>"""
    | EditName      -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"/><path d="M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"/></svg>"""
    | EditWeight    -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 7V4h16v3"/><path d="M9 20h6"/><path d="M12 4v16"/></svg>"""
    | HyweaveBtn    -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><polygon points="5 3 19 12 5 21 5 3"/></svg>"""
    | TabLayoutPanel-> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><rect x="3" y="3" width="18" height="18" rx="2"/><line x1="3" y1="9" x2="21" y2="9"/><line x1="9" y1="21" x2="9" y2="9"/></svg>"""
    | TabViewPanel  -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"/></svg>"""
    | TabBatchPanel -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><rect x="3" y="3" width="7" height="7" rx="1"/><rect x="14" y="3" width="7" height="7" rx="1"/><rect x="14" y="14" width="7" height="7" rx="1"/><rect x="3" y="14" width="7" height="7" rx="1"/></svg>"""
    | BadgeSlider   -> """<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><line x1="4" y1="21" x2="4" y2="14"/><line x1="4" y1="10" x2="4" y2="3"/><line x1="12" y1="21" x2="12" y2="12"/><line x1="12" y1="8" x2="12" y2="3"/><line x1="20" y1="21" x2="20" y2="16"/><line x1="20" y1="12" x2="20" y2="3"/><line x1="1" y1="14" x2="7" y2="14"/><line x1="9" y1="8" x2="15" y2="8"/><line x1="17" y1="16" x2="23" y2="16"/></svg>"""
    | NoBadge       -> ""

let private badgeColor = function
    | BadgeMove     -> "#e67e22"
    | BadgeAdd      -> "#4ade80"
    | BadgeDelete   -> "#f87171"
    | BadgeElevate  -> "#60a5fa"
    | BadgeNest     -> "#34d399"
    | LevelNav      -> "#a78bfa"
    | PropsBar      -> "#fbbf24"
    | EditName      -> "#06b6d4"
    | EditWeight    -> "#06b6d4"
    | HyweaveBtn    -> "#6366f1"
    | TabLayoutPanel-> "#8b5cf6"
    | TabViewPanel  -> "#ec4899"
    | TabBatchPanel -> "#8b5cf6"
    | BadgeSlider   -> "#8b5cf6"
    | NoBadge       -> "transparent"

// ─────────────────────────────────────────────
// View
// ─────────────────────────────────────────────

let viewTutorialBanner (level: TutorialLevel) (tutorialStep: int) (autoPlay: bool) (dispatch: Message -> unit) : Node =
    let totalSteps = getTotalSteps level
    let step       = Math.Clamp(tutorialStep, 0, totalSteps - 1)
    let def        = getStepDef level step
    let isFirst    = step = 0
    let isLast     = step = totalSteps - 1

    div {
        attr.``class`` (if autoPlay then "tutorial-banner is-autoplay" else "tutorial-banner")

        // Module Level Selector Pills (without icons)
        div {
            attr.``class`` "tutorial-level-strip"
            for lvl in TutorialLevel.allLevels do
                let isLvlActive = lvl = level
                button {
                    attr.``class`` (if isLvlActive then "tutorial-level-btn active" else "tutorial-level-btn")
                    attr.title (TutorialLevel.name lvl)
                    on.click (fun _ -> dispatch (SetTutorialLevel lvl))
                    span { attr.``class`` "tutorial-level-name"; text (TutorialLevel.shortName lvl) }
                }
        }

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
            let durationSec =
                match def.Badge, def.TargetPanel with
                | HyweaveBtn, _ | _, Some BatchPanel -> 8.0
                | _ -> 4.5
            div {
                attr.``class`` "tutorial-progress-bar"
                div {
                    attr.``class`` "tutorial-progress-fill"
                    attr.style $"animation-duration: {durationSec}s;"
                }
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
                if isLast then
                    match TutorialLevel.nextLevel level with
                    | Some nxt ->
                        button {
                            attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-dark tutorial-next-module"
                            attr.title $"Proceed to {TutorialLevel.name nxt}"
                            on.click (fun _ -> dispatch (SetTutorialLevel nxt))
                            text $"Next: {TutorialLevel.shortName nxt} "
                            rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="9 18 15 12 9 6"></polyline></svg>"""
                        }
                    | None -> ()

                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-primary tutorial-next"
                        on.click (fun _ -> dispatch DismissTutorial)
                        text "Start designing "
                        rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"></polyline></svg>"""
                    }
                else
                    button {
                        attr.``class`` "hywe-btn hywe-btn-sm hywe-btn-dark tutorial-next"
                        on.click (fun _ -> dispatch TutorialNext)
                        text "Next "
                        rawHtml """<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="9 18 15 12 9 6"></polyline></svg>"""
                    }
            }
        }
    }
