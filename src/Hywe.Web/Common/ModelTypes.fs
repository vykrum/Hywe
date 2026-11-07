/// <summary>
/// Domain model types, Elmish state definitions, message unions, and geometric derivation helpers for Hywe.
/// </summary>
module ModelTypes

open TreeTypes
open Types
open Hywe.Core.Coxel
open System

/// <summary> Specifies the currently active main UI panel in the application shell. </summary>
type ActivePanel =
    | TreePanel
    | BoundaryPanel
    | LayoutPanel
    | AnalyzePanel
    | ViewPanel
    | BatchPanel
    | TeachPanel
    | ReportPanel

/// <summary> Specifies the onboarding tutorial difficulty level sequence. </summary>
type TutorialLevel =
    | Basic
    | Hierarchy
    | Levels
    | Nests

/// <summary> Specifies the input methodology for editing the node hierarchy. </summary>
type EditorMode =
    | Interactive
    | Syntax

/// <summary> Specifies active editor tabs within workspace views. </summary>
type EditorTab =
    | Boundary
    | Editor of isAdvanced: bool

/// <summary> Calculated spatial, geometric, topological, color, and solar derivations from the layout solver. </summary>
type DerivedData = {
    cxCxl1: Cxl[]
    cxlAvl: int[]
    cxClr1: string[]
    cxOuIl: (int*int)[][]
    cxElv1: float[]
    cxRto1: float[]
    cxAdj1: string[] * bool[][]
    cxB36: string[]
    cxSol1: float[] option
}

// --- Constants & Helpers ---

/// <summary> Standard phrase string used for generating unique alphanumeric labels for batch layout variations. </summary>
let labelPhrase = "alternATE◦CONFIGURATions"

/// <summary> Maps a zero-based variation index to a unique, formatted variation label string. </summary>
let indexToLabel = function
    | i when i < 0 -> "a"
    | i when i < 24 -> string labelPhrase.[i]
    | i ->
        let first = labelPhrase.[(i / 24) - 1]
        let second = labelPhrase.[i % 24]
        $"{first}{second}"

/// <summary> Converts a hexadecimal color string to RGB integer components. </summary>
let hexToRgb = Coloring.hexToRgb

/// <summary> Generates deterministic pastel color hex codes based on a seed color and count. </summary>
let generatePastels = Coloring.generatePastels

/// <summary> Derives calculated spatial attributes (<see cref="DerivedData"/>) from raw layout coxels, boundary contours, and solar parameters. </summary>
let deriveDataFromLayout (cxCxl1: Cxl[]) (cxOuIl: (int*int)[][]) (cxElv1: float[]) (cxRto1: float[]) (elv: int) (latitude: float option) : DerivedData =
    let fallbackSqn = Hywe.Core.Hexel.VRCCNE 
    let activeSqn = 
        cxCxl1 
        |> Array.tryFind (fun c -> let (_, _, z) = Hywe.Core.Hexel.hxlCrd c.Base in z = elv)
        |> Option.orElse (Array.tryHead cxCxl1)
        |> Option.map (fun c -> c.Seqn)
        |> Option.defaultValue fallbackSqn
    
    let cxlAvl = 
        match cxCxl1 with
        | [||] -> [||]
        | _ -> Hywe.Core.Coxel.cxlExp cxCxl1 activeSqn elv
    
    // Deterministic coloring based on architectural ID (Rfid) to ensure consistency across levels
    let uniqueRfids = cxCxl1 |> Array.map (fun c -> Hywe.Core.Coxel.prpVlu c.Rfid) |> Array.distinct
    let colorMap = 
        generatePastels "#888888" (max 1 (Array.length uniqueRfids)) 0.85
        |> Array.zip uniqueRfids
        |> Map.ofArray

    let cxClr1 = cxCxl1 |> Array.map (fun c -> colorMap.[Hywe.Core.Coxel.prpVlu c.Rfid])
    let cxAdj1 = Hywe.Core.Coxel.cxlAdj cxCxl1
    let cxB36 = cxCxl1 |> Array.map Hywe.Core.Coxel.getCxlCoordsString

    let cxSol1 = 
        latitude
        |> Option.map (fun lat ->
            let allHxls = cxCxl1 |> Array.collect (fun c -> c.Hxls)
            let occSet = Hywe.Core.Hexel.hxlSet allHxls

            let degreesToRadians deg = deg * System.Math.PI / 180.0
            let approxDailyInsolation azDeg =
                let equatorFacing = 
                    match lat with
                    | l when l >= 0.0 -> 180.0
                    | _ -> 0.0
                let azDiff = abs(azDeg - equatorFacing)
                let azDiffMod = 
                    match azDiff with
                    | d when d > 180.0 -> 360.0 - d
                    | d -> d
                let facingFactor = System.Math.Cos(degreesToRadians azDiffMod)
                200.0 + (100.0 * facingFactor)

            cxCxl1 |> Array.map (fun cxl ->
                let openEdges = 
                    cxl.Hxls 
                    |> Array.collect (fun h -> 
                        let hx, hy, _ = Hywe.Core.Hexel.hxlCrd h
                        Hywe.Core.Hexel.adjacent cxl.Seqn h
                        |> Array.filter (occSet.Contains >> not)
                        |> Array.map (fun a -> 
                            let ax, ay, _ = Hywe.Core.Hexel.hxlCrd a
                            let dx = float (ax - hx)
                            let dy = float (ay - hy)
                            
                            let rad = System.Math.Atan2(dy, dx)
                            let mutAz = 90.0 + (rad * 180.0 / System.Math.PI)
                            let azimuth = 
                                match mutAz with
                                | a when a < 0.0 -> a + 360.0
                                | a when a >= 360.0 -> a - 360.0
                                | a -> a
                            
                            approxDailyInsolation azimuth
                        )
                    )

                match openEdges with
                | [||] -> 0.0
                | edges ->
                    try Array.average edges
                    with _ -> 0.0
            )
        )

    {
        cxCxl1 = cxCxl1
        cxlAvl = cxlAvl
        cxClr1 = cxClr1
        cxOuIl = cxOuIl
        cxElv1 = cxElv1
        cxRto1 = cxRto1
        cxAdj1 = cxAdj1
        cxB36 = cxB36
        cxSol1 = cxSol1
    }

/// <summary> Default empty instance of <see cref="DerivedData"/> used before initial layout solving. </summary>
let emptyDerivedData : DerivedData = {
    cxCxl1 = [||]
    cxlAvl = [||]
    cxClr1 = [||]
    cxOuIl = [||]
    cxElv1 = [||]
    cxRto1 = [||]
    cxAdj1 = ([||], [||])
    cxB36 = [||]
    cxSol1 = None
}

/// <summary> Action types requiring user confirmation in modal dialogs before execution. </summary>
type ConfirmAction =
    | ResetWorkspace
    | LoadPreset of name: string * label: string
    | LoadGallery of name: string * rowId: string * author: string
    | SwitchTo of EditorTab
    | ResetBoundaryAction

/// <summary> Default published timestamp string for document metadata. </summary>
let PUBLISHED_DATE = "2022-08-15T00:00:00Z"

/// <summary> Default modified timestamp string for document metadata. </summary>
let MODIFIED_DATE = "2026-09-28T03:35:49Z"

/// <summary> Number of community gallery entries displayed per page. </summary>
let GALLERY_PAGE_SIZE = 12

/// <summary> Export payload containing serialized boundary string representations, dimensions, entry points, and geographic metadata. </summary>
type PolygonExportData = {
    OuterStr: string
    IslandsStr: string
    AbsStr: string
    EntryStr: string
    Elevation: int
    BaseStr: string
    Width: int
    Height: int
    Latitude: float option
    MapScale: float
}

/// <summary> Minimal snapshot of undoable editor state. </summary>
type UndoSnapshot = {
    SrcOfTrth: string
    Tree: SubModel
    PolygonEditor: EditorState
    Sequences: Map<int, string>
}

/// <summary> Design metadata and classification tags captured during AI exploration and community submissions. </summary>
type TeachMetadata = {
    Author: string
    ExplorationDescription: string
    SessionId: string
    Scale: string
    Typology: string
    Flow: string
    Ambience: string
    Stage: string
    ThumbnailLevel: string
    ThumbnailVariation: int
}

/// <summary> Application workflow view state (loading screen, onboarding intro, or main workspace interface). </summary>
type AppScreen =
    | LoadingScreen
    | IntroScreen
    | MainScreen

// Batch Export Types

/// <summary> Renderable component shape data for batch export preview SVGs. </summary>
type BatchComponent = {| color: string; points: float[]; name: string; lx: float; ly: float |}

/// <summary> Complete layout variation configuration result produced during batch synthesis. </summary>
type BatchConfgrtns = {| sqnName: string; shapes: BatchComponent[]; wtmkShapes: BatchComponent[] option; w: float; h: float; mapScale: float; cxCxl1: Cxl[]; cxElv1: float[]; cxlAvl: int[]; cxOuIl: (int*int)[][]; cxAdj1: string[] * bool[][] ; cxB36: string[]; cxRto1: float[]; cxClr1: string[]; cxSol1: float[] option |}

// Report Types

/// <summary> Section visibility flags and variation selection filters for exported reports. </summary>
type LevelReportSections = {
    FlowChart    : bool
    BatchOverview: bool
    Variations   : bool
    SelectedVariations: Set<int>
    IsFilterExpanded: bool
}

/// <summary> Options and configuration for generating PDF and HTML architectural documentation reports. </summary>
type ReportOptions = {
    ProjectTitle  : string
    Author        : string
    IncludeCover  : bool
    LevelSections : Map<string, LevelReportSections>
    Captured3DImage: string option
}

/// <summary> In-memory cache mapping level markers to arrays of 24 batch layout configurations. </summary>
type LayoutCache = Map<string, BatchConfgrtns option []>

/// <summary> Metadata entry representing a published community design exploration in the gallery. </summary>
type GalleryEntry = {
    Id: string
    ExplorationDescription: string
    Author: string
    Description: string
    SvgThumbnail: string
    LevelsCount: int
    SpacesCount: int
    Typology: string
    Scale: string
    Stage: string
    Flow: string
    Ambience: string
    CreatedAt: string
    IsFeatured: bool
}

/// <summary> Main application state model for Hywe Elmish architecture. </summary>
type Model =
    {
        Sequences: Map<int, string>
        Elevation: int
        BaseStr: string
        SrcOfTrth : string
        Tree : SubModel
        LastValidTree: SubModel
        ParseError: bool
        Derived : DerivedData
        LayoutCache : LayoutCache
        NeedsHyweave: bool
        IsHyweaving: bool
        PolygonEditor: EditorState
        ActivePanel: ActivePanel
        EditorMode: EditorMode
        IsCancelling: bool
        CancelToken: System.Threading.CancellationTokenSource option
        LastBatchSrc: string option
        SelectedPreviewIndex : int option
        Captured3DImage: string option
        UserDescription : string 
        TeachMetadata: TeachMetadata
        HoveredInfo: string option
        IsSavingToHynteract : bool
        ShowSuccessMessage : bool
        TeachErrorMessage : string option
        IsRecording : bool
        PolygonExport: PolygonExportData
        BatchProgress: int
        CurrentScreen: AppScreen
        ViewLocked: bool
        ReportOptions: ReportOptions
        ReportBatch: Map<int, BatchConfgrtns[]>
        IsGeneratingReport: bool
        SelectedPreset: string option
        EditsCount: int
        IsPresetsCollapsed: bool
        IsWorkspaceCollapsed: bool
        IsHarvestCollapsed: bool
        IsTreeCollapsed: bool
        PendingConfirm: ConfirmAction option
        UndoStack: UndoSnapshot list
        RedoStack: UndoSnapshot list
        PreDragSnapshot: UndoSnapshot option
        InstallPromptAvailable: bool
        ShowPrivacyAlert: bool
        IsStandalone: bool
        IsCoordsVisible: bool
        ShowLinkCopied: bool
        ShowGallery: bool
        ShowAboutModal: bool
        ShowHarvestModal: bool
        IsLoadingGallery: bool
        GalleryEntries: GalleryEntry list option
        GalleryOffset: int
        GalleryFilter: string
        LoadedCommunityAuthor: string option
        CachedAuthor: string option
        HasAppendedModSuffix: bool
        TutorialLevel: TutorialLevel
        TutorialStep: int option
        TutorialAutoPlay: bool
        PreTutorialSnapshot: UndoSnapshot option
    }

/// <summary> Metadata header used when generating downloadable CSV and SVG exports. </summary>
[<CLIMutable>]
type ExportMetadata = {
    ProjectTitle: string
    Author: string
    Date: string
    Level: string
}

/// <summary> Helper utilities for preparing export metadata and reports. </summary>
module ExportHelpers =
    /// <summary> Creates an <see cref="ExportMetadata"/> snapshot from current application model settings. </summary>
    let createExportMetadata (model: Model) (levelStr: string) : ExportMetadata =
        let pTitle = 
            if System.String.IsNullOrWhiteSpace model.ReportOptions.ProjectTitle then "Hywe Exploration" 
            else model.ReportOptions.ProjectTitle
        let pAuthor = 
            if System.String.IsNullOrWhiteSpace model.ReportOptions.Author then "Hywe Design Team" 
            else model.ReportOptions.Author
        {
            ProjectTitle = pTitle
            Author = pAuthor
            Date = System.DateTime.Now.ToString("yyyy-MM-dd")
            Level = levelStr
        }

    /// <summary> Serializes ExportMetadata to a clean JSON string to prevent JSInterop constructor reflection errors. </summary>
    let toJson (meta: ExportMetadata) : string =
        let esc (s: string) = 
            if isNull s then "\"\""
            else System.Text.Json.JsonSerializer.Serialize(s)
        sprintf "{\"projectTitle\":%s,\"author\":%s,\"date\":%s,\"level\":%s}"
            (esc meta.ProjectTitle)
            (esc meta.Author)
            (esc meta.Date)
            (esc meta.Level)

    /// <summary> Parses a JSON string into an ExportMetadata record. </summary>
    let fromJson (json: string) : ExportMetadata =
        try
            if System.String.IsNullOrWhiteSpace json then
                { ProjectTitle = "Hywe Exploration"; Author = "Hywe Design Team"; Date = System.DateTime.Now.ToString("yyyy-MM-dd"); Level = "L0" }
            else
                use doc = System.Text.Json.JsonDocument.Parse json
                let root = doc.RootElement
                let getString (prop: string) (defaultVal: string) : string =
                    let mutable elem = Unchecked.defaultof<System.Text.Json.JsonElement>
                    if root.TryGetProperty(prop, &elem) && elem.ValueKind = System.Text.Json.JsonValueKind.String then
                        elem.GetString()
                    else
                        defaultVal
                let title = 
                    match getString "projectTitle" "" with
                    | "" -> getString "ProjectTitle" "Hywe Exploration"
                    | t -> t
                let author = 
                    match getString "author" "" with
                    | "" -> getString "Author" "Hywe Design Team"
                    | a -> a
                let date = 
                    match getString "date" "" with
                    | "" -> getString "Date" (System.DateTime.Now.ToString("yyyy-MM-dd"))
                    | d -> d
                let level = 
                    match getString "level" "" with
                    | "" -> getString "Level" "L0"
                    | l -> l
                { ProjectTitle = title; Author = author; Date = date; Level = level }
        with _ ->
            { ProjectTitle = "Hywe Exploration"; Author = "Hywe Design Team"; Date = System.DateTime.Now.ToString("yyyy-MM-dd"); Level = "L0" }

/// <summary> Messages representing all possible state changes in the main application Elmish loop. </summary>
type Message =
    | SetSqnIndex of int
    | SetSrcOfTrth of string
    | TreeMsg of SubMsg
    | StartHyweave
    | RunHyweave
    | FinishHyweave
    | HyweaveResult of src: string * cache: LayoutCache
    | CacheResult of marker: string * lvl: int * sqnIdx: int * data: BatchConfgrtns
    | PolygonEditorMsg of PolygonEditorMessage
    | PolygonEditorUpdated of PolygonEditorModel
    | SetActivePanel of ActivePanel
    | SetBatchFinished
    | SetBatchProgress of int
    | GenerateNextBatchItem of int
    | AddBatchItem of LayoutCache
    | ToggleEditorMode
    | ToggleBoundary
    | ExportPdfRequested
    | DownloadCoordCsv
    | DownloadMetricsCsv
    | DownloadAdjCsv
    | DownloadBatchCoordCsv
    | DownloadBatchMetricsCsv
    | DownloadBatchAdjCsv
    | DownloadBatchSvg
    | DownloadBatchPng
    | Download3DSvg
    | TapBatchPreview of int
    | CloseBatch
    | CancelBatch
    | BatchCancelled
    | SaveRequested
    | ImportRequested
    | FileImported of string
    | SetDescription of string
    | SuggestDescription
    | UpdateMetadata of (TeachMetadata -> TeachMetadata)
    | SetHoveredInfo of string option
    | RecordToHynteract
    | RecordResult of success: bool * errorMsg: string option * cache: LayoutCache
    | StartVoiceCapture
    | OnVoiceResult
    | TogglePresetsCollapse
    | ToggleWorkspaceCollapse
    | ToggleTreeCollapse
    | SetTreeCollapse of bool
    | TransitionToIntro
    | TransitionToMain
    | ToggleViewLock
    | Download3DPng
    | DownloadTreeSvg
    | DownloadTreePng
    | ToggleHarvestCollapse
    | SetHarvestCollapse of bool
    | DownloadLayoutSvg
    | DownloadLayoutPng
    | DownloadMapImage
    | DownloadTerrainGrid
    | UpdateReportOptions of (ReportOptions -> ReportOptions)
    | GenerateReport
    | ReportGenerated of html: string * cache: LayoutCache
    | ViewCaptured of string
    | SelectPreset of string
    | LoadState of content: string * panel: ActivePanel option * isFromUrl: bool
    | ShareLink
    | HideLinkCopied
    | HardReset
    | ToggleConfirm of ConfirmAction option
    | Undo
    | Redo
    | SetInstallPromptAvailable of bool
    | InstallRequested
    | SetPrivacyAlert of bool
    | SetIsStandalone of bool
    | ToggleCoords
    | ToggleGallery
    | ToggleAboutModal
    | SetShowAboutModal of bool
    | ToggleHarvestModal
    | SetShowHarvestModal of bool
    | LoadGalleryEntries
    | NextGalleryPage
    | PrevGalleryPage
    | GoToGalleryPage of page: int
    | GalleryEntriesLoaded of GalleryEntry list
    | LoadGalleryDefinition of name: string * rowId: string * author: string
    | LoadGalleryDefinitionSuccess of name: string * definition: string * author: string
    | UpdateGalleryFilter of string
    | SetAuthor of string
    | SetExplorationTitle of string
    | AuthorCachedLoaded of string
    | TitleCachedLoaded of string
    | CommunityAuthorCachedLoaded of string
    | SetTutorialLevel of TutorialLevel
    | SetTutorialStep of int
    | TutorialNext
    | TutorialBack
    | DismissTutorial
    | ToggleTutorialAutoPlay
    | TutorialAutoAdvance of step: int
    | NoOp

/// <summary> Synchronizes the site polygon editor model state into a pure <see cref="PolygonExportData"/> cache. </summary>
let syncPolygonState (p: PolygonEditorModel) =
    let outer, islands, absolute, entry, w, h, elv, baseS = State.exportPolygonStrings p
    
    let w', h', entry', outer', islands' =
        match p.UseBoundary with
        | false -> w, h, "0,0", "", ""
        | true  -> w, h, entry, outer, islands
        
    let lat = 
        p.TopographyData
        |> Option.map (fun topoData ->
            try
                let node = System.Text.Json.Nodes.JsonNode.Parse(topoData)
                let extents = node.["extents"]
                let n = extents.["north"].GetValue<float>()
                let s = extents.["south"].GetValue<float>()
                (n + s) / 2.0
            with _ -> 0.0
        )

    { Elevation = elv; BaseStr = baseS; OuterStr = outer'; IslandsStr = islands'; AbsStr = absolute; EntryStr = entry'; Width = w'; Height = h'; Latitude = lat; MapScale = p.MapScale }
