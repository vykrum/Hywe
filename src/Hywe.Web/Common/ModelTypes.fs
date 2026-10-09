/// <summary>
/// Domain model types, Elmish state definitions, message unions, and geometric derivation helpers for Hywe.
/// </summary>
module ModelTypes

open TreeTypes
open Types
open Hywe.Core.Coxel
open System

/// <summary>
/// Specifies the currently active main UI panel in the application shell.
/// </summary>
type ActivePanel =
    /// <summary> Boundary &amp; site contour editor panel. </summary>
    | BoundaryPanel
    /// <summary> Interactive node tree and layout solver panel. </summary>
    | LayoutPanel
    /// <summary> Architectural metrics &amp; spatial analysis panel. </summary>
    | AnalyzePanel
    /// <summary> 3D volume view panel. </summary>
    | ViewPanel
    /// <summary> Multi-variation batch exploration panel. </summary>
    | BatchPanel
    /// <summary> AI exploration, metadata, and voice annotation panel. </summary>
    | TeachPanel
    /// <summary> PDF &amp; HTML report generation panel. </summary>
    | ReportPanel

/// <summary>
/// Specifies the onboarding tutorial difficulty level sequence.
/// </summary>
type TutorialLevel =
    /// <summary> Basic quickstart level introducing core concepts. </summary>
    | Basic
    /// <summary> Hierarchical tree structures and nesting level. </summary>
    | Hierarchy
    /// <summary> Multi-story architectural level workflows. </summary>
    | Levels
    /// <summary> Nesting boundary and polygon clipping level. </summary>
    | Nests

/// <summary>
/// Specifies the input methodology for editing the node hierarchy (interactive flowchart vs. DSL text syntax).
/// </summary>
type EditorMode =
    /// <summary> Interactive visual node tree flowchart editor. </summary>
    | Interactive
    /// <summary> Direct domain-specific syntax text editor. </summary>
    | Syntax

/// <summary>
/// Specifies active editor tabs within workspace views.
/// </summary>
type EditorTab =
    /// <summary> Site polygon and boundary setup tab. </summary>
    | Boundary
    /// <summary> Node tree editor tab, parameterized by whether advanced options are visible. </summary>
    | Editor of isAdvanced: bool

/// <summary>
/// Calculated spatial, geometric, topological, color, and solar derivations computed from the layout solver.
/// </summary>
type DerivedData = {
    /// <summary> Array of spatial coxel cells making up the calculated spatial layout. </summary>
    cxCxl1: Cxl[]
    /// <summary> Array of accessible architectural level indices per coxel. </summary>
    cxlAvl: int[]
    /// <summary> Hexadecimal CSS color strings assigned to each coxel. </summary>
    cxClr1: string[]
    /// <summary> Contour point pairs forming outer boundaries and inner island holes per level. </summary>
    cxOuIl: (int*int)[][]
    /// <summary> Elevation heights for each coxel. </summary>
    cxElv1: float[]
    /// <summary> Aspect / area ratio metrics per coxel. </summary>
    cxRto1: float[]
    /// <summary> Spatial adjacency matrix consisting of room identifiers and 2D connectivity booleans. </summary>
    cxAdj1: string[] * bool[][]
    /// <summary> Base36 spatial coordinate representation strings per coxel. </summary>
    cxB36: string[]
    /// <summary> Calculated daily solar insolation values per coxel (if latitude is provided). </summary>
    cxSol1: float[] option
}

// --- Constants & Helpers ---

/// <summary>
/// Standard phrase string used for generating unique alphanumeric labels for batch layout variations.
/// </summary>
let labelPhrase = "alternATE◦CONFIGURATions"

/// <summary>
/// Maps a zero-based variation index to a unique, formatted variation label string.
/// </summary>
/// <param name="i">Zero-based variation index.</param>
/// <returns>Formatted variation label string.</returns>
let indexToLabel = function
    | i when i < 0 -> "a"
    | i when i < 24 -> string labelPhrase.[i]
    | i ->
        let first = labelPhrase.[(i / 24) - 1]
        let second = labelPhrase.[i % 24]
        $"{first}{second}"

/// <summary>
/// Converts a hexadecimal color string to RGB integer components.
/// </summary>
let hexToRgb = Coloring.hexToRgb

/// <summary>
/// Generates deterministic pastel color hex codes based on a seed color and count.
/// </summary>
let generatePastels = Coloring.generatePastels

/// <summary>
/// Derives calculated spatial attributes (<see cref="DerivedData"/>) from raw layout coxels, boundary contours, and solar parameters.
/// </summary>
/// <param name="cxCxl1">Array of layout coxels.</param>
/// <param name="cxOuIl">Boundary contour point pairs.</param>
/// <param name="cxElv1">Elevation values per coxel.</param>
/// <param name="cxRto1">Ratio values per coxel.</param>
/// <param name="elv">Active elevation level index.</param>
/// <param name="latitude">Optional geographical latitude for solar insolation calculations.</param>
/// <returns>Constructed <see cref="DerivedData"/> record.</returns>
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

/// <summary>
/// Default empty instance of <see cref="DerivedData"/> used before initial layout solving.
/// </summary>
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

/// <summary>
/// Action types requiring user confirmation in modal dialogs before execution.
/// </summary>
type ConfirmAction =
    /// <summary> Reset workspace state to default template. </summary>
    | ResetWorkspace
    /// <summary> Load a predefined design preset. </summary>
    | LoadPreset of name: string * label: string
    /// <summary> Load a community gallery entry. </summary>
    | LoadGallery of name: string * rowId: string * author: string
    /// <summary> Switch editor tab view. </summary>
    | SwitchTo of EditorTab
    /// <summary> Reset site boundary geometry. </summary>
    | ResetBoundaryAction

/// <summary> Default published timestamp string for document metadata. </summary>
let PUBLISHED_DATE = "2022-08-15T00:00:00Z"

/// <summary> Default modified timestamp string for document metadata. </summary>
let MODIFIED_DATE = "2026-09-28T03:35:49Z"

/// <summary> Number of community gallery entries displayed per page. </summary>
let GALLERY_PAGE_SIZE = 12

/// <summary>
/// Export payload containing serialized boundary string representations, dimensions, entry points, and geographic metadata.
/// </summary>
type PolygonExportData = {
    /// <summary> Outer site boundary polygon string representation. </summary>
    OuterStr: string
    /// <summary> Island hole polygon string representation. </summary>
    IslandsStr: string
    /// <summary> Absolute position string representation. </summary>
    AbsStr: string
    /// <summary> Site entry coordinate string. </summary>
    EntryStr: string
    /// <summary> Base elevation index. </summary>
    Elevation: int
    /// <summary> Base boundary coordinate string. </summary>
    BaseStr: string
    /// <summary> Bounding box width. </summary>
    Width: int
    /// <summary> Bounding box height. </summary>
    Height: int
    /// <summary> Optional geographic latitude for solar analysis. </summary>
    Latitude: float option
    /// <summary> Physical map scaling factor (meters per unit). </summary>
    MapScale: float
}

/// <summary> Minimal snapshot of undoable editor state. </summary>
type UndoSnapshot = {
    /// <summary> Source-of-truth DSL text string. </summary>
    SrcOfTrth: string
    /// <summary> Hierarchical node tree state. </summary>
    Tree: SubModel
    /// <summary> Site polygon editor state wrapper. </summary>
    PolygonEditor: EditorState
    /// <summary> Level sequence operators map. </summary>
    Sequences: Map<int, string>
}

/// <summary>
/// Design metadata and classification tags captured during AI exploration and community submissions.
/// </summary>
type TeachMetadata = {
    /// <summary> Author name. </summary>
    Author: string
    /// <summary> Description or title of the exploration. </summary>
    ExplorationDescription: string
    /// <summary> Unique session identifier. </summary>
    SessionId: string
    /// <summary> Scale classification tag (e.g. Small, Medium, Large). </summary>
    Scale: string
    /// <summary> Typology classification tag (e.g. Residential, Commercial). </summary>
    Typology: string
    /// <summary> Flow classification tag. </summary>
    Flow: string
    /// <summary> Ambience classification tag. </summary>
    Ambience: string
    /// <summary> Design stage classification. </summary>
    Stage: string
    /// <summary> Architectural level shown in thumbnail preview. </summary>
    ThumbnailLevel: string
    /// <summary> Variation index shown in thumbnail preview. </summary>
    ThumbnailVariation: int
}

/// <summary>
/// Application workflow view state (loading screen, onboarding intro, or main workspace interface).
/// </summary>
type AppScreen =
    /// <summary> Initial asset loading screen. </summary>
    | LoadingScreen
    /// <summary> Onboarding introduction screen. </summary>
    | IntroScreen
    /// <summary> Main workspace design application screen. </summary>
    | MainScreen

// Batch Export Types

/// <summary> Renderable component shape data for batch export preview SVGs. </summary>
type BatchComponent = {| color: string; points: float[]; name: string; lx: float; ly: float |}

/// <summary> Complete layout variation configuration result produced during batch synthesis. </summary>
type BatchConfgrtns = {| sqnName: string; shapes: BatchComponent[]; wtmkShapes: BatchComponent[] option; w: float; h: float; mapScale: float; cxCxl1: Cxl[]; cxElv1: float[]; cxlAvl: int[]; cxOuIl: (int*int)[][]; cxAdj1: string[] * bool[][] ; cxB36: string[]; cxRto1: float[]; cxClr1: string[]; cxSol1: float[] option |}

// Report Types

/// <summary> Section visibility flags and variation selection filters for a specific level section in exported reports. </summary>
type LevelReportSections = {
    /// <summary> Whether to include the flowchart hierarchy section. </summary>
    FlowChart    : bool
    /// <summary> Whether to include the batch overview section. </summary>
    BatchOverview: bool
    /// <summary> Whether detailed variation breakdown sections are included. </summary>
    Variations   : bool
    /// <summary> Set of selected variation indices (0 to 23) included in detailed report. </summary>
    SelectedVariations: Set<int>
    /// <summary> Whether the variation filter drawer is expanded in UI. </summary>
    IsFilterExpanded: bool
}

/// <summary> Options and configuration for generating PDF and HTML architectural documentation reports. </summary>
type ReportOptions = {
    /// <summary> Project title for report headers. </summary>
    ProjectTitle  : string
    /// <summary> Author name for report headers. </summary>
    Author        : string
    /// <summary> Whether to generate a cover page. </summary>
    IncludeCover  : bool
    /// <summary> Section configurations keyed by level marker (e.g. "L0", "L1"). </summary>
    LevelSections : Map<string, LevelReportSections>
    /// <summary> Base64 data URL of captured 3D viewport canvas image. </summary>
    Captured3DImage: string option
}

/// <summary> In-memory cache mapping level markers to arrays of 24 batch layout configurations. </summary>
type LayoutCache = Map<string, BatchConfgrtns option []>

/// <summary> Metadata entry representing a published community design exploration in the gallery. </summary>
type GalleryEntry = {
    /// <summary> Unique identifier. </summary>
    Id: string
    /// <summary> Title/exploration description. </summary>
    ExplorationDescription: string
    /// <summary> Author name. </summary>
    Author: string
    /// <summary> Full design description. </summary>
    Description: string
    /// <summary> SVG thumbnail markup string. </summary>
    SvgThumbnail: string
    /// <summary> Number of architectural levels. </summary>
    LevelsCount: int
    /// <summary> Total space count across all levels. </summary>
    SpacesCount: int
    /// <summary> Architectural typology tag. </summary>
    Typology: string
    /// <summary> Scale classification tag. </summary>
    Scale: string
    /// <summary> Design stage tag. </summary>
    Stage: string
    /// <summary> Circulation flow classification. </summary>
    Flow: string
    /// <summary> Ambience classification tag. </summary>
    Ambience: string
    /// <summary> ISO timestamp string of creation date. </summary>
    CreatedAt: string
    /// <summary> Whether entry is highlighted as a featured design. </summary>
    IsFeatured: bool
}

/// <summary>
/// Main application state model for Hywe Elmish architecture.
/// </summary>
type Model =
    {
        /// <summary> Sequence operator strings per level index. </summary>
        Sequences: Map<int, string>
        /// <summary> Active architectural elevation level index. </summary>
        Elevation: int
        /// <summary> Base level string representation. </summary>
        BaseStr: string
        /// <summary> Canonical source-of-truth AST text string. </summary>
        SrcOfTrth : string
        /// <summary> Current hierarchical node tree. </summary>
        Tree : SubModel
        /// <summary> Last valid node tree before syntax parse errors. </summary>
        LastValidTree: SubModel
        /// <summary> Indicates if current text syntax contains parse errors. </summary>
        ParseError: bool
        /// <summary> Derived spatial layout calculations. </summary>
        Derived : DerivedData
        /// <summary> Multi-level variation layout cache. </summary>
        LayoutCache : LayoutCache
        /// <summary> Flag indicating layout solver recompilation is pending. </summary>
        NeedsHyweave: bool
        /// <summary> Flag indicating layout solver is actively running. </summary>
        IsHyweaving: bool
        /// <summary> Site polygon editor state wrapper. </summary>
        PolygonEditor: EditorState
        /// <summary> Currently active UI panel. </summary>
        ActivePanel: ActivePanel
        /// <summary> Active editor methodology mode. </summary>
        EditorMode: EditorMode
        /// <summary> Flag indicating batch synthesis cancellation is requested. </summary>
        IsCancelling: bool
        /// <summary> Cancellation token source for ongoing batch operations. </summary>
        CancelToken: System.Threading.CancellationTokenSource option
        /// <summary> Source-of-truth text used for last batch generation. </summary>
        LastBatchSrc: string option
        /// <summary> Currently selected variation index in batch grid. </summary>
        SelectedPreviewIndex : int option
        /// <summary> Data URL of captured 3D canvas rendering. </summary>
        Captured3DImage: string option
        /// <summary> User design description. </summary>
        UserDescription : string 
        /// <summary> Exploration metadata and classification tags. </summary>
        TeachMetadata: TeachMetadata
        /// <summary> Hover info tooltip message. </summary>
        HoveredInfo: string option
        /// <summary> Flag indicating save to Hynteract cloud service is in progress. </summary>
        IsSavingToHynteract : bool
        /// <summary> Flag indicating user feedback message display. </summary>
        ShowSuccessMessage : bool
        /// <summary> Error message from AI exploration service. </summary>
        TeachErrorMessage : string option
        /// <summary> Flag indicating active voice recording. </summary>
        IsRecording : bool
        /// <summary> Synchronized site boundary export payload. </summary>
        PolygonExport: PolygonExportData
        /// <summary> Number of variations successfully generated in the current batch. </summary>
        BatchProgress: int
        /// <summary> Current active screen view. </summary>
        CurrentScreen: AppScreen
        /// <summary> Flag indicating 3D camera orientation lock. </summary>
        ViewLocked: bool
        /// <summary> Options for PDF/HTML report generation. </summary>
        ReportOptions: ReportOptions
        /// <summary> Batch configuration results prepared for report rendering. </summary>
        ReportBatch: Map<int, BatchConfgrtns[]>
        /// <summary> Flag indicating report generation is in progress. </summary>
        IsGeneratingReport: bool
        /// <summary> Currently selected preset identifier. </summary>
        SelectedPreset: string option
        /// <summary> Total count of user modifications in current session. </summary>
        EditsCount: int
        /// <summary> Whether preset selection drawer is collapsed. </summary>
        IsPresetsCollapsed: bool
        /// <summary> Whether workspace panel drawer is collapsed. </summary>
        IsWorkspaceCollapsed: bool
        /// <summary> Pending action confirmation modal state. </summary>
        PendingConfirm: ConfirmAction option
        /// <summary> History stack of undoable snapshots. </summary>
        UndoStack: UndoSnapshot list
        /// <summary> History stack of redoable snapshots. </summary>
        RedoStack: UndoSnapshot list
        /// <summary> Pre-drag snapshot captured before boundary manipulation. </summary>
        PreDragSnapshot: UndoSnapshot option
        /// <summary> Whether PWA install prompt is available. </summary>
        InstallPromptAvailable: bool
        /// <summary> Whether privacy alert banner is displayed. </summary>
        ShowPrivacyAlert: bool
        /// <summary> Whether app is running in standalone PWA mode. </summary>
        IsStandalone: bool
        /// <summary> Whether coordinate overlay is visible. </summary>
        IsCoordsVisible: bool
        /// <summary> Whether "link copied" notification is displayed. </summary>
        ShowLinkCopied: bool
        /// <summary> Whether community gallery modal is open. </summary>
        ShowGallery: bool
        /// <summary> Whether about modal is open. </summary>
        ShowAboutModal: bool
        /// <summary> Flag indicating community gallery data is loading. </summary>
        IsLoadingGallery: bool
        /// <summary> Fetched gallery entries list. </summary>
        GalleryEntries: GalleryEntry list option
        /// <summary> Current offset index for gallery pagination. </summary>
        GalleryOffset: int
        /// <summary> Current search filter string for gallery entries. </summary>
        GalleryFilter: string
        /// <summary> Loaded community author name for derivative tracking. </summary>
        LoadedCommunityAuthor: string option
        /// <summary> Local user author name saved in local storage. </summary>
        CachedAuthor: string option
        /// <summary> Flag indicating modification suffix has been appended to project title. </summary>
        HasAppendedModSuffix: bool
        /// <summary> Active tutorial level module. </summary>
        TutorialLevel: TutorialLevel
        /// <summary> Current tutorial step index. None means tutorial is not active. </summary>
        TutorialStep: int option
        /// <summary> Whether auto step advance is active in the tutorial. </summary>
        TutorialAutoPlay: bool
        /// <summary> Preserved snapshot of user's pre-tutorial project state to restore on exit. </summary>
        PreTutorialSnapshot: UndoSnapshot option
    }

/// <summary> Metadata header used when generating downloadable CSV and SVG exports. </summary>
type ExportMetadata = {
    /// <summary> Project title. </summary>
    ProjectTitle: string
    /// <summary> Author name. </summary>
    Author: string
    /// <summary> ISO date string. </summary>
    Date: string
    /// <summary> Level marker string. </summary>
    Level: string
}

/// <summary> Helper utilities for preparing export metadata and reports. </summary>
module ExportHelpers =
    /// <summary> Creates an <see cref="ExportMetadata"/> snapshot from current application model settings. </summary>
    /// <param name="model">Current application model.</param>
    /// <param name="levelStr">Target level marker string.</param>
    /// <returns>Constructed export metadata record.</returns>
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

/// <summary> Messages representing all possible state changes in the main module. </summary>
type Message =
    /// <summary> Sets the active sequence operator index for the current architectural level. </summary>
    | SetSqnIndex of int
    /// <summary> Updates the source-of-truth DSL text string. </summary>
    | SetSrcOfTrth of string
    /// <summary> Sub-message delegated to hierarchical node tree updates. </summary>
    | TreeMsg of SubMsg
    /// <summary> Initiates the layout solver compilation pipeline. </summary>
    | StartHyweave
    /// <summary> Runs the layout solver computation asynchronously. </summary>
    | RunHyweave
    /// <summary> Concludes the layout solver computation. </summary>
    | FinishHyweave
    /// <summary> Callback handling layout solver completion with serialized syntax and layout cache. </summary>
    | HyweaveResult of src: string * cache: LayoutCache
    /// <summary> Callback delivering a single calculated variation layout configuration to cache. </summary>
    | CacheResult of marker: string * lvl: int * sqnIdx: int * data: BatchConfgrtns
    /// <summary> Sub-message delegated to polygon boundary editor actions. </summary>
    | PolygonEditorMsg of PolygonEditorMessage
    /// <summary> Callback delivering updated site polygon model state. </summary>
    | PolygonEditorUpdated of PolygonEditorModel
    /// <summary> Switches the active UI panel view. </summary>
    | SetActivePanel of ActivePanel
    /// <summary> Signals that multi-variation batch generation is complete. </summary>
    | SetBatchFinished
    /// <summary> Updates current batch generation progress count. </summary>
    | SetBatchProgress of int
    /// <summary> Triggers the generation of the next configuration in a batch sequence. </summary>
    | GenerateNextBatchItem of int
    /// <summary> Adds a completed configuration to the accumulator and proceeds to the next item. </summary>
    | AddBatchItem of LayoutCache
    /// <summary> Toggles between interactive flowchart editor and DSL text syntax mode. </summary>
    | ToggleEditorMode
    /// <summary> Toggles boundary overlay visibility in editor views. </summary>
    | ToggleBoundary
    /// <summary> Requests PDF report export. </summary>
    | ExportPdfRequested
    /// <summary> Triggers download of coordinate metrics as CSV. </summary>
    | DownloadCoordCsv
    /// <summary> Triggers download of spatial metrics as CSV. </summary>
    | DownloadMetricsCsv
    /// <summary> Triggers download of adjacency matrix as CSV. </summary>
    | DownloadAdjCsv
    /// <summary> Triggers download of batch coordinate metrics as CSV. </summary>
    | DownloadBatchCoordCsv
    /// <summary> Triggers download of batch spatial metrics as CSV. </summary>
    | DownloadBatchMetricsCsv
    /// <summary> Triggers download of batch adjacency matrix as CSV. </summary>
    | DownloadBatchAdjCsv
    /// <summary> Triggers download of batch layout variations as SVG vector files. </summary>
    | DownloadBatchSvg
    /// <summary> Triggers download of batch layout variations as PNG image files. </summary>
    | DownloadBatchPng
    /// <summary> Triggers download of 3D volume model as SVG. </summary>
    | Download3DSvg
    /// <summary> Toggles selection of a specific variation preview index in batch panel. </summary>
    | TapBatchPreview of int
    /// <summary> Closes the batch exploration panel. </summary>
    | CloseBatch
    /// <summary> Requests cancellation of ongoing batch synthesis. </summary>
    | CancelBatch
    /// <summary> Callback handling batch synthesis cancellation. </summary>
    | BatchCancelled
    /// <summary> Triggers project save to file and local storage backup. </summary>
    | SaveRequested
    /// <summary> Triggers file import picker dialog. </summary>
    | ImportRequested
    /// <summary> Handles imported Hywe file DSL content string. </summary>
    | FileImported of string
    /// <summary> Sets the user exploration description text. </summary>
    | SetDescription of string
    /// <summary> Requests AI suggestion for exploration description based on geometry. </summary>
    | SuggestDescription
    /// <summary> Updates teach metadata properties using a transformer function. </summary>
    | UpdateMetadata of (TeachMetadata -> TeachMetadata)
    /// <summary> Sets active hover info tooltip text. </summary>
    | SetHoveredInfo of string option
    /// <summary> Records exploration dataset to Hynteract cloud service. </summary>
    | RecordToHynteract
    /// <summary> Callback delivering result of Hynteract dataset recording. </summary>
    | RecordResult of success: bool * errorMsg: string option * cache: LayoutCache
    /// <summary> Starts voice audio recording for voice annotation. </summary>
    | StartVoiceCapture
    /// <summary> Handles voice speech-to-text recognition result. </summary>
    | OnVoiceResult
    /// <summary> Toggles collapsed state of design preset selection drawer. </summary>
    | TogglePresetsCollapse
    /// <summary> Toggles collapsed state of workspace panel drawer. </summary>
    | ToggleWorkspaceCollapse
    /// <summary> Navigates to onboarding intro screen. </summary>
    | TransitionToIntro
    /// <summary> Navigates to main workspace screen. </summary>
    | TransitionToMain
    /// <summary> Toggles 3D camera orientation lock. </summary>
    | ToggleViewLock
    /// <summary> Triggers download of 3D rendering canvas as PNG. </summary>
    | Download3DPng
    /// <summary> Updates report generation options using a transformer function. </summary>
    | UpdateReportOptions of (ReportOptions -> ReportOptions)
    /// <summary> Triggers report document generation. </summary>
    | GenerateReport
    /// <summary> Callback delivering generated report HTML markup string. </summary>
    | ReportGenerated of html: string * cache: LayoutCache
    /// <summary> Captures base64 data URL of 3D canvas view. </summary>
    | ViewCaptured of string
    /// <summary> Selects and applies a predefined design preset. </summary>
    | SelectPreset of string
    /// <summary> Restores application model state from content string, target panel, and URL source flag. </summary>
    | LoadState of content: string * panel: ActivePanel option * isFromUrl: bool
    /// <summary> Generates and copies compressed URL link for sharing current design. </summary>
    | ShareLink
    /// <summary> Hides "link copied" notification popup. </summary>
    | HideLinkCopied
    /// <summary> Resets application state to empty template and clears local storage backups. </summary>
    | HardReset
    /// <summary> Displays or dismisses confirmation modal dialog for a specified action. </summary>
    | ToggleConfirm of ConfirmAction option
    /// <summary> Performs undo operation to restore previous state snapshot. </summary>
    | Undo
    /// <summary> Performs redo operation to re-apply undone state snapshot. </summary>
    | Redo
    /// <summary> Sets PWA install prompt availability status. </summary>
    | SetInstallPromptAvailable of bool
    /// <summary> Triggers PWA installation prompt dialog. </summary>
    | InstallRequested
    /// <summary> Toggles privacy alert banner visibility. </summary>
    | SetPrivacyAlert of bool
    /// <summary> Sets standalone PWA mode status flag. </summary>
    | SetIsStandalone of bool
    /// <summary> Toggles coordinate grid overlay visibility. </summary>
    | ToggleCoords
    /// <summary> Toggles community gallery modal visibility. </summary>
    | ToggleGallery
    /// <summary> Toggles about dialog modal visibility. </summary>
    | ToggleAboutModal
    /// <summary> Sets about dialog modal visibility directly. </summary>
    | SetShowAboutModal of bool
    /// <summary> Triggers asynchronous loading of community gallery entries. </summary>
    | LoadGalleryEntries
    /// <summary> Navigates to next page in community gallery view. </summary>
    | NextGalleryPage
    /// <summary> Navigates to previous page in community gallery view. </summary>
    | PrevGalleryPage
    /// <summary> Navigates to specified page number in community gallery view. </summary>
    | GoToGalleryPage of page: int
    /// <summary> Callback delivering loaded community gallery entry list. </summary>
    | GalleryEntriesLoaded of GalleryEntry list
    /// <summary> Requests fetching design definition DSL string for a gallery item. </summary>
    | LoadGalleryDefinition of name: string * rowId: string * author: string
    /// <summary> Callback delivering design definition DSL string for a gallery item. </summary>
    | LoadGalleryDefinitionSuccess of name: string * definition: string * author: string
    /// <summary> Updates search filter query string for community gallery. </summary>
    | UpdateGalleryFilter of string
    /// <summary> Sets local user author name. </summary>
    | SetAuthor of string
    /// <summary> Sets exploration project title. </summary>
    | SetExplorationTitle of string
    /// <summary> Callback delivering cached author name retrieved from local storage. </summary>
    | AuthorCachedLoaded of string
    /// <summary> Callback delivering cached project title retrieved from local storage. </summary>
    | TitleCachedLoaded of string
    /// <summary> Callback delivering community author name retrieved from local storage. </summary>
    | CommunityAuthorCachedLoaded of string
    /// <summary> Sets active tutorial difficulty level module. </summary>
    | SetTutorialLevel of TutorialLevel
    /// <summary> Advances to next step in active tutorial. </summary>
    | TutorialNext
    /// <summary> Returns to previous step in active tutorial. </summary>
    | TutorialBack
    /// <summary> Dismisses and exits active tutorial. </summary>
    | DismissTutorial
    /// <summary> Toggles auto step advance timer in tutorial mode. </summary>
    | ToggleTutorialAutoPlay
    /// <summary> Callback triggered when tutorial auto-advance timer expires for expected step. </summary>
    | TutorialAutoAdvance of step: int
    /// <summary> No-operation message payload. </summary>
    | NoOp

/// <summary>
/// Synchronizes the site polygon editor model state into a pure <see cref="PolygonExportData"/> cache.
/// </summary>
/// <param name="p">Site polygon editor model.</param>
/// <returns>Synchronized polygon export data record.</returns>
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

