module ModelTypes


open Hywe.Node

open Hywe.Site
open Hywe.Site.State
open Hywe.Site.View
open Hywe.Core.Coxel
open System



/// <summary> Specifies the currently visible configuration panel. </summary>
type ActivePanel =
    | BoundaryPanel
    | LayoutPanel
    | AnalyzePanel
    | ViewPanel
    | BatchPanel
    | TeachPanel
    | ReportPanel

/// <summary> Specifies the input methodology - flowchart or text </summary>
type EditorMode =
    | Interactive
    | Syntax

type EditorTab =
    | Boundary
    | Editor of isAdvanced: bool

type DerivedData = {
    cxCxl1: Cxl[]
    cxlAvl: int[]
    cxClr1: string[]
    cxOuIl: (int*int)[][]
    cxElv1: float[]
    cxRto1: float[]
    cxAdj1: string[] * bool[][]
    cxB36: string[]
}

// --- Constants & Helpers ---

let labelPhrase = "alternATE◦CONFIGURATions"

let indexToLabel (i: int) =
    if i < 0 then "a"
    elif i < 24 then string labelPhrase.[i]
    else
        let first = labelPhrase.[(i / 24) - 1]
        let second = labelPhrase.[i % 24]
        $"{first}{second}"

// Consistent Pastel Color
let hexToRgb (hex: string) =
    let hex = hex.TrimStart('#')
    let r = Convert.ToInt32(hex.Substring(0, 2), 16)
    let g = Convert.ToInt32(hex.Substring(2, 2), 16)
    let b = Convert.ToInt32(hex.Substring(4, 2), 16)
    (r, g, b)

let private clamp value = min 255 (max 0 value)

/// Deterministic pastel generator — first color is base color
let generatePastels (rootHex: string) (count: int) (opacity: float) : string[] =
    let (baseR, baseG, baseB) = hexToRgb rootHex

    [| 0 .. count - 1 |]
    |> Array.map (fun i ->
        match i = 0 with
        | true ->
            $"rgba({baseR}, {baseG}, {baseB}, {opacity})"
        | false ->
            let hueShift = (i * 137) % 360
            let angleRad = float hueShift * Math.PI / 180.0

            let vary cmpnent phase =
                let offset = int (40.0 * Math.Sin(angleRad + phase))
                clamp ((cmpnent + offset + 255) >>> 1)

            let r = vary baseR 0.0
            let g = vary baseG 2.0
            let b = vary baseB 4.0

            $"rgba({r}, {g}, {b}, {opacity})"
    )

let deriveDataFromLayout (cxCxl1: Cxl[]) (cxOuIl: (int*int)[][]) (cxElv1: float[]) (cxRto1: float[]) (elv: int) : DerivedData =
    let fallbackSqn = Hywe.Core.Hexel.VRCCNE 
    let activeSqn = 
        cxCxl1 
        |> Array.tryFind (fun c -> let (_, _, z) = Hywe.Core.Hexel.hxlCrd c.Base in z = elv)
        |> Option.map (fun c -> c.Seqn)
        |> Option.defaultValue (if Array.isEmpty cxCxl1 then fallbackSqn else (Array.head cxCxl1).Seqn)
    
    let cxlAvl = if Array.isEmpty cxCxl1 then [||] else Hywe.Core.Coxel.cxlExp cxCxl1 activeSqn elv
    
    // Deterministic coloring based on architectural ID (Rfid) to ensure consistency across levels
    let uniqueRfids = cxCxl1 |> Array.map (fun c -> Hywe.Core.Coxel.prpVlu c.Rfid) |> Array.distinct
    let colorMap = 
        generatePastels "#888888" (max 1 (Array.length uniqueRfids)) 0.85
        |> Array.zip uniqueRfids
        |> Map.ofArray

    let cxClr1 = cxCxl1 |> Array.map (fun c -> colorMap.[Hywe.Core.Coxel.prpVlu c.Rfid])
    let cxAdj1 = Hywe.Core.Coxel.cxlAdj cxCxl1
    let cxB36 = cxCxl1 |> Array.map Hywe.Core.Coxel.getCxlCoordsString

    {
        cxCxl1 = cxCxl1
        cxlAvl = cxlAvl
        cxClr1 = cxClr1
        cxOuIl = cxOuIl
        cxElv1 = cxElv1
        cxRto1 = cxRto1
        cxAdj1 = cxAdj1
        cxB36 = cxB36
    }

type ConfirmAction =
    | ResetWorkspace
    | LoadPreset of name: string * label: string
    | SwitchTo of EditorTab



let elv = 0

let PUBLISHED_DATE = "2022-08-15T00:00:00Z"
let MODIFIED_DATE = "2026-05-07T00:00:00Z"

type PolygonExportData = {
    OuterStr: string
    IslandsStr: string
    AbsStr: string
    EntryStr: string
    Elevation: int
    BaseStr: string
    Width: int
    Height: int
}

type OnboardingStep =
    | Welcome
    | BoundaryGuide
    | NodeGuide
    | NodeMenuGuide
    | ElevateGuide
    | MoveNodeGuide
    | LayoutGuide
    | Finish

type OnboardingState = {
    IsActive: bool
    IsAutoSimulating: bool
    CurrentStep: OnboardingStep
    SeenSteps: Set<OnboardingStep>
}

/// <summary> Minimal snapshot of undoable editor state. </summary>
type UndoSnapshot = {
    SrcOfTrth: string
    Tree: SubModel
    PolygonEditor: EditorState
    Sequences: Map<int, string>
}

type TeachMetadata = {
    Author: string
    ProjectTitle: string
    SessionId: string
    Scale: string
    Typology: string
    Flow: string
    Ambience: string
    Stage: string
}

type AppScreen =
    | LoadingScreen
    | IntroScreen
    | MainScreen

// Batch Export Types
type BatchComponent = {| color: string; points: float[]; name: string; lx: float; ly: float |}
type BatchConfgrtns = {| sqnName: string; shapes: BatchComponent[]; w: float; h: float; cxCxl1: Cxl[]; cxElv1: float[]; cxlAvl: int[]; cxOuIl: (int*int)[][]; cxAdj1: string[] * bool[][] ; cxB36: string[]; cxRto1: float[]; cxClr1: string[] |}

// Report Types
type LevelReportSections = {
    FlowChart    : bool
    BatchOverview: bool
    Variations   : bool   // detailed section toggle
    SelectedVariations: Set<int> // which of the 24 are included
    IsFilterExpanded: bool
}

type ReportOptions = {
    ProjectTitle  : string
    ProjectNumber : string
    Author        : string
    ClientName    : string
    Description   : string
    IncludeCover  : bool
    LevelSections : Map<string, LevelReportSections>
    Captured3DImage: string option
}

type LayoutCache = Map<string, BatchConfgrtns option []>

/// <summary> Central application state for the interface. </summary>
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
        IsRecording : bool
        PolygonExport: PolygonExportData
        Onboarding: OnboardingState
        /// <summary> Number of variations successfully generated in the current batch. </summary>
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
        IsHelpCollapsed: bool
        PendingConfirm: ConfirmAction option
        UndoStack: UndoSnapshot list
        RedoStack: UndoSnapshot list
        InstallPromptAvailable: bool
        ShowPrivacyAlert: bool
        IsStandalone: bool
        IsCoordsVisible: bool
        ShowLinkCopied: bool
    }

/// <summary> Messages representing all possible state changes in the main module. </summary>
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
    /// <summary> Triggers the generation of the next configuration in a batch sequence. </summary>
    | GenerateNextBatchItem of int
    /// <summary> Adds a completed configuration to the accumulator and proceeds to the next item. </summary>
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
    | DownloadDxf
    | DownloadObj
    | DownloadBatchDxf
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
    | RecordResult of success: bool * cache: LayoutCache
    | StartVoiceCapture
    | OnVoiceResult
    | NextOnboardingStep
    | PreviousOnboardingStep
    | SkipOnboarding
    | RestartOnboarding
    | TogglePresetsCollapse
    | ToggleWorkspaceCollapse
    | ToggleHelpCollapse
    | StartAutoSimulation
    | StopAutoSimulation
    | TransitionToIntro
    | TransitionToMain
    | ToggleViewLock
    | Download3DSvg
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
    | NoOp

/// <summary> Synchronizes the PolygonEditor state to pure data cache. </summary>
let syncPolygonState (p: PolygonEditorModel) =
    let outer, islands, absolute, entry, w, h, elv, baseS = State.exportPolygonStrings p
    
    let w', h', entry', outer', islands' =
        match p.UseBoundary with
        | false -> w, h, "0,0", "", ""
        | true  -> w, h, entry, outer, islands
        
    { Elevation = elv; BaseStr = baseS; OuterStr = outer'; IslandsStr = islands'; AbsStr = absolute; EntryStr = entry'; Width = w'; Height = h' }
