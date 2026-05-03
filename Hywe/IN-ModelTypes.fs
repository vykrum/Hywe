module ModelTypes

open NodeCode
open Page
open PolygonEditor
open Coxel

type ConfirmAction =
    | ResetWorkspace
    | LoadPreset of name: string * label: string

let elv = 0

let PUBLISHED_DATE = "2022-08-15T00:00:00Z"
let MODIFIED_DATE = "2026-04-27T00:00:00Z"

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
    Sequence: string
}

type TeachMetadata = {
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
type BatchConfgrtns = {| sqnName: string; shapes: BatchComponent[]; w: float; h: float; cxCxl1: Cxl[]; cxElv1: float[]; cxlAvl: int[]; cxOuIl: (float*float)[][]; cxAdj1: string[] * bool[][]; cxB36: string[] |}

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
    LevelSections : Map<int, LevelReportSections>
    Captured3DImage: string option
}

/// <summary> Central application state for the interface. </summary>
type Model =
    {
        Sequence: string
        Elevation: int
        BaseStr: string
        SrcOfTrth : string
        Tree : SubModel
        LastValidTree: SubModel
        ParseError: bool
        Derived : DerivedData
        NeedsHyweave: bool
        IsHyweaving: bool
        PolygonEditor: EditorState
        ActivePanel: ActivePanel
        EditorMode: EditorMode
        BatchPreview: BatchConfgrtns[] option
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
        /// <summary> Temporary storage for configurations as they are generated recursively. </summary>
        BatchAccumulator: BatchConfgrtns list
        CurrentScreen: AppScreen
        ViewLocked: bool
        ReportOptions: ReportOptions
        ReportBatch: Map<int, BatchConfgrtns[]>
        IsGeneratingReport: bool
        SelectedPreset: string option
        EditsCount: int
        IsPresetsCollapsed: bool
        IsHelpCollapsed: bool
        PendingConfirm: ConfirmAction option
        UndoStack: UndoSnapshot list
        RedoStack: UndoSnapshot list
        InstallPromptAvailable: bool
        ShowPrivacyAlert: bool
        IsStandalone: bool
    }

/// <summary> Messages representing all possible state changes in the main module. </summary>
type Message =
    | SetSqnIndex of int
    | SetSrcOfTrth of string
    | TreeMsg of SubMsg
    | StartHyweave
    | RunHyweave
    | FinishHyweave
    | PolygonEditorMsg of PolygonEditorMessage
    | PolygonEditorUpdated of PolygonEditorModel
    | SetActivePanel of ActivePanel
    | SetBatchPreview of BatchConfgrtns[]
    | SetBatchProgress of int
    /// <summary> Triggers the generation of the next configuration in a batch sequence. </summary>
    | GenerateNextBatchItem of int
    /// <summary> Adds a completed configuration to the accumulator and proceeds to the next item. </summary>
    | AddBatchItem of BatchConfgrtns option
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
    | DownloadBatchObj
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
    | RecordResult of bool
    | StartVoiceCapture
    | OnVoiceResult
    | NextOnboardingStep
    | PreviousOnboardingStep
    | SkipOnboarding
    | RestartOnboarding
    | TogglePresetsCollapse
    | ToggleHelpCollapse
    | StartAutoSimulation
    | StopAutoSimulation
    | TransitionToIntro
    | TransitionToMain
    | ToggleViewLock
    | Download3DSvg
    | UpdateReportOptions of (ReportOptions -> ReportOptions)
    | GenerateReport
    | ReportGenerated of string
    | ViewCaptured of string
    | SelectPreset of string
    | LoadBackup of string
    | HardReset
    | ToggleConfirm of ConfirmAction option
    | Undo
    | Redo
    | SetInstallPromptAvailable of bool
    | InstallRequested
    | SetPrivacyAlert of bool
    | SetIsStandalone of bool
    | NoOp

/// <summary> Synchronizes the PolygonEditor state to pure data cache. </summary>
let syncPolygonState (p: PolygonEditorModel) =
    let outer, islands, absolute, entry, w, h, elv, baseS = PolygonEditor.exportPolygonStrings p
    
    let w', h', entry', outer', islands' =
        match p.UseBoundary with
        | false -> w, h, "0,0", "", ""
        | true  -> w, h, entry, outer, islands
        
    { Elevation = elv; BaseStr = baseS; OuterStr = outer'; IslandsStr = islands'; AbsStr = absolute; EntryStr = entry'; Width = w'; Height = h' }
