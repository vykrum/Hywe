module ModelTypes

open Layout
open Page
open NodeCode
open PolygonEditor

type PolygonExportData = {
    OuterStr: string
    IslandsStr: string
    AbsStr: string
    EntryStr: string
    Width: int
    Height: int
}

/// <summary> Central application state for the interface. </summary>
type Model =
    {
        Sequence: string
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
        UserDescription : string 
        IsSavingToHynteract : bool
        ShowSuccessMessage : bool
        IsRecording : bool
        PolygonExport: PolygonExportData
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
    | ToggleEditorMode
    | ToggleBoundary
    | ExportPdfRequested
    | TapBatchPreview of int
    | CloseBatch
    | CancelBatch
    | BatchCancelled
    | SaveRequested
    | ImportRequested
    | FileImported of string
    | SetDescription of string
    | RecordToHynteract
    | RecordResult of bool
    | StartVoiceCapture
    | OnVoiceResult of string

/// <summary> Synchronizes the PolygonEditor state to pure data cache. </summary>
let syncPolygonState (p: PolygonEditorModel) =
    let outer, islands, absolute, entry, w, h = PolygonEditor.exportPolygonStrings p
    
    let w', h', entry', outer', islands' =
        match p.UseBoundary with
        | false -> 0, 0, "0,0", "", ""
        | true  -> w, h, entry, outer, islands
        
    { OuterStr = outer'; IslandsStr = islands'; AbsStr = absolute; EntryStr = entry'; Width = w'; Height = h' }

