[<AutoOpen>]
module Types

open System
open Microsoft.AspNetCore.Components.Web

// ---------- Types ----------

/// <summary> 2D point with Cartesian coordinates. </summary>
type Point = { X: float; Y: float }

/// <summary>
/// Identifies a targeted vertex by polygon index (0 for outer, >0 for islands)
/// and vertex index within that polygon.
/// </summary>
type DragInfo = { PolyIndex: int; VertexIndex: int }

/// <summary>
/// Candidate projection point along a polygon edge for vertex insertion.
/// </summary>
type GhostCandidate = { PolyIndex: int; EdgeIndex: int; Point: Point }

/// <summary>
/// Cached SVG viewBox and client rect metrics for coordinate mapping.
/// </summary>
type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

/// <summary>
/// State model for the polygon boundary editor.
/// Uses arrays for fast random access and shallow copies.
/// </summary>
type PolygonEditorModel =
    {
        UseBoundary: bool
        UseAbsolute: bool
        PolygonEnabled: bool
        UseMapBase: bool
        IsMapLocked: bool
        TopographyData: string option
        LogicalWidth: float
        LogicalHeight: float
        Elevation: int
        BaseStr: string
        Outer: Point[]
        Islands: Point[][]
        VertexRadius: int
        Dragging: DragInfo option
        DraggingIsland: int option
        DragOffset: Point option      // offset between pointer svg point and vertex/shape so dragging doesn't jump
        SvgInfo: SvgInfo option       // cached transform info so we don't call JS on every mousemove
        LastMoveMs: float option      // for simple throttling
        EntryPoint: Point
        DraggingEntry: bool
        SelectedVertex: DragInfo option
        GhostVertex: GhostCandidate option
        OuterPointsStr: string        // Cached for performance
        IslandPointsStrs: string[]    // Cached for performance
        DisplayWidth: float
        DisplayHeight: float
        DisplayOuter: Point[]
        DisplayIslands: Point[][]
        MapScale: float
        ShowInstructions: bool
        IsLocked: bool
    }

/// <summary>
/// Editor lifecycle state distinguishing standard editing from fresh syntax imports.
/// </summary>
type EditorState =
    | Stable of PolygonEditorModel
    | FreshlyImported of PolygonEditorModel

/// <summary>
/// Messages representing user interactions and state changes in the polygon editor.
/// </summary>
type PolygonEditorMessage =
    | ToggleBoundary of bool
    | ToggleAbsolute of bool
    | ToggleMapBase of bool
    | ToggleMapLock of bool
    | ToggleInstructions
    | ToggleLock
    | MapTopographyReceived of float * float * string
    | UpdateLogicalWidth of float
    | UpdateLogicalHeight of float
    | UpdateLogicalDimensions of float * float
    | PointerDown of MouseEventArgs
    | PointerUp
    | PointerMove of MouseEventArgs
    | DoubleClick of MouseEventArgs
    | SelectVertex of DragInfo option
    | DeleteSelectedVertex
    | KeyDown of KeyboardEventArgs
    | RemoveVertex of int * int
    | CommitGhostVertex
    | StartDragEntry of MouseEventArgs
    | MoveDragEntry of MouseEventArgs
    | EndDragEntry
    | ResetBoundary
    | RequestResetBoundary
    | UndoBoundary
    | RedoBoundary
    | ImportFromSyntax of string * string * string * string * int * int
