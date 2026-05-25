namespace Hywe.Site

open System
open Microsoft.AspNetCore.Components.Web

// ---------- Types ----------
type Point = { X: float; Y: float }
type DragInfo = { PolyIndex: int; VertexIndex: int }

type SvgInfo =
    { ViewBoxX: float; ViewBoxY: float; ViewBoxW: float; ViewBoxH: float
      ClientLeft: float; ClientTop: float; ClientW: float; ClientH: float }

// Use arrays for fast random access and cheap shallow copies
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
        DragOffset: Point option      // offset between pointer svg point and vertex so dragging doesn't jump
        SvgInfo: SvgInfo option       // cached transform info so we don't call JS on every mousemove
        LastMoveMs: float option      // for simple throttling
        EntryPoint: Point
        DraggingEntry: bool
        OuterPointsStr: string        // Cached for performance
        IslandPointsStrs: string[]    // Cached for performance
        DisplayWidth: float
        DisplayHeight: float
        DisplayOuter: Point[]
        DisplayIslands: Point[][]
        MapScale: float
    }

type EditorState =
    | Stable of PolygonEditorModel
    | FreshlyImported of PolygonEditorModel

type PolygonEditorMessage =
    | ToggleBoundary of bool
    | ToggleAbsolute of bool
    | ToggleMapBase of bool
    | ToggleMapLock of bool
    | MapTopographyReceived of float * float * string
    | UpdateLogicalWidth of float
    | UpdateLogicalHeight of float
    | UpdateLogicalDimensions of float * float
    | PointerDown of MouseEventArgs
    | PointerUp
    | PointerMove of MouseEventArgs
    | DoubleClick of MouseEventArgs
    | RemoveVertex of int * int
    | StartDragEntry of MouseEventArgs
    | MoveDragEntry of MouseEventArgs
    | EndDragEntry
    | ImportFromSyntax of string * string * string * string * int * int
