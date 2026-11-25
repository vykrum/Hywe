(**
---
title: Xyxel (Layout)
category: Modules
categoryindex: 2
index: 5
---

# Xyxel (Layout)

`Xyxel` handles planar layout planning, boundary fitting, and 2D spatial arrangement.

---

## Types

* **`LayoutTree`**: Scaled representation of the architectural program tree with total requested area.
* **`LayoutOptions`**: Configuration options for layout synthesis (elevation, boundary overrides, entry points).
* **`LayoutContext`**: Complete resolved context containing boundary constraints, keep-out islands, and scaled node trees.

---

## Functions

* **`prepareLayoutContext : Map<string, string> -> LayoutTree -> LayoutOptions -> LayoutContext`**  
  Prepares site boundary polylines, voids, and scaled program trees into an executable layout synthesis context.
* **`generateBaseCxl : LayoutContext -> (Coxel.Cxl * Hxl[]) option`**  
  Synthesizes the primary anchor root room and establishes initial boundary occupancy obstacles.
* **`generateCxlLayout : LayoutContext -> Coxel.Cxl -> Hxl[] -> Coxel.Cxl[] * (int * int)[][] * float`**  
  Executes full planar layout compilation, returning resolved room clusters, boundary polygons, and scaling ratio.
* **`applyReproportioning : int64 -> LayoutTree -> float option -> (Prp * Prp * Prp)[][] * float`**  
  Proportionally scales program target areas to fit within the physical net area of site boundaries.
* **`calculateTargetRatio : Map<string, string> -> LayoutTree -> LayoutOptions -> float`**  
  Computes the optimal packing scale ratio.
* **`tryParseUnion<'T> : string -> 'T option`**  
  Generic reflection helper parsing string representation into strongly-typed discriminated union cases.

---

## Usage Examples

### 1. Preparing Layout Context and Synthesizing Floor Plans
*)
(*** hide ***)
#r "../src/Hywe.Core/bin/Debug/net10.0/Hywe.Core.dll"
(** *)
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

// Define architectural room tree: (Id, TargetArea, Label)[][]
let rawTree = [|
    [| ("1", 25, "Living"); ("1.1", 10, "Dining") |]
    [| ("2", 15, "Kitchen") |]
|]
let layoutTree = Xyxel.LayoutTree.Create rawTree

let options : Xyxel.LayoutOptions = {
    EntryFallback = "0,0"
    InitialOcc    = [||]
    Seq           = Some Hexel.Sqn.VRCWEE
    Width         = Some 40
    Height        = Some 30
    OuterStr      = None
    IslandsStr    = None
    ParentCxl     = None
    Ratio         = None
    Elevation     = Some 0
}

let context = Xyxel.prepareLayoutContext Map.empty layoutTree options

// Generate the anchor base room and compile full planar room layout
match Xyxel.generateBaseCxl context with
| Some (rootCxl, nextOcc) ->
    let rooms, bounds, appliedRatio = Xyxel.generateCxlLayout context rootCxl nextOcc
    printfn "Generated %d rooms with packing ratio %f" rooms.Length appliedRatio
| None ->
    printfn "Could not place anchor base room"
(*** include-output ***)

(**
### 2. Reflection-Based Union Parsing
*)
// Safely parse sequence operator from string
match Xyxel.tryParseUnion<Hexel.Sqn> "VRCWEE" with
| Some sqn -> printfn "Parsed valid sequence: %A" sqn
| None     -> printfn "Unknown sequence operator"
(*** include-output ***)
