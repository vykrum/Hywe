(**
---
title: Zaxel (3D Stacking)
category: Modules
categoryindex: 2
index: 7
---

# Zaxel (3D Stacking)

`Zaxel` manages multi-level vertical aggregation, floor-to-floor stacking, and 3D volumetric massing data structures.

---

## Types

* **`BuildState`**: State accumulator for multi-level buildings: `{ Cxls: Cxl[]; Bounds: (int * int)[][][]; Elvs: float[]; Ratio: float option; ... }`.

---

## Functions

* **`generateMultiLevelLayout : string -> string -> Hxl[] -> (int * Sqn) option -> string option -> string option -> Coxel.Cxl[] * (int * int)[][][] * float[] * float[]`**  
  Parses a complete multi-level `.hyw` architectural dossier, resolves all floor levels and nested spaces, and outputs the volumetric building rooms, boundary polygons, elevations, and scaling ratios.

---

## Usage Examples

### 1. Generating a Multi-Level Architectural Building

Synthesize a complete multi-level building layout from a compact `.hyw` program specification:
*)
(*** hide ***)
#r "../src/Hywe.Core/bin/Debug/net10.0/Hywe.Core.dll"
(** *)
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Zaxel

// Multi-level specification defining ground floor L0 and upper floor L1
let hyweCode = 
    "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
    "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

// Synthesize multi-level massing with default entry at origin (0,0) and no obstacles
let rooms, bounds, elvs, ratios = 
    Zaxel.generateMultiLevelLayout 
        hyweCode 
        "0,0" 
        [||] 
        None 
        None 
        None

let roomNames =
    rooms
    |> Array.map (fun room -> Coxel.prpVlu room.Name)

printfn "Synthesized %d rooms across %d levels: %s" roomNames.Length elvs.Length (String.concat ", " roomNames)
(*** include-output ***)
