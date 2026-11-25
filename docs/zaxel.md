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

* **`BuildState`**: State accumulator for multi-level buildings: `{ Cxls: Cxl[]; Bounds: (int * int * int * int)[]; Elvs: (int * int)[]; Ratio: float; ... }`.

---

## Functions

* **`generateMultiLevelLayout : string -> string -> Hxl[] -> (int * Sqn) option -> string option -> string option -> BuildState`**  
  Parses a complete multi-level `.hyw` architectural dossier, resolves all floor levels and nested spaces, and outputs the volumetric building state.

---

## Usage Examples

### 1. Generating a Multi-Level Architectural Building

Synthesize a complete multi-level building layout from a compact `.hyw` program specification:

```fsharp
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Zaxel

// Multi-level specification defining ground floor L0 and upper floor L1
let hyweCode = 
    "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
    "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

// Synthesize multi-level massing with default entry at origin (0,0) and no obstacles
let buildState = 
    Zaxel.generateMultiLevelLayout 
        hyweCode 
        "0,0" 
        [||] 
        None 
        None 
        None

printfn "Synthesized building contains %d total rooms across levels" buildState.Cxls.Length
for room in buildState.Cxls do
    printfn "Room %s at elevation" (Coxel.prpVlu room.Name)
```

