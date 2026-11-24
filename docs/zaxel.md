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
