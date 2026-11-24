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
* **`generateCxlLayout : LayoutContext -> (Coxel.Cxl * Hxl[]) option -> Hxl[] -> Coxel.Cxl[] * (int * int * int * int) * float`**  
  Executes full planar layout compilation, returning resolved room clusters, bounding box, and scaling ratio.
* **`applyReproportioning : int64 -> LayoutTree -> float option -> LayoutTree * float`**  
  Proportionally scales program target areas to fit within the physical net area of site boundaries.
* **`calculateTargetRatio : Map<string, string> -> LayoutTree -> LayoutOptions -> float`**  
  Computes the optimal packing scale ratio.
* **`tryParseUnion<'T> : string -> 'T option`**  
  Generic reflection helper parsing string representation into strongly-typed discriminated union cases.
