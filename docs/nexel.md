---
title: Nexel (Nesting)
category: Modules
categoryindex: 2
index: 6
---

# Nexel (Nesting)

`Nexel` provides recursive spatial nesting, allowing architectural sub-programmes to be nested inside parent room boundaries.

---

## Functions

* **`generateNestLayout : NestBlock -> Coxel.Cxl -> float -> Coxel.Cxl[] -> string option -> Coxel.Cxl[] * (int * int * int * int) * float`**  
  Synthesizes an internal sub-layout within the boundary footprint of a host room.
* **`isNestMarker : string -> bool`**  
  Determines whether a block header identifier corresponds to an interior nest.
* **`getNestIndex : string -> int option`**  
  Extracts the numerical nest index from a marker string.
