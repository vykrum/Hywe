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

---

## Usage Examples

### 1. Inspecting Nested Sub-Layout Markers

```fsharp
open Hywe.Core
open Hywe.Core.Nexel

let marker1 = "N1"
let marker2 = "L0"

let isN1 = Nexel.isNestMarker marker1 // true
let isL0 = Nexel.isNestMarker marker2 // false

let nestIdx = Nexel.getNestIndex marker1 // Some 1
```

### 2. Synthesizing Nested Sub-Rooms Inside a Host Room

```fsharp
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Lexel
open Hywe.Core.Nexel

// Assume hostRoom was generated as a parent room (e.g. Master Bedroom)
// and nestBlock was parsed from "N1(Q=VRCWEE)(1/6/WalkInCloset)(2/8/MasterBath)"
let nestBlock : Lexel.NestBlock = {
    Marker = "N1"
    Attributes = Lexel.parseAttributes "(Q=VRCWEE)"
    Tree = [ [ { Id = "1"; Area = 6; Label = "Closet"; Extrusion = None; Base = None } ] ]
}

// Generate the sub-layout confined inside the host room perimeter
let subRooms, bounds, ratio = 
    Nexel.generateNestLayout nestBlock hostRoom 3.0 allExistingRooms None

printfn "Generated %d nested sub-rooms" subRooms.Length
```

