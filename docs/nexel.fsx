(**
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

* **`generateNestLayout : NestBlock -> Coxel.Cxl -> float -> Coxel.Cxl[] -> string option -> (Coxel.Cxl[] * (int * int)[][] * float) option`**  
  Synthesizes an internal sub-layout within the boundary footprint of a host room.
* **`isNestMarker : string -> bool`**  
  Determines whether a block header identifier corresponds to an interior nest.
* **`getNestIndex : string -> int option`**  
  Extracts the numerical nest index from a marker string.

---

## Usage Examples

### 1. Inspecting Nested Sub-Layout Markers
*)
(*** hide ***)
#r "../src/Hywe.Core/bin/Debug/net10.0/Hywe.Core.dll"
(** *)
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Lexel
open Hywe.Core.Nexel

let marker1 = "N1"
let marker2 = "L0"

let isN1 = Nexel.isNestMarker marker1 // true
let isL0 = Nexel.isNestMarker marker2 // false
let nestIdx = Nexel.getNestIndex marker1 // Some 1

printfn "isN1=%b, isL0=%b, nestIdx=%A" isN1 isL0 nestIdx
(*** include-output ***)

(**
### 2. Synthesizing Nested Sub-Rooms Inside a Host Room
*)
// Generate a host room (e.g. Master Suite) to serve as the nesting boundary
let sqn = Hexel.Sqn.VRCWEE
let hostSeed = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
let allExistingRooms =
    Coxel.coxel sqn 0 [| (hostSeed, Coxel.Refid "1", Coxel.Count 30, Coxel.Label "MasterSuite") |] [||]

let hostRoom = Array.head allExistingRooms

let nestBlock : Lexel.NestBlock = {
    Marker = "N1"
    Attributes = Lexel.parseAttributes "(Q=VRCWEE)"
    Tree = [ [ { Id = "1"; Area = 6; Label = "Closet"; Extrusion = None; Base = None } ] ]
}

// Generate the sub-layout confined inside the host room perimeter
match Nexel.generateNestLayout nestBlock hostRoom 3.0 allExistingRooms None with
| Some (subRooms, _, _) ->
    printfn "Generated %d nested sub-rooms inside %s" subRooms.Length (Coxel.prpVlu hostRoom.Name)
| None ->
    printfn "Host room could not accommodate nested layout"
(*** include-output ***)
