---
title: Coxel (Clustering)
category: Modules
categoryindex: 2
index: 2
---

# Coxel (Clustering)

A **Coxel** (*Collection of Hexels*) represents a discrete architectural zone, room, or functional space.

---

## Types

| Type | Kind | Description |
| :--- | :--- | :--- |
| `Prp` | Union | Architectural space metadata tag: `Label of string`, `Refid of string`, or `Count of int`. |
| `Cxl` | Record | Encapsulates a resolved room: `{ Name: Prp; Rfid: Prp; Size: Prp; Seqn: Sqn; Base: Hxl; Hxls: Hxl[] }`. |

---

## Functions

* **`prpVlu : Prp -> string`**  
  Extracts the string representation from any `Prp` tag.
* **`coxel : Sqn -> int -> (Hxl * Prp * Prp * Prp)[] -> Hxl[] -> Cxl[]`**  
  The primary deterministic clustering engine. Concurrently grows multiple room clusters from their respective seed bases up to their target capacities while avoiding collisions and obstacles.
* **`coxelChildren : Sqn -> int -> Cxl -> (Prp * Prp * Prp)[] -> Hxl[] -> Cxl * Cxl[]`**  
  Recursively clusters child spaces inside or immediately adjacent to a parent `Cxl` boundary according to hierarchical program intent.
* **`createBaseCoxel : Sqn -> int -> string -> Hxl -> Prp -> Prp -> Prp -> Hxl[] -> Cxl option -> Cxl * Hxl[]`**  
  Initializes a primary anchor room, integrating site boundary constraints and entry orientation.
* **`cxlExp : Cxl[] -> Sqn -> int -> (Cxl * int)[]`**  
  Calculates perimeter exposure metrics and exterior boundary availability for an array of rooms.
* **`cxlHxl : Cxl -> int -> Hxl[]`**  
  Extracts all member hexels of a `Cxl` at a given elevation.
* **`cxlPrm : Sqn -> int -> Cxl -> (int * int)[]`**  
  Computes the continuous perimeter boundary coordinates along the outer edge of a room cluster.
* **`cxlCnt : Cxl -> int * int`**  
  Computes the geometric centroid cell coordinate `(x, y)` of a room cluster, finding the nearest member hexel to the true mathematical center.
* **`cxlAdj : Cxl[] -> string[] * bool[][]`**  
  Computes the topological spatial adjacency matrix across all room clusters, determining which spaces share common boundary walls.
* **`getCxlCoordsString : Cxl -> string`**  
  Serializes all cell coordinates of a Coxel into space-separated Base34 coordinate pairs (e.g. `"0.0 2.0 1.-2"`).
* **`getBaseCoordString : Cxl -> string`**  
  Serializes only the anchor base coordinate of a Coxel into Base34.

---

## Usage Examples

### 1. Extracting Coxel Property Values

```fsharp
open Hywe.Core
open Hywe.Core.Coxel

let labelProp = Coxel.Label "Living Room"
let refidProp = Coxel.Refid "1.0"
let countProp = Coxel.Count 18

let labelStr = Coxel.prpVlu labelProp // "Living Room"
let countStr = Coxel.prpVlu countProp // "18"
```

### 2. Growing Spatial Room Clusters

Grow room clusters concurrently on the discrete Hygrid lattice from initial seed positions up to target capacities:

```fsharp
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel

let sqn = Hexel.Sqn.VRCWEE

// Define initial seed positions snapped to the lattice parity
let livingSeed = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
let diningSeed = Hexel.Hxl.AV(4, 2, 0) |> Hexel.hxlVld sqn

// Define architectural room definitions: (Seed, Refid, Area, Label)
let program = [|
    (livingSeed, Coxel.Refid "1.0", Coxel.Count 15, Coxel.Label "Living")
    (diningSeed, Coxel.Refid "2.0", Coxel.Count 10, Coxel.Label "Dining")
|]

// Grow clusters with no initial obstacles
let rooms = Coxel.coxel sqn 0 program [||]

rooms
|> Array.map (fun room -> Coxel.prpVlu room.Name, Coxel.prpVlu room.Rfid, room.Hxls.Length)
|> Array.iter (fun (name, refid, count) ->
    printfn "Room %s (%s): %d hexels" name refid count)
```

### 3. Calculating Centroids, Boundaries, and Adjacency

```fsharp
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel

let sqn = Hexel.Sqn.VRCWEE

// Centroid and continuous outer perimeter of the primary room
let primaryCentroid  = rooms |> Array.tryHead |> Option.map Coxel.cxlCnt
let primaryPerimeter = rooms |> Array.tryHead |> Option.map (Coxel.cxlPrm sqn 0)

// Topological wall adjacency matrix across all rooms
let roomLabels, adjMatrix = Coxel.cxlAdj rooms
```

