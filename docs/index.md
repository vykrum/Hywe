---
title: HYWE Core API Documentation
category: Overview
categoryindex: 1
index: 1
---

# Hywe.Core API Documentation

**Deterministic Spatial Topology & Integer Lattice Packing Engine**

`Hywe.Core` is an open-source, zero-dependency .NET engine for the Hygrid Woven Ensemble (HYWE). It resolves relational, hierarchical architectural intent onto an integer hybrid orthogonal-hexagonal lattice.

[![NuGet Version](https://img.shields.io/nuget/v/Hywe.Core.svg)](https://www.nuget.org/packages/Hywe.Core)

```bash
dotnet add package Hywe.Core
```

---

## Architectural Hierarchy

The computational pipeline translates abstract spatial graphs into fully resolved 2D floorplans and 3D volumetric massing across seven decoupled stages:

> **Lexel (AST)** &rarr; **Hexel (Lattice)** &rarr; **Coxel (Clustering)** &rarr; **Goxel (Geometry)** &rarr; **Xyxel / Nexel / Zaxel**

---

## 1. Module: `Hywe.Core.Hexel`

The `Hexel` module defines the atomic unit of the Hygrid lattice and provides the discrete arithmetic, sequence operators, parity validation, and neighbor traversal functions.

### Hexel Types

| Type | Kind | Description |
| :--- | :--- | :--- |
| `Point` | Struct | Integer 2D Cartesian coordinate: `{ X: int; Y: int }`. |
| `Hxl` | Struct Union | Atomic cell state: `AV(x, y, z)` (Available), `RV(x, y, z)` (Reserved/Occupied), `EX(x, y, z)` (External). |
| `Sqn` | Struct Union | The 24 canonical sequence operators (Q₂₄): 12 Vertical (`VRCWEE`..`VRCCNE`) and 12 Horizontal (`HRCWNN`..`HRCCNW`). |
| `(|Vertical|Horizontal|)` | Active Pattern | Categorizes any `Sqn` into either vertical or horizontal grid orientation. |

### Hexel Functions

#### Coordinate Arithmetic & Validation
* **`hxlCrd : Hxl -> int * int * int`**  
  Deconstructs any `Hxl` instance and returns its raw `(x, y, z)` coordinate triple.
* **`identity : int -> Hxl`**  
  Returns the lattice origin anchor cell `RV(0, 0, elv)` at the specified elevation.
* **`hxlVld : Sqn -> Hxl -> Hxl`**  
  Validates and snaps an arbitrary coordinate to the nearest legal discrete lattice cell based on modulo parity rules (`% 2` and `% 4`) for the given sequence orientation.
* **`hxlUni : int -> Hxl[] -> Hxl[]`**  
  Uniformly converts an array of hexels to a single target occupancy type (`1` = `AV`, `2` = `RV`, `3` = `EX`).
* **`toBase34 : int64 -> string`**  
  Encodes an integer into HYWE's compact Base34 string notation (excluding `I` and `O`).

#### Neighbor Traversal & Sequence Offsets
* **`sequence : Sqn -> (int * int)[]`**  
  Returns the 7 relative coordinate offset pairs `(dx, dy)` defined by the given sequence orientation. Offset `0` is always `(0, 0)` (host origin).
* **`adjacent : Sqn -> Hxl -> Hxl[]`**  
  Computes the 7 absolute neighbor cells centered on the host hexel, ordered according to the sequence's heading and chirality.
* **`sqnToString : Sqn -> string`**  
  Converts an `Sqn` union case to its canonical string representation (e.g. `"VRCWEE"`).
* **`sqnArray : Sqn[]`**  
  Static array containing all 24 canonical sequence operators.

#### Cluster Expansion & Availability
* **`available : Sqn -> int -> obj -> Hxl[] -> int`**  
  Counts free neighbor cells around a candidate hexel given an array of occupied obstacles.
* **`availableSet : Sqn -> int -> Hxl -> HashSet<Hxl> -> int`**  
  High-performance O(1) variant of `available` using a hash set of occupied cells.
* **`increment : Sqn -> int -> Hxl * int -> Hxl[] -> (Hxl * int) option`**  
  Computes the next optimal expansion hexel for a growing spatial cluster.
* **`incrementSet : Sqn -> int -> Hxl * int -> HashSet<Hxl> -> (Hxl * int) option`**  
  High-performance hash set variant of `increment`.
* **`increments : Sqn -> int -> (Hxl * int)[] -> Hxl[] -> (Hxl * int)[]`**  
  Advances multiple active cluster seeds simultaneously while preventing collision.
* **`incrementsSet : Sqn -> int -> (Hxl * int)[] -> HashSet<Hxl> -> (Hxl * int)[]`**  
  High-performance hash set variant of `increments`.
* **`hxlSet : Hxl seq -> HashSet<Hxl>`**  
  Constructs a `System.Collections.Generic.HashSet<Hxl>` from any sequence of hexels for O(1) membership checks.
* **`getHxls : (Hxl * int)[] -> Hxl[]`**  
  Extracts the underlying `Hxl` array from an array of weighted tuples.

#### Cavity & Perimeter Analysis
* **`hxlFil : Sqn -> int -> Hxl[] -> Hxl[]`**  
  Detects and fills internal cavities or voids trapped within an enclosed cluster of hexels.
* **`bndSqn : Sqn -> int -> Hxl[] -> Hxl[]`**  
  Traces and orders the continuous outer boundary hexels of a cluster.
* **`cntSqn : Sqn -> int -> Hxl[] -> Hxl[]`**  
  Sorts a collection of hexels into a continuous sequence path.
* **`hxlOfs : Sqn -> int -> Hxl[] -> Hxl[]`**  
  Calculates a single-ring exterior buffer/offset around a collection of hexels.
* **`hxlRst : Hxl[] -> Hxl[] -> Hxl[]`**  
  Performs discrete subtraction, returning cells in the first array not present in the second.
* **`hxlChk : Sqn -> int -> Hxl[] -> Hxl[] -> Hxl[]`**  
  Classifies cells into available, reserved, or boundary status relative to site obstacles.
* **`hxlChkSet : Sqn -> int -> HashSet<Hxl> -> Hxl[] -> Hxl[]`**  
  High-performance hash set variant of `hxlChk`.

---

## 2. Module: `Hywe.Core.Coxel`

A **Coxel** (*Collection of Hexels*) represents a discrete architectural zone, room, or functional space.

### Coxel Types

| Type | Kind | Description |
| :--- | :--- | :--- |
| `Prp` | Union | Architectural space metadata tag: `Label of string`, `Refid of string`, or `Count of int`. |
| `Cxl` | Record | Encapsulates a resolved room: `{ Name: Prp; Rfid: Prp; Size: Prp; Seqn: Sqn; Base: Hxl; Hxls: Hxl[] }`. |

### Coxel Functions

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

## 3. Module: `Hywe.Core.Goxel`

The `Goxel` module translates discrete integer lattice coordinates into continuous vector geometry, boundary polygons, and linear segments.

### Goxel Functions

* **`hxlLin : Sqn -> int -> Hxl -> Hxl -> Hxl[]`**  
  Computes a discrete Bresenham-like raster line of hexels connecting two coordinates on the hybrid integer lattice.
* **`hxlPgn : Sqn -> int -> (int * int)[] -> Hxl[]`**  
  Converts a series of 2D polygon vertices into a closed, filled set of discrete lattice hexels.
* **`cleanPolygon : Sqn -> (int * int)[] -> (int * int)[]`**  
  Normalizes polygon vertices by removing collinear points, deduplicating sequential coordinates, and smoothing lattice sawtooth artifacts.
* **`dedupeSequential : (int * int)[] -> (int * int)[]`**  
  Removes consecutive duplicate vertex coordinates from a polygon point array.
* **`ensureClosed : (int * int)[] -> (int * int)[]`**  
  Appends the start vertex to the end if not already identical, guaranteeing a closed polygon ring.
* **`removeSawtooth : Sqn -> (int * int)[] -> (int * int)[]`**  
  Eliminates discrete stair-step / sawtooth artifacts along lattice boundary paths.
* **`removeCollinear : (int * int)[] -> (int * int)[]`**  
  Removes redundant points that lie on straight line segments.
* **`normalizeWinding : bool -> (int * int)[] -> (int * int)[]`**  
  Enforces clockwise or counter-clockwise vertex winding order.
* **`bounds : (int * int)[] -> int * int * int * int`**  
  Returns the bounding box `(minX, minY, maxX, maxY)` for a point collection.
* **`centroid : (int * int)[] -> int * int`**  
  Computes the integer centroid coordinate of a polygon.
* **`polygonArea : (int * int)[] -> int64`**  
  Calculates the planar area of a polygon using the Shoelace formula.
* **`polygonWithHolesArea : (int * int)[] -> (int * int)[][] -> int64`**  
  Calculates the net area of an outer boundary polygon subtracting all interior hole/island polygons.
* **`pointInPolygon : int * int -> (int * int)[] -> bool`**  
  Tests whether a point lies within a polygon boundary using ray casting.
* **`parsePolygon : string -> (float * float)[]`**  
  Parses a serialized coordinate stream into floating-point vertex tuples.
* **`parsePolyIslands : string -> (float * float)[][]`**  
  Parses multiple serialized island/void polygons.
* **`parseCoords : string -> (int * int)[]`**  
  Parses space- or comma-separated integer coordinates into coordinate pairs.
* **`parseIslands : string -> (int * int)[][]`**  
  Parses semicolon-delimited multiple interior island/void polygon vertex arrays.

---

## 4. Module: `Hywe.Core.Lexel`

The `Lexel` module parses architectural program declarations and `.hyw` domain-specific syntax into strongly-typed abstract syntax trees.

### Lexel Types

| Type | Kind | Description |
| :--- | :--- | :--- |
| `LexelNode` | Record | Relational node: `{ Id: string; Count: int; Label: string; Parent: string option; Children: LexelNode list }`. |
| `LexelAttributes` | Record | Metadata block attributes: `{ Sequence: string; Level: int; Scale: int; Width: int option; ... }`. |
| `LevelBlock` | Record | Complete definition of an architectural floor level (metadata + program tree). |
| `NestBlock` | Record | Nested sub-configuration embedded within a parent room. |
| `LexelBlock` | Union | Container union: `Level of LevelBlock` or `Nest of NestBlock`. |

### Lexel Functions

* **`parseNodes : string -> string list -> LexelNode list list`**  
  Parses raw string tokens (e.g. `["(1/15/Living)"; "(1.1/8/Entry)"]`) into hierarchical trees of `LexelNode` instances.
* **`parseAttributes : string -> LexelAttributes`**  
  Parses header attribute blocks (e.g. `"[Q:VRCWEE;L:0;W:40;H:40]"`) into strongly-typed `LexelAttributes`.
* **`processXyxel : string -> string -> {| Attributes: LexelAttributes; Tree: LexelNode list list |} option`**  
  Unified parser processing both attribute blocks and node declarations for a single floor level.
* **`processFullString : string -> LexelBlock list`**  
  Top-level parser splitting an entire multi-level `.hyw` architectural program into structured `Level` and `Nest` blocks.
* **`extractSequences : string -> Map<int, string>`**  
  Extracts the mapping of level indices to their active sequence operators (`Q=...`).
* **`injectSqn : string -> int -> string -> string`**  
  Updates or inserts a sequence operator specification into a specific level marker within a `.hyw` program string.

---

## 5. Module: `Hywe.Core.Xyxel`

`Xyxel` handles planar layout planning, boundary fitting, and 2D spatial arrangement.

### Xyxel Types

* **`LayoutTree`**: Scaled representation of the architectural program tree with total requested area.
* **`LayoutOptions`**: Configuration options for layout synthesis (elevation, boundary overrides, entry points).
* **`LayoutContext`**: Complete resolved context containing boundary constraints, keep-out islands, and scaled node trees.

### Xyxel Functions

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

---

## 6. Module: `Hywe.Core.Nexel`

`Nexel` provides recursive spatial nesting, allowing architectural sub-programmes to be nested inside parent room boundaries.

### Nexel Functions

* **`generateNestLayout : NestBlock -> Coxel.Cxl -> float -> Coxel.Cxl[] -> string option -> Coxel.Cxl[] * (int * int * int * int) * float`**  
  Synthesizes an internal sub-layout within the boundary footprint of a host room.
* **`isNestMarker : string -> bool`**  
  Determines whether a block header identifier corresponds to an interior nest.
* **`getNestIndex : string -> int option`**  
  Extracts the numerical nest index from a marker string.

---

## 7. Module: `Hywe.Core.Zaxel`

`Zaxel` manages multi-level vertical aggregation, floor-to-floor stacking, and 3D volumetric massing data structures.

### Zaxel Types

* **`BuildState`**: State accumulator for multi-level buildings: `{ Cxls: Cxl[]; Bounds: (int * int * int * int)[]; Elvs: (int * int)[]; Ratio: float; ... }`.

### Zaxel Functions

* **`generateMultiLevelLayout : string -> string -> Hxl[] -> (int * Sqn) option -> string option -> string option -> BuildState`**  
  Parses a complete multi-level `.hyw` architectural dossier, resolves all floor levels and nested spaces, and outputs the volumetric building state.

---

## Complete Working Example

```fsharp
open System
open Hywe.Core

// 1. Configure sequence and base elevation
let sqn = Hexel.Sqn.VRCWEE
let level = 0

// 2. Define program nodes: (BaseSeed, RefID, AreaCount, RoomLabel)
let livingBase = Hexel.Hxl.AV(0, 0, level) |> Hexel.hxlVld sqn
let diningBase = Hexel.Hxl.AV(8, 0, level) |> Hexel.hxlVld sqn

let program = [|
    (livingBase, Coxel.Refid "1.0", Coxel.Count 15, Coxel.Label "Living Room")
    (diningBase, Coxel.Refid "2.0", Coxel.Count 10, Coxel.Label "Dining Area")
|]

// 3. Synthesize spatial clusters
let obstacles = [||]
let rooms = Coxel.coxel sqn level program obstacles

// 4. Output results
for room in rooms do
    printfn "Room: %s (ID: %s)" (Coxel.prpVlu room.Name) (Coxel.prpVlu room.Rfid)
    printfn "  Anchor Base: %A" (Hexel.hxlCrd room.Base)
    printfn "  Cells Allocated: %d" (room.Hxls.Length + 1)
    printfn "  Base34 Coordinates: %s\n" (Coxel.getCxlCoordsString room)
```

---

## Ecosystem Links

- **[HYWE Web Platform](https://hywe.in)** — Interactive browser-based spatial design studio.
- **[HYWE Wiki](https://wiki.hywe.in)** — Conceptual foundations, mathematical proofs, and user guides.
- **[GitHub Repository](https://github.com/vykrum/Hywe)** — Source code, issue tracker, and contributions.
- **[NuGet Package](https://www.nuget.org/packages/Hywe.Core)** — Download the package on NuGet.org.
