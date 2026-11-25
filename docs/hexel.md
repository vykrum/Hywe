---
title: Hexel (Lattice)
category: Modules
categoryindex: 2
index: 1
---

# Hexel (Lattice)

The `Hexel` module defines the atomic unit of the Hygrid lattice and provides the discrete arithmetic, sequence operators, parity validation, and neighbor traversal functions.

---

## Types

| Type | Kind | Description |
| :--- | :--- | :--- |
| `Point` | Struct | Integer 2D Cartesian coordinate: `{ X: int; Y: int }`. |
| `Hxl` | Struct Union | Atomic cell state: `AV(x, y, z)` (Available), `RV(x, y, z)` (Reserved/Occupied), `EX(x, y, z)` (External). |
| `Sqn` | Struct Union | The 24 canonical sequence operators (Q₂₄): 12 Vertical (`VRCWEE`..`VRCCNE`) and 12 Horizontal (`HRCWNN`..`HRCCNW`). |
| `(|Vertical|Horizontal|)` | Active Pattern | Categorizes any `Sqn` into either vertical or horizontal grid orientation. |

---

## Functions

### Coordinate Arithmetic & Validation

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

### Neighbor Traversal & Sequence Offsets

* **`sequence : Sqn -> (int * int)[]`**  
  Returns the 7 relative coordinate offset pairs `(dx, dy)` defined by the given sequence orientation. Offset `0` is always `(0, 0)` (host origin).
* **`adjacent : Sqn -> Hxl -> Hxl[]`**  
  Computes the 7 absolute neighbor cells centered on the host hexel, ordered according to the sequence's heading and chirality.
* **`sqnToString : Sqn -> string`**  
  Converts an `Sqn` union case to its canonical string representation (e.g. `"VRCWEE"`).
* **`sqnArray : Sqn[]`**  
  Static array containing all 24 canonical sequence operators.

### Cluster Expansion & Availability

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

### Cavity & Perimeter Analysis

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

## Usage Examples

### 1. Creating and Deconstructing Hexels

```fsharp
open Hywe.Core
open Hywe.Core.Hexel

// Origin anchor cell at ground elevation (z = 0)
let origin = Hexel.identity 0 // RV(0, 0, 0)

// Creating an available cell at coordinate (2, 0, 0)
let cell = Hxl.AV(2, 0, 0)

// Deconstructing to raw coordinates
let x, y, z = Hexel.hxlCrd cell
printfn "Cell coordinates: x=%d, y=%d, z=%d" x y z

// Compact Base34 encoding
let b34 = Hexel.toBase34 100L
printfn "Base34: %s" b34
```

### 2. Validating Coordinate Parity

The Hygrid lattice enforces parity rules (`% 2` and `% 4`) to maintain integer grid connectivity across vertical and horizontal weaves:

```fsharp
// Snapping arbitrary input coordinates to the nearest legal lattice cell
let rawPoint = Hxl.AV(1, 1, 0)
let legalCell = Hexel.hxlVld Sqn.VRCWEE rawPoint

let lx, ly, _ = Hexel.hxlCrd legalCell
printfn "Snapped legal coordinates: (%d, %d)" lx ly
```

### 3. Traversal and Neighbor Discovery

Each sequence operator defines the heading and clockwise/counterclockwise order of the 6 surrounding cells:

```fsharp
// Get the 7 relative coordinate offsets (origin + 6 neighbors)
let offsets = Hexel.sequence Sqn.VRCWEE

// Compute the 7 absolute neighbor cells centered on the origin
let host = Hxl.AV(0, 0, 0)
let neighborCoords =
    host
    |> Hexel.adjacent Sqn.VRCWEE
    |> Array.map Hexel.hxlCrd

neighborCoords
|> Array.iter (fun (nx, ny, nz) -> printfn "Neighbor: (%d, %d, %d)" nx ny nz)
```

### 4. Availability & Cluster Expansion

```fsharp
// Create a fast lookup set of occupied obstacles
let occupied = [| Hxl.AV(2, 0, 0); Hxl.AV(1, -2, 0) |]
let occSet = Hexel.hxlSet occupied

// Check how many free neighbors remain around origin
let freeNeighbors = Hexel.availableSet Sqn.VRCWEE 0 host occSet
printfn "Free adjacent cells: %d" freeNeighbors

// Advance a cluster seed towards the next optimal growth step
let seed = (host, 5) // (seed cell, remaining capacity)
match Hexel.incrementSet Sqn.VRCWEE 0 seed occSet with
| nextCell, remaining when remaining >= 0 ->
    printfn "Next cell in cluster: %A" nextCell
| _ ->
    printfn "Cluster expansion blocked"
```

### 5. Boundary Tracing & Exterior Buffer

```fsharp
let cluster = [| Hxl.AV(0, 0, 0); Hxl.AV(2, 0, 0); Hxl.AV(1, 2, 0) |]

// Trace the continuous ordered outer perimeter
let perimeter = Hexel.bndSqn Sqn.VRCWEE 0 cluster

// Compute a 1-ring outer buffer around the cluster
let bufferRing = Hexel.hxlOfs Sqn.VRCWEE 0 cluster
printfn "Buffer ring cell count: %d" bufferRing.Length
```

