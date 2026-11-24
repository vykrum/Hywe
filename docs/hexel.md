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
