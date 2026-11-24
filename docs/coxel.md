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
