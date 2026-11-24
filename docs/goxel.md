---
title: Goxel (Geometry)
category: Modules
categoryindex: 2
index: 3
---

# Goxel (Geometry)

The `Goxel` module translates discrete integer lattice coordinates into continuous vector geometry, boundary polygons, and linear segments.

---

## Functions

### Rasterization & Discrete Conversion

* **`hxlLin : Sqn -> int -> Hxl -> Hxl -> Hxl[]`**  
  Computes a discrete Bresenham-like raster line of hexels connecting two coordinates on the hybrid integer lattice.
* **`hxlPgn : Sqn -> int -> (int * int)[] -> Hxl[]`**  
  Converts a series of 2D polygon vertices into a closed, filled set of discrete lattice hexels.

### Polygon Normalization & Cleanup

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

### Spatial Metrics & Ray-Casting

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

### Serialized Stream Parsing

* **`parsePolygon : string -> (float * float)[]`**  
  Parses a serialized coordinate stream into floating-point vertex tuples.
* **`parsePolyIslands : string -> (float * float)[][]`**  
  Parses multiple serialized island/void polygons.
* **`parseCoords : string -> (int * int)[]`**  
  Parses space- or comma-separated integer coordinates into coordinate pairs.
* **`parseIslands : string -> (int * int)[][]`**  
  Parses semicolon-delimited multiple interior island/void polygon vertex arrays.
