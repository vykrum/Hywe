---
title: Overview
category: Documentation
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

## Core Modules

| Module | Stage | Key Capabilities |
| :--- | :--- | :--- |
| **[Hexel (Lattice)](hexel.html)** | Stage 1 | Atomic cell representation (`Hxl`), Q₂₄ sequence operators (`Sqn`), lattice arithmetic, parity validation, neighbor traversal, and O(1) collision checks. |
| **[Coxel (Clustering)](coxel.html)** | Stage 2 | Spatial clustering engine (`coxel`), concurrent growth, perimeter exposure (`cxlExp`), room boundaries, and spatial adjacency matrices (`cxlAdj`). |
| **[Goxel (Geometry)](goxel.html)** | Stage 3 | Continuous vector geometry bridge, discrete raster lines (`hxlLin`), polygon fills (`hxlPgn`), Shoelace area, and boundary smoothing. |
| **[Lexel (AST)](lexel.html)** | Input AST | Architectural syntax parser for `.hyw` program trees, attribute blocks, and hierarchical graph representations. |
| **[Xyxel (Layout)](xyxel.html)** | Stage 4 | 2D planar floorplan compilation, site boundary constraint solving, keep-out void avoidance, and proportional scaling. |
| **[Nexel (Nesting)](nexel.html)** | Stage 5 | Recursive spatial nesting compiler for embedding nested sub-layouts within host room boundaries. |
| **[Zaxel (3D Stacking)](zaxel.html)** | Stage 6 | Multi-level vertical aggregation, floor-to-floor elevation calculation, and 3D volumetric massing accumulator. |

---

## Complete Working Example

### F#

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
rooms
|> Array.iter (fun room ->
    printfn "Room: %s (ID: %s)" (Coxel.prpVlu room.Name) (Coxel.prpVlu room.Rfid)
    printfn "  Anchor Base: %A" (Hexel.hxlCrd room.Base)
    printfn "  Cells Allocated: %d" (room.Hxls.Length + 1)
    printfn "  Base34 Coordinates: %s\n" (Coxel.getCxlCoordsString room))
```

### C#

`Hywe.Core` is a standard .NET assembly (`net8.0` / `net10.0`) and can be consumed directly from C#:

```csharp
using System;
using Hywe.Core;

// 1. Configure sequence and base elevation
var sqn = Hexel.Sqn.VRCWEE;
var level = 0;

// 2. Define program nodes: (BaseSeed, RefID, AreaCount, RoomLabel)
var livingBase = Hexel.hxlVld(sqn, Hexel.Hxl.NewAV(0, 0, level));
var diningBase = Hexel.hxlVld(sqn, Hexel.Hxl.NewAV(8, 0, level));

var program = new[]
{
    Tuple.Create(livingBase, Coxel.Prp.NewRefid("1.0"), Coxel.Prp.NewCount(15), Coxel.Prp.NewLabel("Living Room")),
    Tuple.Create(diningBase, Coxel.Prp.NewRefid("2.0"), Coxel.Prp.NewCount(10), Coxel.Prp.NewLabel("Dining Area"))
};

// 3. Synthesize spatial clusters
var rooms = Coxel.coxel(sqn, level, program, Array.Empty<Hexel.Hxl>());

// 4. Output results
Array.ForEach(rooms, room =>
{
    Console.WriteLine($"Room: {Coxel.prpVlu(room.Name)} (ID: {Coxel.prpVlu(room.Rfid)})");
    Console.WriteLine($"  Anchor Base: {Hexel.hxlCrd(room.Base)}");
    Console.WriteLine($"  Cells Allocated: {room.Hxls.Length + 1}");
    Console.WriteLine($"  Base34 Coordinates: {Coxel.getCxlCoordsString(room)}\n");
});
```

---

## Ecosystem Links

- **[HYWE Web Platform](https://hywe.in)** — Interactive browser-based spatial design studio.
- **[HYWE Wiki](https://wiki.hywe.in)** — Conceptual foundations, mathematical proofs, and user guides.
- **[GitHub Repository](https://github.com/vykrum/Hywe)** — Source code, issue tracker, and contributions.
- **[NuGet Package](https://www.nuget.org/packages/Hywe.Core)** — Download the package on NuGet.org.
