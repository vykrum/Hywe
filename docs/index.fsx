(**
---
title: Hywe.Core API Documentation
---

# Hywe.Core API Documentation

**Deterministic Spatial Topology & Integer Lattice Packing Engine**

`Hywe.Core` is an open-source, zero-dependency .NET library (`net10.0` / `net8.0`) for the **Hygrid Woven Ensemble (HYWE)**. It resolves relational, hierarchical architectural intent deterministically onto an integer hybrid orthogonal-hexagonal lattice.

[![NuGet Version](https://img.shields.io/nuget/v/Hywe.Core.svg)](https://www.nuget.org/packages/Hywe.Core)

```bash
dotnet add package Hywe.Core
```

Or reference it directly in an F# Interactive script (`.fsx` via `dotnet fsi`):

```fsharp
#r "nuget: Hywe.Core"                              // From NuGet (standalone scripts)
// #r "src/Hywe.Core/bin/Debug/net10.0/Hywe.Core.dll" // Local repo build
```

---

## Architectural Pipeline & Modules

The engine compiles abstract architectural programs into resolved 2D floorplans and 3D volumetric massing across seven decoupled stages:

> **Lexel (AST)** &rarr; **Hexel (Lattice)** &rarr; **Coxel (Clustering)** &rarr; **Goxel (Geometry)** &rarr; **Xyxel (Layout)** &rarr; **Nexel (Nesting)** &rarr; **Zaxel (3D Stacking)**

| Module | Role | Reference |
| :--- | :--- | :--- |
| **Lexel** | `.hyw` syntax parser, attribute blocks, and hierarchical ASTs | [Lexel API & Examples](reference/hywe-core-lexel.html) |
| **Hexel** | Atomic cell states (`Hxl`), $Q_{24}$ sequence operators (`Sqn`), parity validation, and $O(1)$ neighbor checks | [Hexel API & Examples](reference/hywe-core-hexel.html) |
| **Coxel** | Deterministic multi-room spatial clustering (`coxel`), exposure, and adjacency matrices (`cxlAdj`) | [Coxel API & Examples](reference/hywe-core-coxel.html) |
| **Goxel** | Discrete-to-continuous geometry, rasterization (`hxlLin`, `hxlPgn`), polygon cleanup, and hole-area metrics | [Goxel API & Examples](reference/hywe-core-goxel.html) |
| **Xyxel** | 2D planar floorplan compilation, boundary fitting, keep-out voids, and proportional scaling | [Xyxel API & Examples](reference/hywe-core-xyxel.html) |
| **Nexel** | Recursive sub-layout synthesis inside host room boundaries | [Nexel API & Examples](reference/hywe-core-nexel.html) |
| **Zaxel** | Multi-level vertical aggregation, floor-to-floor elevations, and 3D massing (`generateMultiLevelLayout`) | [Zaxel API & Examples](reference/hywe-core-zaxel.html) |

---

## Application Examples

### F# (End-to-End Multi-Level `.hyw` Compilation)
*)
(*** hide ***)
#r "../src/Hywe.Core/bin/Debug/net10.0/Hywe.Core.dll"
(** *)
open Hywe.Core

// 1. Define a multi-level architectural program in .hyw syntax
let hyweSpec =
    "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
    "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

// 2. Synthesize all levels, boundaries, and elevations deterministically
let rooms, bounds, elvs, ratios =
    Zaxel.generateMultiLevelLayout hyweSpec "0,0" [||] None None None

// 3. Inspect synthesized rooms functionally
rooms
|> Array.iter (fun room ->
    printfn "%s (%s): base=%A, cells=%d"
        (Coxel.prpVlu room.Name)
        (Coxel.prpVlu room.Rfid)
        (Hexel.hxlCrd room.Base)
        (room.Hxls.Length + 1))
(*** include-output ***)
(**
### C# (Direct .NET Interoperability)

`Hywe.Core` compiles to a standard .NET assembly (`net10.0` / `net8.0`) and can be called directly from C# (e.g. ASP.NET, Rhino 8 / Grasshopper, Revit 2025+, or Unity):

```csharp
using System;
using Hywe.Core;

// 1. Configure sequence operator and elevation
var sqn = Hexel.Sqn.VRCWEE;
var level = 0;

// 2. Define room seeds snapped to legal lattice parity
var livingBase = Hexel.hxlVld(sqn, Hexel.Hxl.NewAV(0, 0, level));
var diningBase = Hexel.hxlVld(sqn, Hexel.Hxl.NewAV(8, 0, level));

var program = new[]
{
    Tuple.Create(livingBase, Coxel.Prp.NewRefid("1.0"), Coxel.Prp.NewCount(15), Coxel.Prp.NewLabel("Living Room")),
    Tuple.Create(diningBase, Coxel.Prp.NewRefid("2.0"), Coxel.Prp.NewCount(10), Coxel.Prp.NewLabel("Dining Area"))
};

// 3. Synthesize spatial clusters
var rooms = Coxel.coxel(sqn, level, program, Array.Empty<Hexel.Hxl>());

// 4. Inspect resolved rooms and Base34 coordinates
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

- **[HYWE Web App](https://hywe.in)** — Interactive browser-based spatial design environment.
- **[HYWE Wiki](https://wiki.hywe.in)** — Conceptual foundations, formal specifications, and benchmarks.
- **[GitHub Repository](https://github.com/vykrum/Hywe)** — Source code, test suite, and issue tracker.
- **[NuGet Package](https://www.nuget.org/packages/Hywe.Core)** — `dotnet add package Hywe.Core`.
*)
