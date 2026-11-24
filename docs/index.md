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

## Namespaces & Modules

Explore the complete API reference for each engine module:

* **[Hywe.Core.Hexel](reference/hywe-core-hexel.html)**  
  Atomic spatial primitive, 24 canonical sequence operators ($\mathcal{Q}_{24}$), anisotropic parity validation (`hxlVld`), neighbor offsets, and Base34 coordinate compression.

* **[Hywe.Core.Coxel](reference/hywe-core-coxel.html)**  
  Relational spatial clustering, multi-seed room growth (`coxel`), space properties (`Prp`), resolved room footprints (`Cxl`), and hierarchical space nesting (`coxelChildren`).

* **[Hywe.Core.Goxel](reference/hywe-core-goxel.html)**  
  Planar geometry translation, discrete line rasterization on the hybrid lattice (`hxlLin`), and polygon boundary derivation.

* **[Hywe.Core.Lexel](reference/hywe-core-lexel.html)**  
  Relational DSL parsing, syntax tokenization (`.hyw`), and abstract syntax tree (`LexelNode`) construction.

* **[Hywe.Core.Xyxel](reference/hywe-core-xyxel.html)**  
  Planar 2D spatial arrangement, room adjacency resolution, and layout structures.

* **[Hywe.Core.Nexel](reference/hywe-core-nexel.html)**  
  Recursive spatial containment, internal void/island management, and sub-configurations.

* **[Hywe.Core.Zaxel](reference/hywe-core-zaxel.html)**  
  Volumetric aggregation, multi-level vertical elevations, and 3D massing structures.

---

## Architectural Hierarchy

$$\text{Lexel (AST)} \longrightarrow \text{Hexel (Lattice)} \longrightarrow \text{Coxel (Clustering)} \longrightarrow \text{Goxel (Geometry)} \longrightarrow \text{Xyxel / Nexel / Zaxel}$$

---

## Ecosystem Links

- **[HYWE Web Platform](https://hywe.in)** — Interactive browser-based spatial design studio.
- **[HYWE Wiki](https://wiki.hywe.in)** — Conceptual foundations, mathematical proofs, and user guides.
- **[GitHub Repository](https://github.com/vykrum/Hywe)** — Source code, issue tracker, and contributions.
- **[NuGet Package](https://www.nuget.org/packages/Hywe.Core)** — Download the package on NuGet.org.
