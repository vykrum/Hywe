# Hywe.Core Release Notes

All notable changes to the `Hywe.Core` NuGet package and engine are documented in this file.

---

## [2026.11.23] — 2026-11-23

### Highlights
- Official public release of `Hywe.Core` on [NuGet.org](https://www.nuget.org/packages/Hywe.Core/2026.11.23).
- Zero external runtime dependencies.
- Dual-targeted for **.NET 10.0** and **.NET 8.0 (LTS)** with full symbol package (`snupkg`) support.

### Architecture & Modules
- **`Hywe.Core.Hexel`**:
  - Implements the atomic Hygrid lattice cell representation (`Hxl`) with available, reserved, and external states.
  - 24 canonical sequence operators ($\mathcal{Q}_{24}$): 12 vertical and 12 horizontal orientations.
  - Discrete coordinate arithmetic, parity validation (`hxlVld`), neighbor traversal, and $O(1)$ hash-set membership checks (`availableSet`, `incrementSet`, `incrementsSet`).
  - Cavity detection (`cavityScan`, `enclosedCavities`) and perimeter analysis.

- **`Hywe.Core.Coxel`**:
  - Encapsulates discrete architectural spatial clusters and zones (`Cxl`).
  - Deterministic concurrent room expansion engine (`coxel`) with obstacle collision avoidance.
  - Recursive child space clustering (`coxelChildren`).
  - Topological spatial adjacency matrix generation (`cxlAdj`) and boundary perimeter extraction (`cxlPrm`).
  - Space-separated Base34 coordinate serialization (`getCxlCoordsString`).

- **`Hywe.Core.Goxel`**:
  - Vector geometry bridge translating discrete lattice coordinates into planar geometry.
  - Raster line generation (`hxlLin`) and discrete polygon fill (`hxlPgn`).
  - Polygon cleanup pipeline: collinear point elimination (`removeCollinear`), duplicate vertex deduplication (`dedupeSequential`), sawtooth reduction (`removeSawtooth`), and winding normalization (`normalizeWinding`).
  - Planar polygon Shoelace area calculation (`polygonArea`) and nested island/void subtraction (`polygonWithHolesArea`).

- **`Hywe.Core.Lexel`**:
  - Strongly-typed Abstract Syntax Tree (`LexelNode`, `LexelAttributes`, `LevelBlock`, `NestBlock`).
  - Parsing engine for `.hyw` domain-specific architectural syntax and node token streams (`(ID/Count/Label)`).
  - Multi-level program splitting (`processFullString`).

- **`Hywe.Core.Xyxel`**:
  - 2D planar layout planning, boundary polyline fitting, and boundary obstacle compilation.
  - Target area reproportioning to fit within physical site net boundary areas (`applyReproportioning`).

- **`Hywe.Core.Nexel`**:
  - Recursive spatial nesting engine (`generateNestLayout`) generating sub-room layouts embedded inside parent spaces.

- **`Hywe.Core.Zaxel`**:
  - Multi-level vertical aggregation and 3D volumetric massing accumulator (`BuildState`).
  - Unified multi-level synthesis pipeline (`generateMultiLevelLayout`).

---

### Links & Reference
- **NuGet Package**: [https://www.nuget.org/packages/Hywe.Core/2026.11.23](https://www.nuget.org/packages/Hywe.Core/2026.11.23)
- **API Documentation**: [https://hywe.in/api/](https://hywe.in/api/)
- **Wiki Reference**: [https://wiki.hywe.in](https://wiki.hywe.in)
- **GitHub Repository**: [https://github.com/vykrum/Hywe](https://github.com/vykrum/Hywe)
