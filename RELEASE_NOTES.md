# Hywe.Core Release Notes

All notable changes to the `Hywe.Core` NuGet package and engine are documented in this file.

---

## [2026.11.24] — 2026-11-24

<!-- Release notes generated using configuration in .github/release.yml at v2026.11.24 -->



**Full Changelog**: https://github.com/vykrum/Hywe/compare/v2026.11.23...v2026.11.24

---

## [2026.11.24] — 2026-11-24

### Highlights
- Comprehensive XML documentation and IntelliSense code usage examples across all 7 core modules (`Hexel`, `Coxel`, `Goxel`, `Lexel`, `Xyxel`, `Nexel`, `Zaxel`).
- Multi-page documentation overhaul with dedicated module reference pages, interactive code tooltips, and syntax-highlighted examples.
- Automated release management pipeline with Zenodo DOI sync and NuGet Trusted Publishing.

### Modules & Documentation
- **`Hywe.Core.Hexel`**: Added complete `<summary>`, `<param>`, `<returns>`, and `<example><code>` usage blocks for coordinate validation (`hxlVld`), neighbor offsets (`sequence`, `adjacent`), cluster growth (`increment`, `availableSet`), and perimeter analysis (`bndSqn`, `hxlOfs`).
- **`Hywe.Core.Coxel`**: Added usage examples for concurrent clustering (`coxel`), child room expansion (`coxelChildren`), root initialization (`createBaseCoxel`), perimeter exposure (`cxlExp`), and topological adjacency matrix computation (`cxlAdj`).
- **`Hywe.Core.Goxel`**: Added usage examples for vector line drawing (`hxlLin`), discrete polygon fill (`hxlPgn`), geometry cleanup (`cleanPolygon`, `removeSawtooth`), and Shoelace area calculations with hole subtraction (`polygonWithHolesArea`).
- **`Hywe.Core.Lexel`**: Added usage examples for syntax attribute parsing (`parseAttributes`), node AST parsing (`parseNodes`), multi-level spec processing (`processFullString`), and sequence injection (`injectSqn`).
- **`Hywe.Core.Xyxel`**: Added usage examples for discriminated union parsing (`tryParseUnion`), site boundary area scaling (`applyReproportioning`), and layout generation (`generateCxlLayout`).
- **`Hywe.Core.Nexel`**: Added usage examples for nested sub-layout markers (`isNestMarker`) and embedded parent-space nesting (`generateNestLayout`).
- **`Hywe.Core.Zaxel`**: Added usage examples for volumetric massing and multi-level floor plan generation (`generateMultiLevelLayout`).
- **Web Documentation**: Added dedicated module pages (`hexel.md`, `coxel.md`, `goxel.md`, `lexel.md`, `xyxel.md`, `nexel.md`, `zaxel.md`) with copy-pasteable F# code recipes.

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
