# HYWE Conceptual Ontology & Technical Taxonomy

> **Canonical Definition:** HYWE (pronounced `/haɪv/`, acronym for *Hygrid Woven Ensemble*) is a deterministic computational spatial design formalism that generates, evaluates, and serializes architectural layouts from topological relationships and programmatic intent rather than continuous-domain geometric drafting.

---

## 1. The Six Facets of the HYWE Entity

To eliminate conceptual ambiguity across software, research, and data domains, HYWE is formally categorized into six explicit facets:

```text
HYWE (Hygrid Woven Ensemble)
│
├── 1. Method               (Relational topology before coordinate geometry)
├── 2. Computational Model   (Discrete Hygrid integer lattice + 24 sequence sweeps)
├── 3. Spatial Syntax       (Deterministic, token-efficient serialization format)
├── 4. Software Environment (Browser-native F# / WebAssembly / WebGPU CAD sandbox)
├── 5. Benchmark Corpus     (Machine-readable spatial configuration dataset)
└── 6. Research Program     (Investigating computability of spatial intent)
```

| Facet | Formal Designation | Core Function |
| :--- | :--- | :--- |
| **HYWE as Method** | *Relational Spatial Topology* | Solves architectural space planning by resolving hierarchy, adjacency, containment, and circulation sequence *before* geometric realization. |
| **HYWE as Computational Model** | *Deterministic Hygrid Engine* | A hybrid orthogonal-hexagonal discrete integer lattice evaluated through 24 deterministic directional sequence sweeps ($Q_0 \dots Q_{23}$) without probabilistic heuristics. |
| **HYWE as Syntax** | *HYWE Spatial Syntax* | A compact, tokenized alphanumeric grammar encoding design trees, spatial scales, level sequences, and boundaries into URL-safe, stateless hashes. |
| **HYWE as Software** | *HYWE Web Environment* | An edge-native, zero-install WebAssembly application built in functional F# (Bolero/Elmish) with WebGPU hardware-accelerated 3D spatial extrusion. |
| **HYWE as Dataset** | *Spatial Configuration Corpus* | A publicly indexed Hugging Face dataset (`vykrum/hywe-training-data`) providing structured pairings of architectural intent, relational syntax, and resolved geometries. |
| **HYWE as Research Program** | *Computable Spatial Reasoning* | An inquiry exploring whether spatial architectural reasoning can be formally represented, evaluated, and learned as structured relational data. |

---

## 2. Foundational Generational Lineage & Primitives

HYWE rejects continuous floating-point coordinate drafting in its solving stage. Space is discretized into an integer-lattice generational hierarchy where structural classes carry inherent architectural meaning:

```text
ARCHITECTURAL INTENT / PROGRAM
            │
            ▼
         LEXEL           (Linguistic & programmatic syntax parsing)
            │
            ▼
      SEQUENCE Q         (24 deterministic circulation & growth sweeps)
            │
            ▼
     HEXEL ALLOCATION    (Discrete coordinates on the hybrid integer Hygrid)
            │
            ▼
          COXEL          (Functional room clustering & metric compliance)
            │
            ▼
          GOXEL          (Polygon outer boundaries, offsets, and interior islands)
            │
            ▼
          XYXEL          (Resolved planar 2D spatial arrangement)
            │
       ┌────┴────────┐
       ▼             ▼
     NEXEL         ZAXEL
(Nested spaces) (Level stacking)
       │             │
       └────┬────────┘
            │
     ┌──────┴──────┐
     ▼             ▼
  GRAPHICS      ANALYSIS
(2D SVG / GPU) (Adjacency & Metrics)
```

### Primitive Definitions

1. **Hygrid**  
   The underlying discrete spatial lattice combining orthogonal Euclidean coordinates with hexagonal adjacency neighborhoods, providing uniform diagonal and orthogonal step distances without Euclidean floating-point drift.
2. **Lexel (`Lxl`)**  
   The lexical syntax parser converting human-readable functional programs (e.g., room names, target areas, connectivity requirements) into computational node hierarchies.
3. **Hexel (`Hxl`)**  
   The atomic spatial unit of the Hygrid lattice represented as an integer triple $(x, y, z)$. Every space in HYWE is composed of a finite collection of contiguous Hexels.
4. **Coxel (`Cxl`)**  
   A cohesive cluster of Hexels allocated to a specific architectural room or space. A Coxel contains a root `Base` Hexel, an array of body Hexels, programmatic identifiers (`Rfid`, `Name`, `Size`), and target ratio constraints.
5. **Goxel (`Gxl`)**  
   The boundary geometry engine that constrains Coxel growth to site boundaries, setbacks, and forbidden zones (internal courtyards/islands).
6. **Xyxel (`Xyl`)**  
   The planar 2D configuration resolved when all Coxel clusters for a specific elevation level satisfy adjacency and boundary constraints under a selected Sequence operator.
7. **Nexel (`Nxl`)**  
   A nested spatial hierarchy wherein a parent Coxel acts as a bounded sub-environment containing an independent child spatial configuration (e.g., private suites nested within a bedroom wing).
8. **Zaxel (`Zxl`)**  
   The vertical relational stacking operator governing multi-story massing, vertical structural alignment, stair/circulation penetrations, and elevation datum shifts.

---

## 3. Relational Rules & Deterministic Generation

HYWE layout generation is strictly **deterministic**: given an identical architectural program tree, boundary polygon, and Sequence index $Q_i$, the engine will generate the exact same geometry down to the bit across all platforms.

* **Adjacency:** Emerges directly from growth sequence and shared Hexel frontiers; spaces placed consecutively along a circulation tree naturally share borders without requiring post-hoc wall alignment algorithms.
* **Containment:** Governed by parent-child nesting rules where child spaces inherit boundary hulls from parent Coxels.
* **Sequence Sweeps ($Q_0 \dots Q_{23}$):** 24 canonical search vectors that exhaustively explore spatial orientations (e.g., South-first living spaces vs. North-first utility zones) without relying on random seeds or stochastic optimization.

---

## 4. The Spatial Configuration Benchmark Corpus

The **HYWE Spatial Configuration Dataset** published on Hugging Face ([`vykrum/hywe-training-data`](https://huggingface.co/datasets/vykrum/hywe-training-data)) provides an open benchmark for machine learning models in computational architecture:

* **Intent Mapping:** Pairs natural language architectural design briefs with formal spatial constraints.
* **Syntax Representation:** Encodes verified programmatic states into token-efficient strings.
* **Topological Ground Truth:** Contains precomputed 24-variation sequence sweeps, adjacency matrices, achieved area metrics, and SVG visualizations for downstream graph neural networks (GNNs), LLM reasoning benchmarks, and spatial synthesis models.

---

## 5. Canonical Citations & Reference URLs

* **Interactive Environment:** [hywe.in](https://hywe.in)
* **Core Code Repository:** [github.com/vykrum/Hywe](https://github.com/vykrum/Hywe)
* **Benchmark Dataset:** [huggingface.co/datasets/vykrum/hywe-training-data](https://huggingface.co/datasets/vykrum/hywe-training-data)
* **Comprehensive Documentation:** [wiki.hywe.in](https://wiki.hywe.in)
* **Author:** Vikram Subbaiah ([linkedin.hywe.in](https://linkedin.hywe.in))
