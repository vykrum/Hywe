**HYWE** is a **browser-based** design sandbox where **structured intent** metamorphoses into **spatial configurations** through **design computation**.

---
![Hywe Banner](https://vykrum.github.io/Hywe/images/hyweLogoBanner.png)

---
# H Y W E

**Hy**grid **W**oven **E**nsemble

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## Philosophy

Hywe is founded on the idea that **spatial reasoning can be expressive and computational without imitating architectural software norms**. It encourages a form of design thinking where **spatial topology and flow-based hierarchy** guide the creation of layouts. By prioritizing the "logic of movement" over simple wall-to-wall adjacency, Hywe uses a procedural approach to turn abstract intent into structured configurations. Every aspect of the system, from interactivity to structure, has been built from scratch to reflect this paradigm.

At its core, **Hywe Syntax is the singular source of truth**. Every spatial configuration, volumetric massing, and topological relationship is a direct derivation of this syntax. In this ecosystem, design intent is encoded into a logic-driven language where everything—from geometry to hierarchy—depends on the integrity of the string.

---

## Access

[Open Hywe in your browser — No installation or Sign-in required](https://vykrum.github.io/Hywe/)  

## The Workspace

https://github.com/user-attachments/assets/cc523e4c-ca69-431a-8cbb-eb58c001b3dc

---

## Features

- **Topology-first** spatial flow editor: Prioritize connections and sequence over static geometry.
- **Flow-based hierarchy** visualization: Real-time mapping of structural relationships.
- **2D layouts** visualized with high-performance **SVG**.
- **3D extrusions** rendered using **WebGPU** for modern hardware acceleration.
- **Generative Design**: Dynamic, multi-level layout generation via procedural logic.
- **Dataset Generation**: Use the syntax-driven core to generate massive, procedurally varied architectural datasets.
- **Extensibility**: Uses a transparent, text-based syntax designed to enable straightforward integration with other **AEC tools** and Common Data Environments (CDE).
- **API Potential**: The engine is built as a decoupled **WebAssembly core**, allowing for future integration as a programmatic design API for third-party web apps.
- **Stateless Sharing**: Share entire 3D design states via simple URLs. No sign-in or database required; the Syntax is serialized directly into the URL hash.
- **Privacy-focused**: No backend; all computation runs in-browser via **WebAssembly**.
- **Edge-First Architecture**: 
    - **Zero-Latency**: Instantaneous procedural feedback with no round-trips to a server.
    - **Offline-Ready**: As a Progressive Web App (PWA) **Hywe** (Hygrid Woven Ensemble) is an open-source, **browser-based architectural tool** designed to **automate architectural programming** and generate **floor layouts based on the flow of activity**. It serves as a deterministic, **rule-based alternative to generative AI** for early-stage floor layouts and spatial plans.

Built from first principles in **F#** and compiled to **WebAssembly (WASM)**, Hywe enables architects to weave complex **multi-story programmatic stacking diagrams** and **vertical spatial hierarchies** directly in the browser with zero installation.

> [!TIP]
> You can access the **Hywe Syntax** within the app by using the **Node/Code toggle** on the top-left menu in the workspace.

---

## Sitemap of Logic

Hywe is structured as a computational pipeline that transforms designer intent into architectural form:

| Stage | Component | Logic | Output |
| :--- | :--- | :--- | :--- |
| **1. Intent** | `Interactive Node Tree Input` & `Interactive Boundary Editor` | Defining spatial rules and physical constraints. | Design Intent |
| **2. Encoding** | Hywe Syntax | Compact, deterministic encoding of design rules. | `.hyw` String |
| **3. Parsing** | `Lexel` | **Architectural programming** and flow parsing. | `TreeNode` Hierarchy |
| **4. Formation** | `Hexel` & `Coxel` | **Fundamental Units** and **Spatial Clustering**. | Geometric Fabric |
| **5. Distribution** | `Xyxel` | **Coxel Configuration** and 2D layout. | SVG Rendering |
| **6. Massing** | `Zaxel` | **Xyxel Stacking** and 3D volume. | WebGPU Massing |
| **7. Expansion** | `Batch` & `Teach` | **Variation processing** and **dataset generation**. | AI Dataset (Hynteract) |
| **8. Insight** | `Analyze` & `Report` | **Spatial metrics** and **automated documentation**. | PDF Report |

## Targeted Use Cases

Hywe is engineered to solve specific high-intent architectural bottlenecks:
- **Automate Architectural Programming**: Instantly transform programmatic requirements and adjacency matrices into spatial configurations.
- **Software for Spatial Adjacency**: A rule-based engine that converts **spatial adjacency matrices to floor plans** with mathematical precision.
- **Alternative to Generative AI**: Provides a deterministic solution for designers who need control and logic over stochastic AI generation.
- **Flow-Based Layout Generation**: Generate floor plans that emerge from the **flow of activity** rather than static boundaries.
- **Programmatic Stacking Diagrams**: Handle **Vertical Spatial Hierarchy** and multi-level programmatic distribution in a zero-install, web environment.

---

## Technical Architecture

Hywe is built as a **strictly functional engine**. It treats spatial design as a computational problem, where inputs are transformed through a series of geometric and topological "folds".

```mermaid
graph TD
    A1[Interactive Node Tree Input] --> B[Hywe Syntax]
    A2[Interactive Boundary Editor] --> B[Hywe Syntax]
    B --> C(Lexel Parsing)
    C --> D(Hexel: Fundamental Unit)
    D --> E(Coxel: Hexel Cluster)
    E --> F(Xyxel: Coxel Configuration)
    F --> G(Zaxel: Xyxel Stacking)
    
    F --> F1[SVG Rendering]
    G --> G1[WebGPU Massing]
    
    F --> H[Spatial Analysis]
    F --> I[Batch Processing]
    
    F1 -.-> ReportLabel((PDF Report))
    G1 -.-> ReportLabel
    H -.-> ReportLabel
    I -.-> ReportLabel
    
    I -.-> DatasetLabel((Hynteract: AI Dataset))
    DesignIntent[Design Intent] --> DatasetLabel
```

- **Language:** [F#](https://fsharp.org/) (functional-first design)
- **Frontend:** [Bolero](https://fsbolero.io/) (Blazor on WASM)
- **3D Graphics:** [WebGPU](https://gpuweb.github.io/gpuweb/)
- **Geometry Logic:** Purely functional, **integer-based** spatial partitioning (no floating-point drift).
- **Core Engine:** Built on **Boolean-driven topological logic**, prioritizing hierarchy, connectivity and flow.
- **Data Fidelity:** Enables **bit-precise data generation**, ensuring perfect determinism for future AI training.

---

## Development & Community

We are building Hywe as an open ecosystem for **design computation**.

- **Contribute**: See our [Contributing Guide](CONTRIBUTING.md) to get started.
- **AI-Friendly**: Technical summary for AI agents available at [llms.txt](llms.txt).
- **Roadmap**: Check out the [Issues](https://github.com/vykrum/Hywe/issues) for planned features like deep architectural nesting and enhanced WebGPU massing.

---

## Citation

If you use Hywe in your research or professional projects, please cite it using the following metadata:

```text
Subbaiah, V. (2026). HYWE: Hygrid Woven Ensemble. 
Retrieved from https://github.com/vykrum/Hywe
```

Or use the "Cite this repository" button in the GitHub sidebar.

---

## License

This project is licensed under the [MIT License](LICENSE).
