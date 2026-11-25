---
title: Lexel (AST)
category: Modules
categoryindex: 2
index: 4
---

# Lexel (AST)

The `Lexel` module parses architectural program declarations and `.hyw` domain-specific syntax into strongly-typed abstract syntax trees.

---

## Types

| Type | Kind | Description |
| :--- | :--- | :--- |
| `LexelNode` | Record | Relational node: `{ Id: string; Count: int; Label: string; Parent: string option; Children: LexelNode list }`. |
| `LexelAttributes` | Record | Metadata block attributes: `{ Sequence: string; Level: int; Scale: int; Width: int option; ... }`. |
| `LevelBlock` | Record | Complete definition of an architectural floor level (metadata + program tree). |
| `NestBlock` | Record | Nested sub-configuration embedded within a parent room. |
| `LexelBlock` | Union | Container union: `Level of LevelBlock` or `Nest of NestBlock`. |

---

## Functions

* **`parseNodes : string -> string list -> LexelNode list list`**  
  Parses raw string tokens (e.g. `["(1/15/Living)"; "(1.1/8/Entry)"]`) into hierarchical trees of `LexelNode` instances.
* **`parseAttributes : string -> LexelAttributes`**  
  Parses header attribute blocks (e.g. `"[Q:VRCWEE;L:0;W:40;H:40]"`) into strongly-typed `LexelAttributes`.
* **`processXyxel : string -> string -> {| Attributes: LexelAttributes; Tree: LexelNode list list |} option`**  
  Unified parser processing both attribute blocks and node declarations for a single floor level.
* **`processFullString : string -> LexelBlock list`**  
  Top-level parser splitting an entire multi-level `.hyw` architectural program into structured `Level` and `Nest` blocks.
* **`extractSequences : string -> Map<int, string>`**  
  Extracts the mapping of level indices to their active sequence operators (`Q=...`).
* **`injectSqn : string -> int -> string -> string`**  
  Updates or inserts a sequence operator specification into a specific level marker within a `.hyw` program string.

---

## Usage Examples

### 1. Parsing Header Attribute Blocks

Parse `.hyw` architectural header metadata into structured records:

```fsharp
open Hywe.Core
open Hywe.Core.Lexel

// Parse attributes specifying sequence, elevation, scale, and boundary dimensions
let attrs = Lexel.parseAttributes "(Q=VRCWEE/L=0/X=1.0/W=40/H=30)"

printfn "Sequence: %A, Level: %A, Width: %A, Height: %A" 
    attrs.Sequence attrs.Level attrs.Width attrs.Height
```

### 2. Parsing Hierarchical Architectural Node Declarations

Parse room and sub-space hierarchy declarations:

```fsharp
open Hywe.Core
open Hywe.Core.Lexel

// Hierarchy: (1/25/Living) is parent, (1.1/10/Entry) and (1.2/8/Storage) are children
let nodeTokens = [
    "(1/25/Living)"
    "(1.1/10/Entry)"
    "(1.2/8/Storage)"
    "(2/20/Kitchen)"
]

nodeTokens
|> Lexel.parseNodes "0,0"
|> List.collect id
|> List.iter (fun node ->
    printfn "Node %s: Area=%d, Label=%s, Base=%A"
        node.Id node.Area node.Label node.Base)
```

### 3. Parsing Complete Multi-Level `.hyw` Programs

```fsharp
open Hywe.Core
open Hywe.Core.Lexel

let hyweSpec = 
    "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
    "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

hyweSpec
|> Lexel.processFullString
|> List.map (function
    | Lexel.Level lvl -> sprintf "Level %s has %d node groups" lvl.Marker lvl.Tree.Length
    | Lexel.Nest nest -> sprintf "Nest %s has %d node groups" nest.Marker nest.Tree.Length)
|> List.iter (printfn "%s")
```

