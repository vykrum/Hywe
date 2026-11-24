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
