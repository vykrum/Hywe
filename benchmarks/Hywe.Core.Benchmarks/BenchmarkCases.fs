/// <summary>
/// Shared canonical benchmark archetypes and compilation helpers used by both
/// the native .NET BenchmarkDotNet harness and the WebAssembly browser benchmarks.
/// </summary>
module Hywe.Benchmarks.Cases

open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

/// <summary>
/// Canonical 24 discrete hexagonal sequence operator tokens representing all permutations
/// of grid orientation (Vertical/Horizontal), winding order (Clockwise/Counter-Clockwise),
/// and starting compass directional offsets.
/// </summary>
let operators = [|
    "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
    "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
|]

/// <summary>
/// Predefined canonical architectural archetypes used across empirical benchmark runs:
/// CIF-10, Radial-Star, and Deep-Spine.
/// </summary>
let presets = [|
    "CIF-10", LayoutTree.Create [| [|
        ("1", 60, "Primary Intake");
        ("1.1", 40, "Acute Care");
        ("1.1.1", 30, "Critical Care");
        ("1.1.2", 25, "Resuscitation");
        ("1.2", 45, "Diagnostic Imaging");
        ("1.2.1", 35, "Specialized Scan");
        ("1.3", 20, "Clinical Support");
        ("1.4", 50, "Public Arrival");
        ("1.4.1", 35, "Consultation Suites");
        ("1.4.2", 20, "Service Core")
    |] |]
    "Radial-Star", LayoutTree.Create [| [|
        ("1", 60, "Central Hub");
        ("1.1", 40, "Sector A");
        ("1.2", 40, "Sector B");
        ("1.3", 40, "Sector C");
        ("1.4", 40, "Sector D");
        ("1.5", 40, "Sector E");
        ("1.6", 40, "Sector F")
    |] |]
    "Deep-Spine", LayoutTree.Create [| [|
        ("1", 50, "Spine-1");
        ("1.1", 50, "Spine-2");
        ("1.1.1", 50, "Spine-3");
        ("1.1.1.1", 50, "Spine-4");
        ("1.1.1.1.1", 50, "Spine-5");
        ("1.1.1.1.1.1", 50, "Spine-6");
        ("1.1.1.1.1.1.1", 50, "Spine-7")
    |] |]
|]

/// <summary>
/// Sequence operator names pre-parsed into their strongly-typed <see cref="Sqn"/> representations.
/// </summary>
let parsedOperators =
    operators
    |> Array.map (fun opName -> opName, tryParseUnion<Sqn> opName |> Option.get)

/// <summary>
/// Compiles a <see cref="LayoutTree"/> against a target sequence operator (<see cref="Sqn"/>),
/// isolating core AST resolution and spatial packing from DOM, SVG, and WebGPU passes.
/// </summary>
let runCompilation (tree: LayoutTree) (sqn: Sqn) =
    let opts = {
        EntryFallback = "0,0"
        InitialOcc = [||]
        Seq = Some sqn
        Width = Some 100
        Height = Some 100
        OuterStr = None
        IslandsStr = None
        ParentCxl = None
        Ratio = Some 1.0
        Elevation = Some 0
    }

    let ctx = prepareLayoutContext Map.empty tree opts
    match generateBaseCxl ctx with
    | Some (baseCxl, nextOcc) ->
        let layout, _, _ = generateCxlLayout ctx baseCxl nextOcc
        layout
    | None -> failwithf "Failed to generate base hexel for %s" (sqnToString sqn)
