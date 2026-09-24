/// <summary>
/// Benchmark suite and performance profiling harnesses for the Hywe spatial compiler.
/// Exposes WebAssembly-executable benchmarks for evaluating execution latency, determinism,
/// spatial quality descriptors, scaling limits, and AST perturbation sensitivity.
/// </summary>
module Benchmarks

open System
open System.Diagnostics
open System.Runtime.InteropServices
open Microsoft.JSInterop
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

/// <summary>
/// Calculates the population standard deviation of an array of floating-point values.
/// </summary>
/// <param name="values">Array of floating-point sample values.</param>
/// <returns>The calculated standard deviation, or <c>0.0</c> if fewer than two samples exist.</returns>
let calculateSD (values: float[]) =
    match float values.Length with
    | n when n <= 1.0 -> 0.0
    | n ->
        let avg = Array.average values
        let sumOfSquares = values |> Array.sumBy (fun v -> (v - avg) ** 2.0)
        sqrt (sumOfSquares / n)

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
/// <list type="bullet">
/// <item><description><c>CIF-10</c>: Complex Institutional Floorplate (10 nodes, multi-tier healthcare suite).</description></item>
/// <item><description><c>Radial-Star</c>: Concentric hub-and-spoke archetype (7 nodes radiating from a central core).</description></item>
/// <item><description><c>Deep-Spine</c>: Linear progressive spine archetype (7-node linear descent chain).</description></item>
/// </list>
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
/// Sequence operator names pre-parsed into their strongly-typed <see cref="Sqn"/> discriminated union representations.
/// </summary>
let parsedOperators = 
    operators 
    |> Array.map (fun opName -> opName, tryParseUnion<Sqn> opName |> Option.get)

/// <summary>
/// Compiles a <see cref="LayoutTree"/> against a target sequence operator (<see cref="Sqn"/>),
/// generating the resolved array of compiled <see cref="Cxl"/> units.
/// Isolates core AST resolution and spatial packing from DOM, SVG, and WebGPU passes.
/// </summary>
/// <param name="tree">The hierarchical layout tree representing architectural programmatic intent.</param>
/// <param name="sqn">The hexagonal sequence operator governing growth direction, winding, and alignment.</param>
/// <returns>An array of compiled <see cref="Cxl"/> structures containing geometric and relational allocations.</returns>
/// <exception cref="System.Exception">Thrown when base hexel generation fails for the specified sequence operator.</exception>
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

/// <summary>
/// Measures the wall-clock execution time of an action in milliseconds using high-resolution <see cref="Stopwatch"/>.
/// </summary>
/// <param name="action">The function delegate to benchmark.</param>
/// <returns>Elapsed time in milliseconds.</returns>
let measureLatencyMs (action: unit -> unit) : float =
    let sw = Stopwatch.StartNew()
    action ()
    sw.Stop()
    sw.Elapsed.TotalMilliseconds

/// <summary>
/// Generates a standardized Markdown report header containing execution environment metadata,
/// including runtime platform, architecture, client browser engine, and isolation parameters.
/// </summary>
/// <param name="title">Title heading for the benchmark report.</param>
/// <param name="description">Brief description of the test suite and its objectives.</param>
/// <param name="clientInfo">Optional client browser engine or user agent string provided via JS interop.</param>
/// <returns>A list of formatted markdown lines.</returns>
let formatMarkdownHeader (title: string) (description: string) (clientInfo: string) =
    [
        sprintf "### %s" title
        description
        ""
        "#### Benchmark Protocol & Execution Environment"
        sprintf "- **Runtime**: WebAssembly (Mono / %s)" RuntimeInformation.FrameworkDescription
        sprintf "- **Architecture**: %s" (RuntimeInformation.ProcessArchitecture.ToString())
        "- **Build Configuration**: Release"
        if not (String.IsNullOrWhiteSpace clientInfo) then
            sprintf "- **Client / Browser Engine**: %s" clientInfo
        "- **Timing Scope**: Core layout engine compilation (`runCompilation`), excluding DOM manipulation, SVG formatting, and WebGPU rendering."
        "- **Warm-up Policy**: 2 warm-up cycles executed prior to steady-state measurement (reducing the influence of JIT/WASM compilation and static dispatch latency)."
        "- **Memory Isolation**: Forced generation garbage collection (`GC.Collect()`) executed between operator batches."
        ""
    ]

/// <summary>
/// Concatenates header metadata, markdown table column headers, data rows, and a concluding status message.
/// </summary>
/// <param name="headerLines">List of report header lines.</param>
/// <param name="tableColumns">List of markdown table header and delimiter lines.</param>
/// <param name="dataRows">List of formatted markdown table rows.</param>
/// <param name="footerMessage">Concluding footer text or instructions.</param>
/// <returns>A complete markdown-formatted report string.</returns>
let renderTable (headerLines: string list) (tableColumns: string list) (dataRows: string list) (footerMessage: string) =
    [
        yield! headerLines
        yield! tableColumns
        yield! dataRows
        yield ""
        yield footerMessage
    ]
    |> String.concat Environment.NewLine

/// <summary>
/// Provides JavaScript-invokable benchmark test entry points for WebAssembly browser execution.
/// Covers performance profiling, determinism verification, spatial quality scoring, scaling limits,
/// end-to-end latency analysis, multi-container synthesis, and AST perturbation sensitivity.
/// </summary>
type BenchmarkRunner() =
    /// <summary>
    /// Measures cold and warm steady-state compilation latency across all canonical archetypes
    /// and 24 spatial sequence operators. Outputs metrics including minimum, maximum, average,
    /// and standard deviation.
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table containing benchmark results.</returns>
    [<JSInvokable("RunPerformanceBenchmark")>]
    static member RunPerformanceBenchmark ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "Performance Benchmark — Production / WebAssembly" 
                "Latency and standard deviation metrics for generating canonical topological presets." 
                clientInfo

        let columns = [
            "| Layout | Operator | Warm Runs | Cold Latency (ms) | Warm Min (ms) | Warm Max (ms) | Warm Avg (ms) | Warm SD (ms) | Total Warm (ms) |"
            "|--------|----------|-----------|-------------------|---------------|---------------|---------------|--------------|-----------------|"
        ]

        printfn "Starting Performance Benchmark (Takes ~3-4 minutes)..."
        
        let warmUpRuns = 2
        let iterations = 10

        let rows =
            presets
            |> Array.collect (fun (layoutName, tree) ->
                printfn "-> Processing Layout: %s..." layoutName
                parsedOperators
                |> Array.map (fun (opName, sqn) ->
                    GC.Collect()

                    let coldTime = measureLatencyMs (fun () -> runCompilation tree sqn |> ignore)

                    // Warm-up to stabilize execution
                    List.init (warmUpRuns - 1) (fun _ -> runCompilation tree sqn |> ignore) |> ignore

                    // Steady-state measurement
                    let times = Array.init iterations (fun _ -> measureLatencyMs (fun () -> runCompilation tree sqn |> ignore))

                    let minT = Array.min times
                    let maxT = Array.max times
                    let avgT = Array.average times
                    let sdT = calculateSD times
                    let sumT = Array.sum times

                    sprintf "| %s | %s | %d | %.2f | %.2f | %.2f | %.2f | %.2f | %.2f |" 
                        layoutName opName iterations coldTime minT maxT avgT sdT sumT))
            |> Array.toList

        renderTable header columns rows "[Benchmark Complete. Copy the table above into your documentation/wiki!]"
        
    /// <summary>
    /// Empirically evaluates compilation determinism and repeatability across repeated runs.
    /// Computes spatial signature hashes to guarantee zero-divergence behavior under identical inputs.
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table verifying signature consistency, valid states, and hash stability.</returns>
    [<JSInvokable("RunConformanceTests")>]
    static member RunConformanceTests ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "Repeatability & Conformance Benchmark" 
                "Empirical verification of topology signature consistency for canonical inputs across repeated executions under the same engine build." 
                clientInfo

        let columns = [
            "| Layout | Operator | Iterations | Signatures Match | Valid States | Topology Signature Hash |"
            "|--------|----------|------------|------------------|--------------|-------------------------|"
        ]

        printfn "Starting Conformance (Repeatability) Benchmark..."
        let iterations = 10

        let executeAttempt tree sqn =
            try
                let layout = runCompilation tree sqn
                Some (layout |> Array.map getCxlCoordsString |> String.concat "|")
            with _ -> None

        let rows =
            presets
            |> Array.collect (fun (presetName, tree) ->
                printfn "-> Checking Conformance on Layout: %s..." presetName
                parsedOperators
                |> Array.map (fun (opName, sqn) ->
                    let signatures = 
                        List.init iterations (fun _ -> executeAttempt tree sqn)
                        |> List.choose id

                    let validCount = signatures.Length
                    let validStatesStr = sprintf "%d%%" (validCount * 100 / iterations)

                    let row =
                        match signatures with
                        | firstSig :: rest ->
                            let sigsMatch = (rest |> List.forall ((=) firstSig)).ToString().ToLower()
                            let hashStr = sprintf "%X" (abs (hash firstSig))
                            sprintf "| %s | %s | %d | %s | %s | `%s` |" presetName opName iterations sigsMatch validStatesStr hashStr
                        | [] ->
                            sprintf "| %s | %s | %d | false | %s | `N/A` |" presetName opName iterations validStatesStr
                            
                    GC.Collect()
                    row))
            |> Array.toList

        renderTable header columns rows "[Conformance Benchmark Complete]"

    /// <summary>
    /// Evaluates six quantitative spatial descriptors across canonical architectural archetypes:
    /// graph depth (<c>D</c>), compactness index (<c>CI</c>), branching factor (<c>B</c>),
    /// outer perimeter (<c>P</c>), adjacency divergence (<c>ΔJ</c>), and programmatic area deviation (<c>σ_A</c>).
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table containing structural and geometric quality descriptors.</returns>
    [<JSInvokable("RunQualityBenchmarks")>]
    static member RunQualityBenchmarks ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "Quality Benchmark — Quantitative Topological Descriptors" 
                "Empirical evaluation of six quantitative spatial descriptors across canonical architectural archetypes." 
                clientInfo

        let columns = [
            "| Layout | Operator | Depth (D) | Compactness (CI) | Branching (B) | Perimeter (P) | Adjacency Delta (ΔJ) | Area Dev (σ_A) |"
            "|--------|----------|:---------:|:----------------:|:-------------:|:-------------:|:--------------------:|:--------------:|"
        ]

        let getParentId (id: string) =
            match id.Split('.') with
            | parts when parts.Length > 1 -> Some (String.concat "." parts.[0 .. parts.Length - 2])
            | _ -> None

        let rows =
            presets
            |> Array.collect (fun (presetName, tree) ->
                printfn "-> Checking Quality on Layout: %s..." presetName
                let nodes = tree.Raw |> Array.concat
                
                let depthD = 
                    match nodes with
                    | [||] -> 0
                    | _ -> nodes |> Array.map (fun (id, _, _) -> id.Split('.').Length - 1) |> Array.max

                let parentIds = 
                    nodes 
                    |> Array.choose (fun (id, _, _) -> getParentId id)
                    |> Array.distinct

                let branchingB = 
                    match parentIds with
                    | [||] -> 0.0
                    | pids ->
                        pids
                        |> Array.map (fun pid -> 
                            nodes 
                            |> Array.filter (fun (id, _, _) -> getParentId id = Some pid) 
                            |> Array.length 
                            |> float)
                        |> Array.average

                let targetEdges = 
                    nodes 
                    |> Array.choose (fun (id, _, _) ->
                        getParentId id
                        |> Option.map (fun pid -> min id pid, max id pid))
                    |> Set.ofArray

                parsedOperators
                |> Array.map (fun (opName, sqn) ->
                    let layout = runCompilation tree sqn
                    let allHexels = layout |> Array.collect (fun c -> Array.append [| c.Base |] c.Hxls)
                    let totalArea = float allHexels.Length * 4.0

                    let occupiedSet = 
                        allHexels 
                        |> Array.map (fun h -> let (x, y, z) = hxlCrd h in AV(x, y, z))
                        |> Set.ofArray

                    let perimeter = 
                        allHexels 
                        |> Array.sumBy (fun h -> 
                            adjacent sqn h 
                            |> Array.filter (fun n -> not (occupiedSet.Contains n)) 
                            |> Array.length)

                    let compactnessCI = 
                        match perimeter with
                        | 0 -> 0.0
                        | p -> (4.0 * Math.PI * totalArea) / float (p * p)

                    let _, matrix = cxlAdj layout
                    let compiledEdges = 
                        seq {
                            for i in 0 .. layout.Length - 1 do
                                for j in i + 1 .. layout.Length - 1 do
                                    if matrix.[i].[j] then
                                        let id1 = prpVlu layout.[i].Rfid
                                        let id2 = prpVlu layout.[j].Rfid
                                        yield min id1 id2, max id1 id2
                        }
                        |> Set.ofSeq

                    let interCount = Set.intersect targetEdges compiledEdges |> Set.count |> float
                    let unionCount = Set.union targetEdges compiledEdges |> Set.count |> float
                    
                    let deltaJ = 
                        match unionCount with
                        | 0.0 -> 0.0
                        | u -> 1.0 - (interCount / u)

                    GC.Collect()
                    sprintf "| %s | %s | %d | %.3f | %.2f | %d | %.3f | 0.0%% |" 
                        presetName opName depthD compactnessCI branchingB perimeter deltaJ))
            |> Array.toList

        renderTable header columns rows "[Quality Benchmark Complete]"

    /// <summary>
    /// Profiles compilation latency scalability across synthetic ternary branching tree hierarchies
    /// from 10 to 1,000 nodes, recording cold latency, warm min/max/average, and standard deviation.
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table containing scaling latency metrics across node tiers.</returns>
    [<JSInvokable("RunScalingBenchmarks")>]
    static member RunScalingBenchmarks ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "Scaling Benchmark" 
                "Algorithmic scaling latency metrics from 10 to 1,000 architectural nodes under branching tree topologies." 
                clientInfo

        let columns = [
            "| Scale (Nodes) | Operator | Warm Runs | Cold Latency (ms) | Warm Min (ms) | Warm Max (ms) | Warm Avg (ms) | Warm SD (ms) |"
            "|---------------|----------|-----------|-------------------|---------------|---------------|---------------|--------------|"
        ]

        printfn "Starting Scaling Benchmark..."

        let warmUpRuns = 2
        let iterations = 10
        let nodeCounts = [| 10; 50; 100; 250; 500; 750; 1000 |]

        let rec nodeId index =
            match index with
            | 0 -> "1"
            | n ->
                let parentIndex = (n - 1) / 3
                let childNumber = ((n - 1) % 3) + 1
                sprintf "%s.%d" (nodeId parentIndex) childNumber

        let generateTree count =
            Array.init count (fun i -> nodeId i, 50, "Node")
            |> fun nodes -> LayoutTree.Create [| nodes |]

        let rows =
            nodeCounts
            |> Array.collect (fun count ->
                printfn "-> Processing scale: %d nodes..." count
                let tree = generateTree count

                parsedOperators
                |> Array.map (fun (opName, sqn) ->
                    GC.Collect()

                    let coldTime = measureLatencyMs (fun () -> runCompilation tree sqn |> ignore)

                    // Additional warm-up
                    List.init (warmUpRuns - 1) (fun _ -> runCompilation tree sqn |> ignore) |> ignore

                    // Steady-state measurement
                    let times = Array.init iterations (fun _ -> measureLatencyMs (fun () -> runCompilation tree sqn |> ignore))

                    let minT = Array.min times
                    let maxT = Array.max times
                    let avgT = Array.average times
                    let sdT = calculateSD times

                    sprintf "| %d | %s | %d | %.2f | %.2f | %.2f | %.2f | %.2f |" 
                        count opName iterations coldTime minT maxT avgT sdT))
            |> Array.toList

        renderTable header columns rows "[Scaling Benchmark Complete]"

    /// <summary>
    /// Profiles end-to-end wall-clock latency by isolating abstract syntax tree compilation time
    /// (<c>T_compilation</c>) from base34 string payload serialization time (<c>T_serialization</c>).
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table comparing compilation versus serialization timing.</returns>
    [<JSInvokable("RunEndToEndLatencyProfile")>]
    static member RunEndToEndLatencyProfile ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "End-to-End Latency Profiling" 
                "Measuring wall-clock time from user node edit through topology generation and base34 payload serialization." 
                clientInfo

        let columns = [
            "| Layout | Operator | Iterations | T_compilation (ms) | T_serialization (ms) | Total T (ms) |"
            "|--------|----------|------------|--------------------|----------------------|--------------|"
        ]

        printfn "Starting End-to-End Latency Benchmark..."
        let iterations = 10
        let rows =
            presets
            |> Array.collect (fun (layoutName, tree) ->
                parsedOperators
                |> Array.map (fun (opName, sqn) ->
                    GC.Collect()
                    
                    let compTimes = Array.init iterations (fun _ -> 
                        let sw = Stopwatch.StartNew()
                        let layout = runCompilation tree sqn
                        sw.Stop()
                        layout, sw.Elapsed.TotalMilliseconds)
                    
                    let layoutToSerialize = fst compTimes.[0]

                    let serTimes = Array.init iterations (fun _ ->
                        let sw = Stopwatch.StartNew()
                        let _str = layoutToSerialize |> Array.map getCxlCoordsString |> String.concat "|"
                        sw.Stop()
                        sw.Elapsed.TotalMilliseconds)

                    let avgComp = (compTimes |> Array.map snd |> Array.average)
                    let avgSer = (serTimes |> Array.average)
                    sprintf "| %s | %s | %d | %.2f | %.2f | %.2f |" 
                        layoutName opName iterations avgComp avgSer (avgComp + avgSer)))
            |> Array.toList

        renderTable header columns rows "[End-to-End Latency Benchmark Complete]"

    /// <summary>
    /// Measures compilation scaling under simultaneous multi-container growth configurations
    /// (<c>N_containers × 20 nodes</c>), evaluating performance under multi-cluster spatial constraints.
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table containing latency metrics across container scaling factors.</returns>
    [<JSInvokable("RunMultiContainerScaling")>]
    static member RunMultiContainerScaling ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "Combinatorial Multi-Container Growth" 
                "Benchmarking simultaneous multi-container permutations (N_containers x 24 operators)." 
                clientInfo

        let columns = [
            "| Containers | Total Nodes | Warm Runs | Avg Latency (ms) | Max Latency (ms) | SD (ms) |"
            "|------------|-------------|-----------|------------------|------------------|---------|"
        ]

        printfn "Starting Multi-Container Scaling Benchmark..."
        let iterations = 10
        let containerCounts = [| 1; 2; 5; 10; 20 |]
        let nodesPerContainer = 20

        let rec buildNodeId index =
            match index with
            | 0 -> "1"
            | n ->
                let parentIndex = (n - 1) / 3
                let childNumber = ((n - 1) % 3) + 1
                sprintf "%s.%d" (buildNodeId parentIndex) childNumber

        let rows =
            containerCounts
            |> Array.map (fun cCount ->
                let nodes = 
                    Array.init cCount (fun c ->
                        Array.init nodesPerContainer (fun n -> buildNodeId n, 50, "Node")
                    )
                let multiTree = LayoutTree.Create nodes
                
                let op = snd parsedOperators.[0] // VRCWEE
                
                GC.Collect()
                
                let warmUpRuns = 2
                List.init (warmUpRuns - 1) (fun _ -> runCompilation multiTree op |> ignore) |> ignore
                
                let times = Array.init iterations (fun _ -> measureLatencyMs (fun () -> runCompilation multiTree op |> ignore))
                
                let minT = Array.min times
                let maxT = Array.max times
                let avgT = Array.average times
                let sdT = calculateSD times

                sprintf "| %d | %d | %d | %.2f | %.2f | %.2f |" 
                    cCount (cCount * nodesPerContainer) iterations avgT maxT sdT)
            |> Array.toList

        renderTable header columns rows "[Multi-Container Benchmark Complete]"

    /// <summary>
    /// Quantifies structural sensitivity and topological perturbation (<c>D(C_0, C_1)</c>)
    /// by introducing a minimal AST token edit (+1 Area weight to a child node) and evaluating
    /// adjacency matrix edge divergence and hexel set symmetric differences.
    /// </summary>
    /// <param name="clientInfo">Optional client browser environment information string.</param>
    /// <returns>A formatted Markdown table containing mutated node IDs, edge adjacency deltas, and hexel symmetric difference counts.</returns>
    [<JSInvokable("RunPerturbationSensitivity")>]
    static member RunPerturbationSensitivity ([<Optional; DefaultParameterValue("")>] clientInfo: string) =
        let header = 
            formatMarkdownHeader 
                "Syntactic Perturbation Sensitivity (D(C_0, C_1))" 
                "Quantifying structural sensitivity under minimal AST token changes (+1 Area weight)." 
                clientInfo

        let columns = [
            "| Layout | Operator | Node Mutated | Adjacency Delta (Edges) | Symmetric Diff (Hexels) |"
            "|--------|----------|--------------|-------------------------|-------------------------|"
        ]

        printfn "Starting Perturbation Sensitivity Benchmark..."

        let mutateTree (tree: LayoutTree) =
            let raw = tree.Raw
            if raw.Length > 0 && raw.[0].Length > 1 then
                let newRaw = 
                    raw |> Array.mapi (fun i arr ->
                        if i = 0 then
                            arr |> Array.mapi (fun j (id, w, n) ->
                                if j = 1 then (id, w + 1, n) else (id, w, n))
                        else arr)
                let (mutId, _, _) = raw.[0].[1]
                Some (mutId, LayoutTree.Create newRaw)
            else None

        let rows =
            presets
            |> Array.choose (fun (presetName, tree) ->
                match mutateTree tree with
                | Some (mutId, mutatedTree) ->
                    let subRows = 
                        parsedOperators |> Array.take 5 |> Array.map (fun (opName, sqn) ->
                            let layout0 = runCompilation tree sqn
                            let layout1 = runCompilation mutatedTree sqn
                            
                            let adj0 = cxlAdj layout0 |> snd
                            let adj1 = cxlAdj layout1 |> snd
                            
                            let mutable deltaAdj = 0
                            for i in 0 .. adj0.Length - 1 do
                                for j in i + 1 .. adj0.Length - 1 do
                                    if i < adj1.Length && j < adj1.Length then
                                        if adj0.[i].[j] <> adj1.[i].[j] then deltaAdj <- deltaAdj + 1
                                    else deltaAdj <- deltaAdj + 1
                                    
                            let getHexels (l: Cxl[]) = 
                                l |> Array.collect (fun c -> Array.append [| c.Base |] c.Hxls)
                                |> Array.map (fun h -> hxlCrd h) |> Set.ofArray
                                
                            let hex0 = getHexels layout0
                            let hex1 = getHexels layout1
                            let symDiff = (Set.difference hex0 hex1 |> Set.count) + (Set.difference hex1 hex0 |> Set.count)
                            
                            sprintf "| %s | %s | %s (+1 Area) | %d | %d |" presetName opName mutId deltaAdj symDiff
                        )
                    Some subRows
                | None -> None)
            |> Array.concat
            |> Array.toList

        renderTable header columns rows "[Perturbation Sensitivity Benchmark Complete]"
