namespace Hywe.Web

open System
open System.Diagnostics
open Microsoft.JSInterop
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

module Benchmarks =

    let calculateSD (values: seq<float>) =
        let count = values |> Seq.length |> float
        if count <= 1.0 then 0.0 else
        let avg = values |> Seq.average
        let sumOfSquares = values |> Seq.sumBy (fun v -> (v - avg) ** 2.0)
        sqrt (sumOfSquares / count)

    let operators = [|
        "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
        "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
    |]

    let presets = [|
        "Simple", LayoutTree.Create [| [| ("1", 105, "Dock"); ("1.1", 85, "Logistics"); ("1.2", 95, "Lab"); ("1.3", 65, "Habitation"); ("1.4", 75, "Power") |] |]
        "Branched", LayoutTree.Create [| [| ("1", 12, "Foyer"); ("1.1", 12, "Living"); ("1.1.1", 18, "Dining"); ("1.1.1.1", 15, "Kitchen"); ("1.1.1.1.1", 6, "Utility"); ("1.1.1.2", 14, "Bed-1"); ("1.1.1.2.1", 8, "Bath-1"); ("1.1.1.3", 18, "Bed-2"); ("1.1.1.3.1", 10, "Closet-2"); ("1.1.1.3.1.1", 10, "Bath-2"); ("1.1.1.4", 18, "Bed-3"); ("1.1.1.4.1", 11, "Closet-3"); ("1.1.1.4.2", 10, "Bath-3"); ("1.1.2", 12, "Staircase"); ("1.2", 12, "Study") |] |]
        "Stacked", LayoutTree.Create [| [| ("1", 75, "Lobby"); ("1.1", 88, "Retail"); ("1.2", 54, "Toilets"); ("1.3", 67, "Retail"); ("1.4", 94, "Retail") |]; [| ("1", 75, "Lobby"); ("1.1", 43, "Office"); ("1.2", 123, "Office"); ("1.2.1", 34, "Toilets"); ("1.3", 52, "Office") |]; [| ("1", 75, "Lobby"); ("1.1", 99, "Suite") |] |]
    |]

    let runCompilation (tree: LayoutTree) (operatorName: string) =
        let sqn = 
            match tryParseUnion<Sqn> operatorName with
            | Some s -> s
            | None -> failwith "Invalid operator name"

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
        let baseOpt = generateBaseCxl ctx
        match baseOpt with
        | Some (baseCxl, nextOcc) -> 
            let layout, _, _ = generateCxlLayout ctx baseCxl nextOcc
            layout
        | None -> failwithf "Failed to generate base hexel for %s" operatorName

    type BenchmarkRunner() =
        [<JSInvokable("RunPerformanceBenchmark")>]
        static member RunPerformanceBenchmark () =
            let sb = System.Text.StringBuilder()
            sb.AppendLine("### Performance Benchmark — Production / WebAssembly (hywe.in)") |> ignore
            sb.AppendLine("") |> ignore
            sb.AppendLine("- **Note**: Release build metrics gathered via browser execution on WebAssembly.") |> ignore
            sb.AppendLine("- **Runtime**: WebAssembly (Mono)") |> ignore
            sb.AppendLine("- **Build Configuration**: Release") |> ignore
            sb.AppendLine("") |> ignore
            sb.AppendLine("| Layout | Operator | Iterations | Min Latency (ms) | Max Latency (ms) | Avg Latency (ms) | SD (ms) | Total Time (ms) |") |> ignore
            sb.AppendLine("|--------|----------|------------|------------------|------------------|------------------|---------|-----------------|") |> ignore

            printfn "Starting Performance Benchmark (Takes ~3-4 minutes)..."
            
            let iterations = 10
            for layoutName, tree in presets do
                printfn "-> Processing Layout: %s..." layoutName
                for op in operators do
                    let sw = Stopwatch()
                    let times = ResizeArray<float>()

                    for i in 1 .. iterations do
                        sw.Restart()
                        runCompilation tree op |> ignore
                        sw.Stop()
                        times.Add(sw.Elapsed.TotalMilliseconds)

                    let minT = times |> Seq.min
                    let maxT = times |> Seq.max
                    let avgT = times |> Seq.average
                    let sdT = calculateSD times
                    let sumT = times |> Seq.sum

                    sb.AppendLine(sprintf "| %s | %s | %d | %.2f | %.2f | %.2f | %.2f | %.2f |" layoutName op iterations minT maxT avgT sdT sumT) |> ignore
                    GC.Collect()
            
            sb.AppendLine("\n[Benchmark Complete. Copy the table above into your wiki!]") |> ignore
            sb.ToString()
            
        [<JSInvokable("RunConformanceTests")>]
        static member RunConformanceTests () =
            let sb = System.Text.StringBuilder()
            sb.AppendLine("# Repeatability") |> ignore
            sb.AppendLine("Validation of exact reproducibility of topology signatures for canonical inputs across repeated executions.") |> ignore
            sb.AppendLine("") |> ignore
            sb.AppendLine("| Layout | Operator | Iterations | Signatures Match | Valid States | Topology Signature Hash |") |> ignore
            sb.AppendLine("|--------|----------|------------|------------------|--------------|-------------------------|") |> ignore
            
            printfn "Starting Conformance (Repeatability) Benchmark..."
            let iterations = 10
            
            for presetName, tree in presets do
                printfn "-> Checking Conformance on Layout: %s..." presetName
                for op in operators do
                    let mutable validCount = 0
                    let signatures = ResizeArray<string>()
                    
                    for i in 1 .. iterations do
                        try
                            let layout = runCompilation tree op
                            let sigStr = layout |> Array.map getCxlCoordsString |> String.concat "|"
                            signatures.Add(sigStr)
                            validCount <- validCount + 1
                        with _ -> ()
                        
                    let validStatesStr = sprintf "%d%%" (validCount * 100 / iterations)
                    
                    if validCount > 0 then
                        let firstSig = signatures.[0]
                        let sigsMatch = (signatures |> Seq.forall (fun s -> s = firstSig)).ToString().ToLower()
                        let hashStr = sprintf "%X" (abs (hash firstSig))
                        sb.AppendLine(sprintf "| %s | %s | %d | %s | %s | `%s` |" presetName op iterations sigsMatch validStatesStr hashStr) |> ignore
                    else
                        sb.AppendLine(sprintf "| %s | %s | %d | false | %s | `N/A` |" presetName op iterations validStatesStr) |> ignore
                    
                    GC.Collect()
                    
            sb.AppendLine("\n[Conformance Benchmark Complete]") |> ignore
            sb.ToString()

        [<JSInvokable("RunQualityBenchmarks")>]
        static member RunQualityBenchmarks () =
            let sb = System.Text.StringBuilder()
            sb.AppendLine("=== RUNNING QUALITY BENCHMARKS ===") |> ignore
            
            for presetName, tree in presets do
                sb.AppendLine(sprintf "\n--- Preset: %s ---" presetName) |> ignore
                let requiredAdjacencies = 
                    tree.Raw |> Array.concat |> Array.choose (fun (id, _, _) ->
                        let parts = id.Split('.')
                        if parts.Length > 1 then
                            let parentId = parts.[0 .. parts.Length - 2] |> String.concat "."
                            Some (id, parentId)
                        else None)
                
                for op in operators do
                    let layout = runCompilation tree op
                    let allHexels = layout |> Array.collect (fun c -> Array.append [|c.Base|] c.Hxls)
                    let xs = allHexels |> Array.map (fun h -> let (x, _, _) = hxlCrd h in x)
                    let ys = allHexels |> Array.map (fun h -> let (_, y, _) = hxlCrd h in y)
                    
                    let width = (Array.max xs) - (Array.min xs)
                    let height = (Array.max ys) - (Array.min ys)
                    let compactnessArea = width * height
                    
                    let _, matrix = cxlAdj layout
                    
                    let mutable satisfied = 0
                    let mutable possible = 0
                    for (id1, id2) in requiredAdjacencies do
                        let i1 = layout |> Array.tryFindIndex (fun c -> (prpVlu c.Rfid) = id1)
                        let i2 = layout |> Array.tryFindIndex (fun c -> (prpVlu c.Rfid) = id2)
                        match i1, i2 with
                        | Some a, Some b -> 
                            possible <- possible + 1
                            if matrix.[a].[b] then satisfied <- satisfied + 1
                        | _ -> ()
                    
                    let adjacencyScore = if possible = 0 then 100.0 else (float satisfied) / (float possible) * 100.0
                    sb.AppendLine(sprintf "[%s] Compactness: %d | Adjacency Score: %.1f%%" op compactnessArea adjacencyScore) |> ignore
                    GC.Collect()
                
            sb.AppendLine("\n=== QUALITY BENCHMARKS COMPLETE ===") |> ignore
            sb.ToString()

        [<JSInvokable("RunScalingBenchmarks")>]
        static member RunScalingBenchmarks () =
            let sb = System.Text.StringBuilder()
            sb.AppendLine("### Scaling Benchmark (WASM)") |> ignore
            sb.AppendLine("| Scale (Nodes) | Operator | Iterations | Min Latency (ms) | Max Latency (ms) | Avg Latency (ms) | SD (ms) |") |> ignore
            sb.AppendLine("|---------------|----------|------------|------------------|------------------|------------------|---------|") |> ignore
            printfn "Starting Scaling Benchmark..."
            
            let iterations = 10
            let nodeCounts = [| 10; 50; 100; 250; 500; 750; 1000 |]
            
            for count in nodeCounts do
                printfn "-> Processing scale: %d nodes..." count
                let genNodes = Array.init count (fun i -> (if i = 0 then "1" else sprintf "1.%d" i), 50, "Node")
                let tree = LayoutTree.Create [| genNodes |]
                
                for op in operators do
                    let sw = Stopwatch()
                    let times = ResizeArray<float>()
                    
                    for i in 1 .. iterations do
                        sw.Restart()
                        runCompilation tree op |> ignore
                        sw.Stop()
                        times.Add(sw.Elapsed.TotalMilliseconds)
                        
                    let minT = times |> Seq.min
                    let maxT = times |> Seq.max
                    let avgT = times |> Seq.average
                    let sdT = calculateSD times
                    
                    sb.AppendLine(sprintf "| %d | %s | %d | %.2f | %.2f | %.2f | %.2f |" count op iterations minT maxT avgT sdT) |> ignore
                    GC.Collect()
                
            sb.AppendLine("\n[Scaling Benchmark Complete]") |> ignore
            sb.ToString()
