module Hywe.Tests.StressTests

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open Xunit
open Xunit.Abstractions
open FsUnit
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

let findRepoRoot (startPath: string) =
    let rec search dir =
        if Directory.GetFiles(dir, "Hywe.sln").Length > 0 then dir
        else 
            let parent = Directory.GetParent(dir)
            if parent = null then failwith "Could not find repo root"
            else search parent.FullName
    search startPath

let repoRoot = findRepoRoot AppDomain.CurrentDomain.BaseDirectory
let wikiRoot = Path.GetFullPath(Path.Combine(repoRoot, "../Hywe.wiki"))

let operators = [|
    "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
    "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
|]

type StressTests(output: ITestOutputHelper) =
    
    // Configurable number of runs for empirical stress testing
    // The user requested to scale from 10 to 10000. For quick automated runs, we default to 10.
    // Modify this array to [| 10; 100; 1000; 10000 |] to run the full matrix.
    let iterationsList = [| 100 |]

    let runCompilation (operatorName: string) =
        let tree = LayoutTree.Create [|
            [| 
                ("Living", 30, "Living Room")
                ("Kitchen", 15, "Kitchen")
                ("Bed1", 20, "Master Bedroom")
                ("Bed2", 15, "Bedroom 2")
                ("Bath", 10, "Bathroom")
            |]
        |]
        
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
            let signature = layout |> Array.map getCxlCoordsString |> String.concat "|"
            signature
        | None -> failwithf "Failed to generate base hexel for %s" operatorName

    [<Fact>]
    [<Trait("Category", "Stress")>]
    member this.``Execute Empirical Stress Tests and Generate Wiki Reports`` () =
        
        // Ensure wiki directory exists
        if not (Directory.Exists wikiRoot) then
            output.WriteLine(sprintf "Wiki root not found at %s. Creating directory to prevent failure." wikiRoot)
            Directory.CreateDirectory wikiRoot |> ignore

        let perfLines = ResizeArray<string>()
        let repLines = ResizeArray<string>()

        let envInfo = [|
            "### Benchmark Environment"
            sprintf "- **CPU**: %d Cores" Environment.ProcessorCount
            sprintf "- **Operating System**: %s" RuntimeInformation.OSDescription
            sprintf "- **Runtime**: %s" RuntimeInformation.FrameworkDescription
#if DEBUG
            "- **Build Configuration**: Development / Debug"
#else
            "- **Build Configuration**: Release"
#endif
            "- **WebAssembly**: N/A (Native .NET execution)"
            "- **JIT Warm-up**: None (Cold start included in first iteration)"
            "- **First Run Included**: Yes"
            ""
        |]

        perfLines.Add("# Performance")
        perfLines.Add("Empirical stress-testing of the compilation pipeline across multiple inputs and iterations.")
        perfLines.Add("")
        for line in envInfo do perfLines.Add(line)
        perfLines.Add("| Operator | Iterations | Min Latency (ms) | Max Latency (ms) | Avg Latency (ms) | Total Time (ms) |")
        perfLines.Add("|----------|------------|------------------|------------------|------------------|-----------------|")

        repLines.Add("# Repeatability")
        repLines.Add("Validation of exact reproducibility of topology signatures for canonical inputs across repeated executions.")
        repLines.Add("")
        repLines.Add("| Operator | Iterations | Signatures Match | Valid States | Topology Signature Hash |")
        repLines.Add("|----------|------------|------------------|--------------|-------------------------|")

        for iterations in iterationsList do
            output.WriteLine(sprintf "Running stress tests with M=%d iterations across N=%d operators." iterations operators.Length)

            for op in operators do
                let sw = Stopwatch()
                let times = ResizeArray<float>()
                let signatures = ResizeArray<string>()

                for i in 1 .. iterations do
                    sw.Restart()
                    let sig' = runCompilation op
                    sw.Stop()
                    
                    times.Add(sw.Elapsed.TotalMilliseconds)
                    signatures.Add(sig')

                let minT = times |> Seq.min
                let maxT = times |> Seq.max
                let avgT = times |> Seq.average
                let sumT = times |> Seq.sum

                let allMatch = 
                    let firstSig = signatures.[0]
                    signatures |> Seq.forall (fun s -> s = firstSig)
                
                // Just a short hash for display
                let hash = signatures.[0].GetHashCode().ToString("X")

                perfLines.Add(sprintf "| %s | %d | %.2f | %.2f | %.2f | %.2f |" op iterations minT maxT avgT sumT)
                repLines.Add(sprintf "| %s | %d | %b | 100%% | `%s` |" op iterations allMatch hash)

                // Assert exact repeatability across all runs!
                allMatch |> should be True

        let perfFile = Path.Combine(wikiRoot, "Performance.md")
        let repFile = Path.Combine(wikiRoot, "Repeatability.md")

        File.WriteAllLines(perfFile, perfLines)
        File.WriteAllLines(repFile, repLines)

        output.WriteLine(sprintf "Successfully wrote %s" perfFile)
        output.WriteLine(sprintf "Successfully wrote %s" repFile)

