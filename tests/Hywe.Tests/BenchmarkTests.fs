module Hywe.Tests.BenchmarkTests

open System
open Xunit
open Xunit.Abstractions
open FsUnit
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

type BenchmarkTests(output: ITestOutputHelper) =
    
    let operators = [|
        "VRCWEE"; "VRCCEE"; "VRCWSE"; "VRCCSE"; "VRCWSW"; "VRCCSW"; "VRCWWW"; "VRCCWW"; "VRCWNW"; "VRCCNW"; "VRCWNE"; "VRCCNE";
        "HRCWNN"; "HRCCNN"; "HRCWNE"; "HRCCNE"; "HRCWSE"; "HRCCSE"; "HRCWSS"; "HRCCSS"; "HRCWSW"; "HRCCSW"; "HRCWNW"; "HRCCNW"
    |]

    // Required Adjacencies (ID pairs)
    let requiredAdjacencies = [
        ("Living", "Kitchen")
        ("Living", "Bed1")
        ("Bath", "Bed1")
        ("Bath", "Bed2")
    ]

    [<Theory>]
    [<InlineData("VRCWEE")>]
    [<InlineData("VRCCEE")>]
    [<InlineData("VRCWSE")>]
    [<InlineData("VRCCSE")>]
    [<InlineData("VRCWSW")>]
    [<InlineData("VRCCSW")>]
    [<InlineData("VRCWWW")>]
    [<InlineData("VRCCWW")>]
    [<InlineData("VRCWNW")>]
    [<InlineData("VRCCNW")>]
    [<InlineData("VRCWNE")>]
    [<InlineData("VRCCNE")>]
    [<InlineData("HRCWNN")>]
    [<InlineData("HRCCNN")>]
    [<InlineData("HRCWNE")>]
    [<InlineData("HRCCNE")>]
    [<InlineData("HRCWSE")>]
    [<InlineData("HRCCSE")>]
    [<InlineData("HRCWSS")>]
    [<InlineData("HRCCSS")>]
    [<InlineData("HRCWSW")>]
    [<InlineData("HRCCSW")>]
    [<InlineData("HRCWNW")>]
    [<InlineData("HRCCNW")>]
    member this.``Benchmark Residential Canonical Problem`` (operatorName: string) =
        // 1. Arrange: Define the canonical residential problem
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
        
        // 2. Act: Generate the layout
        let (baseCxl, nextOcc) = (generateBaseCxl ctx).Value
        let layout, _, _ = generateCxlLayout ctx baseCxl nextOcc
        
        // 3. Evaluate: Calculate metrics
        // A. Compactness (Bounding Box Area)
        let allHexels = layout |> Array.collect (fun c -> Array.append [|c.Base|] c.Hxls)
        let xs = allHexels |> Array.map (fun h -> let (x, _, _) = hxlCrd h in x)
        let ys = allHexels |> Array.map (fun h -> let (_, y, _) = hxlCrd h in y)
        
        let width = (Array.max xs) - (Array.min xs)
        let height = (Array.max ys) - (Array.min ys)
        let compactnessArea = width * height

        // B. Adjacency Score
        let names, matrix = cxlAdj layout
        
        let indexOf (id: string) = 
            // the name array returned by cxlAdj contains the prpVlu result which is the Label or Id depending on structure.
            // Since we use the Label in LayoutTree, it maps to Name = Label inside Cxl
            layout |> Array.findIndex (fun c -> (prpVlu c.Rfid) = id)

        let mutable satisfied = 0
        for (id1, id2) in requiredAdjacencies do
            let i1 = indexOf id1
            let i2 = indexOf id2
            if matrix.[i1].[i2] then
                satisfied <- satisfied + 1
        
        let adjacencyScore = (float satisfied) / (float requiredAdjacencies.Length) * 100.0
        
        // 4. Output Results
        output.WriteLine(sprintf "--- %s RESULTS ---" operatorName)
        output.WriteLine(sprintf "Compactness (Bounding Box Area): %d" compactnessArea)
        output.WriteLine(sprintf "Adjacency Score: %.1f%%" adjacencyScore)
        
        // This is an experimental instrument, not a pass/fail unit test. 
        // We assert true just to get the runner to complete and log the metrics.
        true |> should be True
