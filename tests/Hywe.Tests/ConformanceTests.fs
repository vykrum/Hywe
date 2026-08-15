module Hywe.Tests.ConformanceTests

open Xunit
open FsUnit
open Hywe.Core.Hexel
open Hywe.Core.Coxel
open Hywe.Core.Xyxel

// We are proving two claims for a specific Operator (e.g. VRCWEE)
// 1. Integer Constancy: A generated Coxel's area exactly matches the scaled requested area.
// 2. Absolute Determinism: Multiple runs produce identical Layout results.

[<Fact>]
let ``VRCWEE Operator should rigidly allocate exact Coxel counts (Integer Constancy)`` () =
    // Arrange: Define standard program
    let tree = LayoutTree.Create [|
        [| ("A", 20, "Room A"); ("B", 30, "Room B") |]
    |]
    
    // We create a large enough boundary so ratio scaling doesn't squeeze it
    let opts = {
        EntryFallback = "0,0"
        InitialOcc = [||]
        Seq = Some VRCWEE
        Width = Some 100
        Height = Some 100
        OuterStr = None
        IslandsStr = None
        ParentCxl = None
        Ratio = Some 1.0 // 1:1 mapping for area
        Elevation = Some 0
    }
    
    let attrs = Map.empty
    let ctx = prepareLayoutContext attrs tree opts
    
    // Act: Generate Layout
    let baseOpt = generateBaseCxl ctx
    baseOpt |> should not' (equal None)
    
    let (baseCxl, nextOcc) = baseOpt.Value
    let layout, _, _ = generateCxlLayout ctx baseCxl nextOcc
    
    // Assert: Integer Constancy
    // The requested areas were 20 and 30, with ratio 1.0.
    // The engine's allocation algorithm generates X child hexels for an area of X, resulting in X + 1 total hexels.
    // Base hexel + Hxls array length = total hexels for the coxel.
    let countHexels (c: Cxl) = c.Hxls.Length + 1
    
    let cxlA = layout |> Array.find (fun c -> prpVlu c.Rfid = "A")
    let cxlB = layout |> Array.find (fun c -> prpVlu c.Rfid = "B")
    
    countHexels cxlA |> should equal (20 + 1)
    countHexels cxlB |> should equal (30 + 1)

[<Fact>]
let ``VRCWEE Operator should be absolutely deterministic across multiple runs`` () =
    // Arrange
    let tree = LayoutTree.Create [|
        [| ("A", 20, "Room A"); ("B", 30, "Room B") |]
    |]
    
    let opts = {
        EntryFallback = "0,0"
        InitialOcc = [||]
        Seq = Some VRCWEE
        Width = Some 100
        Height = Some 100
        OuterStr = None
        IslandsStr = None
        ParentCxl = None
        Ratio = Some 1.0
        Elevation = Some 0
    }
    
    let ctx = prepareLayoutContext Map.empty tree opts
    
    // Act
    let run () = 
        let (baseCxl, nextOcc) = (generateBaseCxl ctx).Value
        let layout, _, _ = generateCxlLayout ctx baseCxl nextOcc
        layout |> Array.map (fun c -> getCxlCoordsString c) // Serialize layout state to string for easy comparison
        
    let result1 = run ()
    let result2 = run ()
    let result3 = run ()
    
    // Assert
    result1 |> should equal result2
    result2 |> should equal result3
