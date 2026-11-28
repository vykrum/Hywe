namespace Hywe.Core.Tests

open Xunit
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Coxel

module CoxelTests =

    [<Fact>]
    let ``prpVlu extracts values from Label, Refid, and Count`` () =
        Assert.Equal("Living", Coxel.prpVlu (Coxel.Label "Living"))
        Assert.Equal("1.0", Coxel.prpVlu (Coxel.Refid "1.0"))
        Assert.Equal("15", Coxel.prpVlu (Coxel.Count 15))

    [<Fact>]
    let ``coxel produces non-overlapping rooms and symmetric adjacency matrix`` () =
        let sqn = Hexel.Sqn.VRCWEE
        let livingSeed = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
        let diningSeed = Hexel.Hxl.AV(4, 2, 0) |> Hexel.hxlVld sqn
        let program = [|
            (livingSeed, Coxel.Refid "1.0", Coxel.Count 15, Coxel.Label "Living")
            (diningSeed, Coxel.Refid "2.0", Coxel.Count 10, Coxel.Label "Dining")
        |]

        let rooms = Coxel.coxel sqn 0 program [||]
        Assert.Equal(2, rooms.Length)

        // All allocated hexels across rooms must have unique (x, y, z) coordinates
        let allCoords =
            rooms
            |> Array.collect (fun r -> Array.append [| r.Base |] r.Hxls |> Array.map Hexel.hxlCrd)
        let uniqueCoords = allCoords |> Array.distinct
        Assert.Equal(allCoords.Length, uniqueCoords.Length)

        // Adjacency matrix must be square and symmetric with false diagonal
        let labels, adj = Coxel.cxlAdj rooms
        Assert.Equal(2, labels.Length)
        Assert.Equal(2, adj.Length)
        Assert.False(adj.[0].[0])
        Assert.False(adj.[1].[1])
        Assert.Equal(adj.[0].[1], adj.[1].[0])

    [<Fact>]
    let ``cxlPrm returns non-empty boundary for synthesized room`` () =
        let sqn = Hexel.Sqn.VRCWEE
        let seed = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
        let rooms = Coxel.coxel sqn 0 [| (seed, Coxel.Refid "1", Coxel.Count 12, Coxel.Label "Hall") |] [||]
        let perimeter = Coxel.cxlPrm rooms.[0] 0
        Assert.NotEmpty(perimeter)

    [<Fact>]
    let ``cxlPrm2x preserves half-unit midpoints as odd 2x integer coordinates`` () =
        let sqn = Hexel.Sqn.VRCWEE
        let seed = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
        let rooms = Coxel.coxel sqn 0 [| (seed, Coxel.Refid "1", Coxel.Count 12, Coxel.Label "Hall") |] [||]
        let perimeter2x = Coxel.cxlPrm2x rooms.[0] 0
        Assert.NotEmpty(perimeter2x)
        // At least some boundary midpoints between adjacent hexels have odd sum (ox + ix) or (oy + iy)
        let hasOddHalfUnits = perimeter2x |> Array.exists (fun (x, y) -> abs (x % 2) = 1 || abs (y % 2) = 1)
        Assert.True(hasOddHalfUnits)
