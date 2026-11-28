namespace Hywe.Core.Tests

open Xunit
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Core.Goxel

module GoxelTests =

    [<Fact>]
    let ``polygonWithHolesArea subtracts interior courtyard hole`` () =
        let outer = [| (0, 0); (30, 0); (30, 30); (0, 30); (0, 0) |]
        let hole  = [| (10, 10); (20, 10); (20, 20); (10, 20); (10, 10) |]
        let outerArea = Goxel.polygonArea outer
        let netArea = Goxel.polygonWithHolesArea outer [| hole |]
        Assert.Equal(900L, outerArea)
        Assert.Equal(800L, netArea)

    [<Fact>]
    let ``pointInPolygon and bounds behave consistently`` () =
        let openSquare = [| (0, 0); (20, 0); (20, 20); (0, 20) |]
        let square = [| (0, 0); (20, 0); (20, 20); (0, 20); (0, 0) |]
        Assert.Equal((0, 0, 20, 20), Goxel.bounds square)
        Assert.Equal((10, 10), Goxel.centroid openSquare)
        Assert.True(Goxel.pointInPolygon (10, 10) square)
        Assert.False(Goxel.pointInPolygon (25, 25) square)

    [<Fact>]
    let ``hxlLin and hxlPgn produce non-empty discrete hexel arrays`` () =
        let sqn = Hexel.Sqn.VRCWEE
        let p0 = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
        let p1 = Hexel.Hxl.AV(10, 8, 0) |> Hexel.hxlVld sqn
        let line = Goxel.hxlLin sqn 0 p0 p1
        Assert.NotEmpty(line)

        let poly = [| (0, 0); (20, 0); (20, 20); (0, 20); (0, 0) |]
        let filled = Goxel.hxlPgn sqn 0 poly
        Assert.NotEmpty(filled)

    [<Fact>]
    let ``cleanPolygon2x produces closed polygon and preserves 2x coordinates`` () =
        let raw2x = [| (0, 0); (1, 0); (1, 0); (20, 0); (20, 20); (0, 20) |]
        let cleaned = Goxel.cleanPolygon2x Hexel.Sqn.VRCWEE raw2x
        Assert.True(cleaned.Length >= 4)
        Assert.Equal(cleaned.[0], cleaned.[cleaned.Length - 1])
