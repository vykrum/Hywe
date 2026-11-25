namespace Hywe.Core.Tests

open Xunit
open FsCheck.Xunit
open Hywe.Core
open Hywe.Core.Hexel

module HexelTests =

    [<Fact>]
    let ``All 24 canonical sequences have 7 offsets starting at origin`` () =
        Assert.Equal(24, Hexel.sqnArray.Length)
        Hexel.sqnArray
        |> Array.iter (fun sqn ->
            let offsets = Hexel.sequence sqn
            Assert.Equal(7, offsets.Length)
            Assert.Equal((0, 0), offsets.[0]))

    [<Fact>]
    let ``adjacent returns 7 cells with host cell first`` () =
        let host = Hxl.AV(0, 0, 0)
        Hexel.sqnArray
        |> Array.iter (fun sqn ->
            let neighbors = Hexel.adjacent sqn host
            Assert.Equal(7, neighbors.Length)
            Assert.Equal(Hexel.hxlCrd host, Hexel.hxlCrd neighbors.[0]))

    [<Property>]
    let ``hxlVld is idempotent across all sequences`` (x: int) (y: int) (z: int) =
        let clampedX = (abs x % 200) - 100
        let clampedY = (abs y % 200) - 100
        let clampedZ = abs z % 10
        let raw = Hxl.AV(clampedX, clampedY, clampedZ)
        Hexel.sqnArray
        |> Array.forall (fun sqn ->
            let once = Hexel.hxlVld sqn raw
            let twice = Hexel.hxlVld sqn once
            once = twice)

    [<Fact>]
    let ``toBase34 encodes zero and positive integers without I or O`` () =
        Assert.Equal("0", Hexel.toBase34 0L)
        let encoded = [| 0L .. 200L |] |> Array.map Hexel.toBase34
        encoded
        |> Array.iter (fun s ->
            Assert.DoesNotContain("I", s)
            Assert.DoesNotContain("O", s))
