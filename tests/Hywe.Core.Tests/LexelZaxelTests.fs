namespace Hywe.Core.Tests

open Xunit
open Hywe.Core
open Hywe.Core.Lexel
open Hywe.Core.Nexel
open Hywe.Core.Zaxel

module LexelZaxelTests =

    [<Fact>]
    let ``parseAttributes and processFullString parse multi-level hywe spec`` () =
        let attrs = Lexel.parseAttributes "(Q=VRCWEE/L=0/X=1.0/W=40/H=30)"
        Assert.Equal("VRCWEE", attrs.Sequence)
        Assert.Equal(0, attrs.Level)
        Assert.Equal(Some 40.0, attrs.Width)
        Assert.Equal(Some 30.0, attrs.Height)

        let spec =
            "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
            "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"
        let blocks = Lexel.processFullString spec
        Assert.Equal(2, blocks.Length)

    [<Fact>]
    let ``generateMultiLevelLayout is deterministic for identical inputs`` () =
        let hyweCode =
            "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
            "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

        let rooms1, bounds1, elvs1, ratios1 =
            Zaxel.generateMultiLevelLayout hyweCode "0,0" [||] None None None
        let rooms2, bounds2, elvs2, ratios2 =
            Zaxel.generateMultiLevelLayout hyweCode "0,0" [||] None None None

        Assert.NotEmpty(rooms1)
        Assert.NotEmpty(elvs1)
        Assert.Equal<Coxel.Cxl>(rooms1, rooms2)
        Assert.Equal<float>(elvs1, elvs2)
        Assert.Equal<float>(ratios1, ratios2)
        Assert.Equal(bounds1.Length, bounds2.Length)

    [<Fact>]
    let ``Nexel marker helpers identify nest blocks`` () =
        Assert.True(Nexel.isNestMarker "N1")
        Assert.False(Nexel.isNestMarker "L0")
        Assert.Equal(Some 2, Nexel.getNestIndex "N2")
