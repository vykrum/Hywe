namespace Hywe.Core.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Hywe.Core
open Hywe.Core.Hexel

[<MemoryDiagnoser>]
type CoreBenchmarks() =

    let sqn = Hexel.Sqn.VRCWEE
    let livingSeed = Hexel.Hxl.AV(0, 0, 0) |> Hexel.hxlVld sqn
    let diningSeed = Hexel.Hxl.AV(8, 0, 0) |> Hexel.hxlVld sqn
    let program = [|
        (livingSeed, Coxel.Refid "1.0", Coxel.Count 15, Coxel.Label "Living")
        (diningSeed, Coxel.Refid "2.0", Coxel.Count 10, Coxel.Label "Dining")
    |]

    let hyweCode =
        "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
        "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

    [<Benchmark>]
    member _.CoxelCluster() =
        Coxel.coxel sqn 0 program [||]

    [<Benchmark>]
    member _.ZaxelMultiLevel() =
        Zaxel.generateMultiLevelLayout hyweCode "0,0" [||] None None None

module Program =
    [<EntryPoint>]
    let main argv =
        BenchmarkSwitcher.FromAssembly(typeof<CoreBenchmarks>.Assembly).Run(argv) |> ignore
        0
