namespace Hywe.Core.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Hywe.Core
open Hywe.Core.Hexel
open Hywe.Benchmarks.Cases

[<MemoryDiagnoser>]
type CoreBenchmarks() =

    let sqn = Hexel.Sqn.VRCWEE
    let presetMap = presets |> Map.ofArray
    let cif10 = presetMap.["CIF-10"]
    let radialStar = presetMap.["Radial-Star"]
    let deepSpine = presetMap.["Deep-Spine"]

    let hyweCode =
        "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)" +
        "L1(Q=HRCWNN/W=30/H=20)(2/15/MasterBed)(2.1/6/Ensuite)"

    [<Benchmark>]
    member _.Compile_CIF10() =
        runCompilation cif10 sqn

    [<Benchmark>]
    member _.Compile_RadialStar() =
        runCompilation radialStar sqn

    [<Benchmark>]
    member _.Compile_DeepSpine() =
        runCompilation deepSpine sqn

    [<Benchmark>]
    member _.Zaxel_MultiLevel() =
        Zaxel.generateMultiLevelLayout hyweCode "0,0" [||] None None None

module Program =
    [<EntryPoint>]
    let main argv =
        BenchmarkSwitcher.FromAssembly(typeof<CoreBenchmarks>.Assembly).Run(argv) |> ignore
        0
