open BenchmarkDotNet.Running
open FSharp.Compiler.Benchmarks
open BenchmarkDotNet.Configs
open FSharp.Benchmarks

[<EntryPoint>]
let main args =
    let b = GraphTypeCheckingBenchmarks.GraphTypeCheckingBenchmarks()
    b.SingleDependentChain_Setup()
    let r = b.SingleDependentChain()

    let cfg = ManualConfig.Create(DefaultConfig.Instance).WithOptions(ConfigOptions.DisableOptimizationsValidator)
    BenchmarkSwitcher.FromAssembly(typeof<DecentlySizedStandAloneFileBenchmark>.Assembly).Run(args,cfg) |> ignore
    0
