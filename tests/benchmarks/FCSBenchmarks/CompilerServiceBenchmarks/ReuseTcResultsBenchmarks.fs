namespace FSharp.Compiler.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open FSharp.Compiler.CodeAnalysis
open FSharp.Test.ProjectGeneration
open FSharp.Benchmarks.Common.Categories

[<MemoryDiagnoser>]
[<BenchmarkCategory(ShortCategory)>]
type ReuseTcResultsBenchmarks() =

    let getShortId() = Guid.NewGuid().ToString().[..7]

    // Temporary directory is TempPath + "/FSharp.Test.Utilities/xxxxxxx/"
    let tempDirectoryOfThisTestRun() =
        let temp = Path.GetTempPath()
        DirectoryInfo(temp).CreateSubdirectory($"FSharp.Test.Utilities/{getShortId()}")

    let createTemporaryDirectory () =
        tempDirectoryOfThisTestRun()
            .CreateSubdirectory($"{getShortId()}")

    let getTemporaryFileName () =
        createTemporaryDirectory().FullName ++ getShortId()

    member val Benchmark = Unchecked.defaultof<_> with get, set

    member this.setup(project) =
        let checker = FSharpChecker.Create()
        this.Benchmark <- ProjectWorkflowBuilder(project, checker = checker).CreateBenchmarkBuilder()
        saveProject project false checker |> Async.RunSynchronously

    [<Benchmark>]
    member this.NoReuse() = 
        let tempPath = $"{getTemporaryFileName()}.fsx"
        let code = "printfn \"Hello world!\""
        File.WriteAllText(tempPath, code) 

        let options = [
            "--compressmetadata-"
            "--optimize-"
        ]

        let arguments =
            [|
                yield "fsc.exe"
                yield! options
                yield! [tempPath]
            |]



        let checker = FSharpChecker.Create()
        let _r = 
            checker.Compile(arguments)
            |> Async.RunSynchronously

        let _r = 
            checker.Compile(arguments)
            |> Async.RunSynchronously

        true

    [<Benchmark>]
    member this.Reuse() =
        let tempPath = $"{getTemporaryFileName()}.fsx"
        let code = "printfn \"Hello world!\""
        File.WriteAllText(tempPath, code) 

        let options = [
            "--compressmetadata-"
            "--optimize-"
            "--reusetypecheckingresults"
        ]

        let arguments =
            [|
                yield "fsc.exe"
                yield! options
                yield! [tempPath]
            |]

        let checker = FSharpChecker.Create()
        let _r = 
            checker.Compile(arguments)
            |> Async.RunSynchronously

        let _r = 
            checker.Compile(arguments)
            |> Async.RunSynchronously

        true
