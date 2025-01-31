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
        this.setup
            { SyntheticProject.Create() with
                SourceFiles =
                    [
                        { 
                              Id = "Test"
                              PublicVersion = 1
                              InternalVersion = 1
                              DependsOn = []
                              FunctionName = "f"
                              SignatureFile = No
                              HasErrors = false
                              Source = "module Test"
                              ExtraSource = ""
                              EntryPoint = false
                              IsPhysicalFile = true 
                        }
                    ]
                OtherOptions =
                    [
                        "--compressmetadata-"
                        "--optimize-"
                    ]
                SkipInitialCheck = true
                AutoAddModules = false
            }

        this.Benchmark { 
            compileWithFSC 
            compileWithFSC 
        }

    [<Benchmark>]
    member this.Reuse() =
        this.setup
            { SyntheticProject.Create() with
                SourceFiles =
                    [
                        { 
                              Id = "Test"
                              PublicVersion = 1
                              InternalVersion = 1
                              DependsOn = []
                              FunctionName = "f"
                              SignatureFile = No
                              HasErrors = false
                              Source = "module Test"
                              ExtraSource = ""
                              EntryPoint = false
                              IsPhysicalFile = true 
                        }
                    ]
                OtherOptions =
                    [
                        "--compressmetadata-"
                        "--optimize-"
                        "--reusetypecheckingresults"
                    ]
                SkipInitialCheck = true
                AutoAddModules = false
            }

        this.Benchmark { 
            compileWithFSC 
            compileWithFSC 
        }
