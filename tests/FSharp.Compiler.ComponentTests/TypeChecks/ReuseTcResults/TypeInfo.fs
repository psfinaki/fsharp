namespace TypeChecks.ReuseTcResultsTests

open System.IO

open FSharp.Test
open FSharp.Test.Compiler

open Xunit

open TestFramework


type TypeInfo() =

    [<Fact>]
    let ``Recompilation 1``() =
        let tempPath = $"{getTemporaryFileName()}.fsx"
        
        File.WriteAllText(tempPath, "42") 

        let cUnit =
            FsxFromPath tempPath
            |> withReuseTcResults
            |> withOptions [ "--compressmetadata-" ]

        let r1 =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        let r2 =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        Assert.Equal(r1, r2)

    [<Fact>]
    let ``Recompilation 2``() =
        let tempPath = $"{getTemporaryFileName()}.fsx"
        
        File.WriteAllText(tempPath, "printfn \"Hello world!\"") 

        let cUnit =
            FsxFromPath tempPath
            |> withReuseTcResults
            |> withOptions [ "--compressmetadata-" ]

        let r1 =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        let r2 =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        Assert.Equal(r1, r2)
