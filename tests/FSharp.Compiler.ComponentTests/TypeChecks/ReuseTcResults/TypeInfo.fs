namespace TypeChecks.ReuseTcResultsTests

open System.IO

open FSharp.Test
open FSharp.Test.Compiler

open Xunit

open TestFramework
open FSharp.Test


type TypeInfo() =

    let tempPath = $"{getTemporaryFileName()}.fsx"

    [<Fact>]
    let ``Recompilation``() =
        File.WriteAllText(tempPath, "42") 

        let cUnit =
            FsxFromPath tempPath
            |> withReuseTcResults
            |> withOptions [ "--compressmetadata-" ]
            |> withOptions [ "--checknulls-" ]

        let _r1 =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        let _r2 =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        Assert.True(true)
