﻿namespace TypeChecks.ReuseTcResultsTests

open System.IO

open FSharp.Test.Compiler

open Xunit

open TestFramework


type TypeInfo() =

    let tempPath = $"{getTemporaryFileName()}.fsx"

    [<Fact>]
    let ``Recompilation``() =
        File.WriteAllText(tempPath, "42")

        let cUnit =
            FsxFromPath tempPath
            |> withReuseTcResults

        cUnit
        |> compileExisting
        |> shouldSucceed
        |> ignore

        cUnit
        |> compileExisting
        |> shouldSucceed
        |> ignore

        Assert.True(true)
