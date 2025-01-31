namespace TypeChecks.ReuseTcResultsTests

open System.IO

open FSharp.Test
open FSharp.Test.Compiler

open Xunit

open TestFramework

[<Collection(nameof NotThreadSafeResourceCollection)>]
type Recompilation() =

    [<Theory>]
    [<InlineData "">]
    [<InlineData "()">]
    [<InlineData "42">]
    [<InlineData "exit 0">]
    [<InlineData "printfn \"Hello world!\"">]
    [<InlineData "module Test">]
    [<InlineData "namespace Test">]
    [<InlineData """namespace Test
module M1 = ()""">]
    //[<InlineData s"let x = 42">]
    //[<InlineData "let f() = 42">]
    [<InlineData "let f x = x">]
    //[<InlineData "let f x = x * x">]
    [<InlineData "type T = int">]
    //[<InlineData "type DU = A of int">]
    //[<InlineData "type DU = A of int | B of string">]
    //[<InlineData "type R = { v: int }">]
    //[<InlineData "Some 42">]
    //[<InlineData "None">]

    let ``Recompiles using restored TC info`` (code: string) =
        let tempPath = $"{getTemporaryFileName()}.fsx"
        
        File.WriteAllText(tempPath, code) 

        let cUnit =
            FsxFromPath tempPath
            |> withReuseTcResults
            |> withOptions [ "--compressmetadata-" ]
            |> withOptions [ "--optimize-" ]

        let expected =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        let actual =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        Assert.Equal(expected, actual)
