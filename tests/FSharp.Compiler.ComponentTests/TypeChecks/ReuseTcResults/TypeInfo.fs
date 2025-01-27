namespace TypeChecks.ReuseTcResultsTests

open System.IO

open FSharp.Test
open FSharp.Test.Compiler

open Xunit

open TestFramework

[<Collection(nameof NotThreadSafeResourceCollection)>]
type TypeInfo() =

    let test (code: string) =
        let tempPath = $"{getTemporaryFileName()}.fsx"
        
        File.WriteAllText(tempPath, code) 

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
    let ``Empty file``() =
        test ""

    [<Fact>]
    let ``Empty function``() =
        test "()"
        
    [<Fact>]
    let ``42``() =
        test "42"
        
    [<Fact>]
    let ``exit``() =
        test "exit 0"

    [<Fact>]
    let ``hello world``() =
        test "printfn \"Hello world!\""
