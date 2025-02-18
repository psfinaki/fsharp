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
    [<InlineData "let x = 42">]
    [<InlineData "let f x = x">]
    [<InlineData "let rec f x = x">]
    [<InlineData "type T = int">]
    [<InlineData "let f = function | _ -> 42">]
    [<InlineData "Some 42">]
    [<InlineData "None">]
    [<InlineData "42 |> Some">]
    [<InlineData "let myTuple = 42, true">]
    [<InlineData "let naturalNumbers = Seq.initInfinite id">]
    [<InlineData "let applyTwice f x = f (f x)">]
    [<InlineData "let mutable x = 42">]

    [<InlineData """namespace Test
module M1 = ()""">]
    [<InlineData """let f1 x = x
let rec f2 x = x""">]
    [<InlineData """let x = "Hello world!"
printfn $"{x}" """>]
    [<InlineData """let x = 42
printfn $"{x}" """>]
    [<InlineData """module M1
let helloWorld = "hello world!" """>]
    [<InlineData """module M2
printfn "hello world!" """>]
    [<InlineData """let mutable x = 42
x <- 43 """>]

    //[<InlineData "let x = []">]
    //[<InlineData "let f() = 42">]
    //[<InlineData "type DU = A of int">]
    //[<InlineData "type DU = A of int | B of string">]
    //[<InlineData "type R = { v: int }">]
    //[<InlineData "if true then ()">]
    //[<InlineData "if true then () else ()">]
    //[<InlineData "let add a b = a + b">]
    //[<InlineData "let addOne = fun x -> x + 1">]
    //[<InlineData "let square n = n * n">]

//    [<InlineData """
//let rec factorial n =
//    if n = 0 then 1
//    else n * factorial (n - 1) """>]
//    [<InlineData """
//match true with 
//| true -> ()
//| false -> () """>]
    let ``Recompiles using restored TC info`` (code: string) =
        let fileName = getTemporaryFileName()
        let tempPath = $"{fileName}.fsx"
        
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

        let outcome, _msg, _actualIL = 
            ILChecker.compareIL
                fileName 
                actual 
                [ expected ]

        Assert.True(outcome)

    [<Fact>]
    let ``Multiple files`` () =
        let tempDir = createTemporaryDirectory().FullName

        let code1 = """module M1
let helloWorld = "hello world!" """

        let code2 = """module M2
printfn $"{M1.helloWorld}" """

        let fileName1 = "File0"
        let fileName2 = "File1"

        let tempPath1 = tempDir ++ $"{fileName1}.fs"
        let tempPath2 = tempDir ++ $"{fileName2}.fs"

        File.WriteAllText(tempPath1, code1) 
        File.WriteAllText(tempPath2, code2) 

        let cUnit = 
            FsFromPath tempPath1
            |> withAdditionalSourceFile (SourceCodeFileKind.Create tempPath2)
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

        let outcome, _msg, _actualIL = 
            ILChecker.compareIL
                fileName1 
                actual 
                [ expected ]

        Assert.True(outcome)

    [<Theory>]
    [<InlineData(
        """
module M1
let test1 = 42 """, 
        """
module M2
let test2 = M1.test1 """)>]
    [<InlineData(
        """
module M1
let helloWorld = "hello world!" """, 
        """
module M2
printfn $"{M1.helloWorld}" """)>]
    let ``Multiple files - partial TC info reuse`` (code1: string) (code2: string) =
        let tempDir = createTemporaryDirectory().FullName

        let fileName1 = "File0"
        let fileName2 = "File1"

        let tempPath1 = tempDir ++ $"{fileName1}.fs"
        let tempPath2 = tempDir ++ $"{fileName2}.fs"

        File.WriteAllText(tempPath1, code1) 
        File.WriteAllText(tempPath2, code2) 

        let cUnit = 
            FsFromPath tempPath1
            |> withAdditionalSourceFile (SourceCodeFileKind.Create tempPath2)
            |> withReuseTcResults
            |> withNoInterfaceData
            |> withOptions [ "--compressmetadata-" ]
            |> withOptions [ "--optimize-" ]
            
        let expected =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        File.WriteAllText(tempPath2, code2) 

        let actual =
            cUnit
            |> compileExisting
            |> shouldSucceed
            |> fun r -> ILChecker.generateIL r.Output.OutputPath.Value []

        let outcome, _msg, _actualIL = 
            ILChecker.compareIL
                fileName1 
                actual 
                [ expected ]

        Assert.True(outcome)
