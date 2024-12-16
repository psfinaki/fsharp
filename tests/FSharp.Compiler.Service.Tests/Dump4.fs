module FSharp.Compiler.Service.Tests.Dump4

open System.IO
open System.Text

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.IO
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle
open FSharp.Compiler.TypedTreeOps

open Internal.Utilities.Collections
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

open Xunit

let magicFunction1 = id
let magicFunction2 = id
let magicFunction3 = id
let magicFunction4 = id

[<Fact>]
let Signatures() =
    let originalCode = "printfn \"hello world\""

    let signatureData = magicFunction1 originalCode

    let encodedSignatureData = magicFunction2 signatureData

    let decodedSignatureData = magicFunction3 encodedSignatureData

    let resultingCode = magicFunction4 decodedSignatureData

    Assert.Equal(originalCode, resultingCode)