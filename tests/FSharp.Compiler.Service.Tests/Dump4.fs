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

let private toSignatureData code : (TcConfig * TcGlobals * CcuThunk) =
    failwith ""

let private encodeSignatureData (tcConfig, tcGlobals, ccuThunk) =
    CompilerImports.EncodeSignatureData(
        tcConfig,
        tcGlobals,
        Remap.Empty,
        ccuThunk,
        "",
        false)

let private decodeSignatureData = id

let private fromSignatureData = id

[<Fact>]
let Signatures() =
    let originalCode = "printfn \"hello world\""

    let signatureData = toSignatureData originalCode

    let encodedSignatureData = encodeSignatureData signatureData

    let decodedSignatureData = decodeSignatureData encodedSignatureData

    let resultingCode = fromSignatureData decodedSignatureData

    Assert.True(true)
    //Assert.Equal(originalCode, resultingCode)