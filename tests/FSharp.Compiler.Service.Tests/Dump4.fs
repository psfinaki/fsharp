﻿module FSharp.Compiler.Service.Tests.Dump4

open System.IO
open System.Text

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.IO
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Driver
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle
open FSharp.Compiler.TypedTreeOps

open Internal.Utilities.Collections
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras
open Internal.Utilities.TypeHashing

open Xunit
open FSharp.Compiler.Syntax

let private toSignatureData (code: string) : (TcConfig * TcGlobals * CcuThunk) =
    // tcConfig
    
    let resolver = SimulatedMSBuildReferenceResolver.getResolver()
    let currentDir = Directory.GetCurrentDirectory()

    let builder = 
        TcConfigBuilder.CreateNew(
            resolver,
            currentDir,
            ReduceMemoryFlag.No,
            "",
            false,
            false,
            CopyFSharpCoreFlag.No,
            (fun _ -> None),
            None,
            Range.range0,
            compressMetadata = false
        )

    let tcConfig = TcConfig.Create(builder, false)
    
    /// tcGlobals

    let sysRes, otherRes, knownUnresolved =
        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)
    
    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
    let tcGlobals, frameworkTcImports = 
        TcImports.BuildFrameworkTcImports(
            foundationalTcConfigP,
            sysRes,
            otherRes) 
        |> Async.RunImmediate

    /// ccuThunk

    let logger = DiagnosticsLogger.AssertFalseDiagnosticsLogger

    let tcImports =
        TcImports.BuildNonFrameworkTcImports(
            foundationalTcConfigP,
            frameworkTcImports, 
            otherRes, 
            knownUnresolved, 
            new DependencyProvider())
        |> Async.RunImmediate

    let tcEnv0, openDecls0 =
        ParseAndCheckInputs.GetInitialTcEnv(
            "blah",
            rangeStartup,
            tcConfig, 
            tcImports,
            tcGlobals)
    
    File.WriteAllText("testblah.fs", code)
    let sourceFiles = ["testblah.fs"]

    let inputs =
        ParseAndCheckInputs.ParseInputFiles(
            tcConfig, 
            Lexhelp.LexResourceManager(), 
            sourceFiles, 
            logger, 
            false)
    
    let inputs = inputs |> List.map fst

    let tcState, _, _, _ = 
        TypeCheck(
            CompilationThreadToken(),
            tcConfig,
            tcImports,
            tcGlobals,
            logger,
            "testblah",
            tcEnv0,
            openDecls0,
            inputs,
            DiagnosticsLogger.QuitProcessExiter)

    let ccuThunk = tcState.Ccu

    tcConfig, tcGlobals, ccuThunk

let private encodeSignatureData (tcConfig, tcGlobals, ccuThunk) : byte array =
    let _, resources = CompilerImports.EncodeSignatureData(
        tcConfig,
        tcGlobals,
        Remap.Empty,
        ccuThunk,
        "",
        false)

    let resource = resources.Head
    let bytes = resource.GetBytes().ReadAllBytes()
    bytes



let private decodeSignatureData (bytes: byte array) =
    let byteReaderA () = ByteMemory.FromArray(bytes).AsReadOnly()

    let data = CompilerImports.GetSignatureData(
        "",
        ILScopeRef.Local,
        None,
        byteReaderA,
        None)

    data


let private fromSignatureData (data: PickledDataWithReferences<PickledCcuInfo>) : string =
    failwith ""



[<Fact>]
let Signatures() =
    let originalCode = """
module BlahModule1 =

    let blahFunction1() = 41

module BlahModule2 =

    let blahFunction2() = 42

System.Console.WriteLine(183)
"""

    let signatureData = toSignatureData originalCode
    let _, _, three = signatureData

    let encodedSignatureData = encodeSignatureData signatureData

    let decodedSignatureData = decodeSignatureData encodedSignatureData

    //let resultingCode = fromSignatureData decodedSignatureData
   
    let contents1 = three.Contents
    let contents2 = decodedSignatureData.RawData.mspec
    
    //let test = contents1 = contents2



    let x = (DebugPrint.entityL contents1).ToString()
    let y = (DebugPrint.entityL contents2).ToString()
    Assert.Equal(x, y)
    //Assert.Equal(originalCode, resultingCode)