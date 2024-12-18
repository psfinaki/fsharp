module FSharp.Compiler.Service.Tests.TypedTreePickleTests

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

let private toTcData (code: string) =
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

    let tcState, _, implFiles, _ = 
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

    tcConfig, tcGlobals, ccuThunk, implFiles[0]

let private encodeTcData (tcConfig, tcGlobals, ccuThunk, implFile) : byte array =
    let resources = CompilerImports.EncodeTypecheckingData(
        tcConfig,
        tcGlobals,
        ccuThunk,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>,
        implFile)

    let resource = resources.Head
    let bytes = resource.GetBytes().ReadAllBytes()
    bytes



let private decodeTcData (bytes: byte array) =
    let byteReader () = ByteMemory.FromArray(bytes).AsReadOnly()

    let data = CompilerImports.GetTypecheckingData(
        "",
        ILScopeRef.Local,
        None,
        byteReader)

    data

[<Fact>]
let Test() =
    let originalCode = """
module BlahModule1 =

    let blahFunction1() = 41

module BlahModule2 =

    let blahFunction2() = 42

System.Console.WriteLine(183)
"""

    let tcData = toTcData originalCode
    let _, _, _, file = tcData

    let encodedTcData = encodeTcData tcData

    let decodedTcData = decodeTcData encodedTcData

    let contents1 = file
    let contents2 = decodedTcData.RawData
   
    //Assert.Equal(contents1.Signature, contents2.Signature) kind doesn't work
    // Assert.Equal(contents1.Contents, contents2.Contents) no equality
    Assert.Equal(contents1.QualifiedNameOfFile, contents2.QualifiedNameOfFile)
    Assert.Equal<ScopedPragma list>(contents1.Pragmas, contents2.Pragmas)
    Assert.Equal(contents1.IsScript, contents2.IsScript)
    Assert.Equal(contents1.HasExplicitEntryPoint, contents2.HasExplicitEntryPoint)

