module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open System.IO

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Text
open FSharp.Compiler.Driver
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle
open FSharp.Compiler.Syntax

open Internal.Utilities.Library

open Xunit

type TypedTreePickleTests() =

    /// random auxiliary stuff

    let builder = TcConfigBuilder.CreateNew(
        SimulatedMSBuildReferenceResolver.getResolver(),
        Directory.GetCurrentDirectory(),
        ReduceMemoryFlag.No,
        "",
        false,
        false,
        CopyFSharpCoreFlag.No,
        (fun _ -> None),
        None,
        Range.range0,
        compressMetadata = false)

    let tcConfig = TcConfig.Create(builder, false)

    let sysRes, otherRes, knownUnresolved =
        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)

    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
    let tcGlobals, frameworkTcImports = 
        TcImports.BuildFrameworkTcImports(
            foundationalTcConfigP,
            sysRes,
            otherRes) 
        |> Async.RunImmediate

    let tcImports =
        TcImports.BuildNonFrameworkTcImports(
            foundationalTcConfigP,
            frameworkTcImports, 
            otherRes, 
            knownUnresolved, 
            new DependencyProvider())
        |> Async.RunImmediate

    let tcEnv0, openDecls0 = ParseAndCheckInputs.GetInitialTcEnv(
        "test",
        Range.rangeStartup,
        tcConfig, 
        tcImports,
        tcGlobals)

    /// helper functions

    let prepareData sourceFiles =
        let inputs =
            ParseAndCheckInputs.ParseInputFiles(
                tcConfig, 
                Lexhelp.LexResourceManager(), 
                sourceFiles, 
                DiagnosticsLogger.AssertFalseDiagnosticsLogger, 
                false) |> List.map fst

        let tcState, topAttribs, implFiles, _ = 
            TypeCheck(
                CompilationThreadToken(),
                tcConfig,
                tcImports,
                tcGlobals,
                DiagnosticsLogger.AssertFalseDiagnosticsLogger,
                "test",
                tcEnv0,
                openDecls0,
                inputs,
                DiagnosticsLogger.QuitProcessExiter,
                "")

        let tcInfo : PickledTcInfo = {
            MainMethodAttrs = topAttribs.mainMethodAttrs
            NetModuleAttrs = topAttribs.netModuleAttrs
            AssemblyAttrs = topAttribs.assemblyAttrs
            DeclaredImpls = implFiles
        }

        tcState.Ccu, tcInfo


    [<Fact>]
    let PickleAndUnpickleTcData() =
        // prepare stuff

        let fileName1 = "test1.fsx"
        let source1 = "42"
        File.WriteAllText(fileName1, source1)

        let ccuThunk, tcInfo = prepareData [ fileName1 ]

        // convert back and forth

        let encodedTcData = EncodeTypecheckingData(tcConfig, tcGlobals, ccuThunk, "", false, tcInfo)
        let memoryReader () = encodedTcData.Head.GetBytes()
        let decodedTcData = GetTypecheckingData("", ILScopeRef.Local, None, memoryReader, None)

        // compare data

        let originalTcInfo = tcInfo
        let restoredTcInfo = decodedTcData.RawData

        // won't work, part of the data has no equality constraint
        // Assert.Equal(originalTcInfo, restoredTcInfo)

        Assert.Equal<Attribs>(originalTcInfo.MainMethodAttrs, restoredTcInfo.MainMethodAttrs)
        Assert.Equal<Attribs>(originalTcInfo.NetModuleAttrs, restoredTcInfo.NetModuleAttrs)
        Assert.Equal<Attribs>(originalTcInfo.AssemblyAttrs, restoredTcInfo.AssemblyAttrs)

        // won't work, part of the data has no equality constraint
        //Assert.Equal<CheckedImplFile list>(originalTcInfo.DeclaredImpls, restoredTcInfo.DeclaredImpls)

        let originalFiles = originalTcInfo.DeclaredImpls
        let restoredFiles = restoredTcInfo.DeclaredImpls
        Assert.Equal(originalFiles.Length, restoredFiles.Length)

        for i = 0 to originalFiles.Length - 1 do
            let (CheckedImplFile (
                    o_qualifiedNameOfFile,
                    o_pragmas,
                    o_signature,
                    o_contents,
                    o_hasExplicitEntryPoint,
                    o_isScript,
                    o_anonRecdTypeInto,
                    o_namedDebugPointsForInlinedCode)) = originalFiles[i]

            let (CheckedImplFile (
                    r_qualifiedNameOfFile,
                    r_pragmas,
                    r_signature,
                    r_contents,
                    r_hasExplicitEntryPoint,
                    r_isScript,
                    r_anonRecdTypeInto,
                    r_namedDebugPointsForInlinedCode)) = restoredFiles[i]

            // doesn't work, no equality for QualifiedNameOfFile
            // Assert.Equal(o_qualifiedNameOfFile, r_qualifiedNameOfFile)

            Assert.Equal<ScopedPragma list>(o_pragmas, r_pragmas)

            // doesn't work, need to figure out namespace equivalence
            // Assert.Equal(o_signature, r_signature)

            Assert.Equal(o_contents, r_contents)

            Assert.Equal(o_hasExplicitEntryPoint, r_hasExplicitEntryPoint)
            Assert.Equal(o_isScript, r_isScript)
            Assert.Equal<StampMap<AnonRecdTypeInfo>>(o_anonRecdTypeInto, r_anonRecdTypeInto)
            Assert.Equal<Map<NamedDebugPointKey, range>>(o_namedDebugPointsForInlinedCode, r_namedDebugPointsForInlinedCode)
