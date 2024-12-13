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
open FSharp.Compiler.Driver
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeOps

open Internal.Utilities.Collections
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

open Xunit

[<Fact>]
let EncodeTypecheckingData() =
    let resolver = SimulatedMSBuildReferenceResolver.getResolver()
    let currentDir = Directory.GetCurrentDirectory()

    let builder = TcConfigBuilder.CreateNew(
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
        compilationMode = CompilationMode.OneOff
        )

    let tcConfig = TcConfig.Create(builder, false)

    ///

    let sysRes, otherRes, knownUnresolved =
        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)
    
    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
    let tcGlobals, frameworkTcImports = 
        TcImports.BuildFrameworkTcImports(
            foundationalTcConfigP,
            sysRes,
            otherRes) 
        |> Async.RunImmediate

    ///

    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.Namespace false,
        QueueList.Empty,
        QueueList.Empty)

    let contents = {
        Entity.NewUnlinked() with 
            entity_typars = LazyWithContext.NotLazy Typars.Empty
            entity_attribs = Attribs.Empty
            entity_tycon_repr = TNoRepr
            entity_tycon_tcaug = TyconAugmentation.Create()
            entity_modul_type = MaybeLazy.Strict(modul_type)
            entity_logical_name = "test"
    }

    let ccuData : CcuData = 
        {
            IsFSharp = true
            UsesFSharp20PlusQuotations = false
            InvalidateEvent = (Event<_>()).Publish
            IsProviderGenerated = false
            ImportProvidedType = Unchecked.defaultof<_>
            TryGetILModuleDef = (fun () -> None)
            FileName = None
            Stamp = Unchecked.defaultof<_>
            QualifiedName = None
            SourceCodeDirectory = Unchecked.defaultof<_>
            ILScopeRef = ILScopeRef.Local
            Contents = contents
            MemberSignatureEquality = Unchecked.defaultof<_>
            TypeForwarders = CcuTypeForwarderTable.Empty
            XmlDocumentationInfo = None
        }

    let ccuThunk = CcuThunk.Create(
        "",
        ccuData)

    ///

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
            
    let logger = DiagnosticsLogger.AssertFalseDiagnosticsLogger
    
    File.WriteAllText("testblah.fs", "printfn \"blah\"")
    let sourceFiles = ["testblah.fs"]

    let inputs =
        ParseAndCheckInputs.ParseInputFiles(
            tcConfig, 
            Lexhelp.LexResourceManager(), 
            sourceFiles, 
            logger, 
            false)
    
    let inputs = inputs |> List.map fst

    let _, _, implFiles, _ = 
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

    let implFile = implFiles[0]

    ///

    let resources = CompilerImports.EncodeTypecheckingData(
        tcConfig,
        tcGlobals,
        ccuThunk,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>,
        implFile)

    let bytes = resources.Head.GetBytes().ReadAllBytes()
    let zero = (char)0
    let expected = $"c`�\u0002{zero}"
    let result = Encoding.Default.GetString(bytes)

    
    Assert.Equal(expected, result)