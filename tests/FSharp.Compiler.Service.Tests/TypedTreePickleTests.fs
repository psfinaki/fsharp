module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open System.IO
open System.Text

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.Driver
open FSharp.Compiler.TcGlobals
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

    let sysRes, otherRes, _ =
        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)
    
    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
    let tcGlobals, _ = 
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


    //let implFile = 
    //    FSharp.Compiler.Driver.TypeCheck

    ///

    let resources = CompilerImports.EncodeTypecheckingData(
        tcConfig,
        tcGlobals,
        ccuThunk,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>)

    let bytes = resources.Head.GetBytes().ReadAllBytes()
    let _result = Encoding.Default.GetString(bytes)

    
    Assert.True(true)