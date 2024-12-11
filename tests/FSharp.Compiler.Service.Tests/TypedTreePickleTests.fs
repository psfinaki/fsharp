module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open System.IO

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.IO
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeOps
open FSharp.Compiler.TypedTreePickle

open Internal.Utilities
open Internal.Utilities.Collections
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

open Xunit

let private magicFunction (modul_type: ModuleOrNamespaceType) =
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

    let contents = Entity.NewUnlinked()
    let contents = {
        contents with 
            entity_typars = LazyWithContext.NotLazy Typars.Empty
            entity_attribs = Attribs.Empty
            entity_tycon_repr = TyconRepresentation.TNoRepr
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
        "testd",
        ccuData)

    let sysRes, otherRes, _ =
        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)
    
    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
    let tcGlobals, _ = 
        TcImports.BuildFrameworkTcImports(
            foundationalTcConfigP,
            sysRes,
            otherRes) 
        |> Async.RunImmediate

    let result = CompilerImports.EncodeSignatureData(
        tcConfig,
        tcGlobals,
        Remap.Empty,
        ccuThunk,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>)

    let (_, resources) = result
    let bytes = resources.Head.GetBytes().ReadAllBytes()
    
    System.Text.Encoding.Default.GetString(bytes)


[<Fact>]
let EncodeSignatureData1() =
    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.Namespace false,
        QueueList.Empty,
        QueueList.Empty)

    let result = magicFunction modul_type
    let expected = "c`d``f)I-.a/����/�c```p\u0006\u0011� \u0002\r0�\bf�$\u000e\u0005�\u0010U"
    
    Assert.Contains(expected, result)

[<Fact>]
let EncodeSignatureData2() =
    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.ModuleOrType,
        QueueList.Empty,
        QueueList.Empty)

    let result = magicFunction modul_type
    let expected = "c`d``f)I-.a/����/�c```p\u0006\u0011� \u0002\r0�\bf�$6\u0005 � �"
    
    Assert.Contains(expected, result)

[<Fact>]
let EncodeSignatureData3() =
    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.FSharpModuleWithSuffix,
        QueueList.Empty,
        QueueList.Empty)

    let result = magicFunction modul_type
    let expected = "c`d``f)I-.a/����/�c```p\u0006\u0011� \u0002\r0�\bf�$6\u0005`i�*"
    
    Assert.Contains(expected, result)

[<Fact>]
let EncodeSignatureData4() =
    let v =  
        { Val.NewUnlinked() with
            val_logical_name = "test"
            val_type = TType.TType_measure Measure.One
        }

    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.FSharpModuleWithSuffix,
        QueueList.ofList [v],
        QueueList.Empty)

    let result = magicFunction modul_type
    let expected = "c`d`df)I-.a/����/�c```\b\u0006\u0011� \u0002\r0�\ba�$�\u00020\u001fL�� T30Ch"
    
    Assert.Contains(expected, result)
