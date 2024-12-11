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

//let _oglobals = TcGlobals.TcGlobals(
//    false,
//    ilg,
//    fslibCcu,
//    "test",
//    false,
//    false,
//    false,
//    false,
//    tryFindSysTypeCcu,
//    false,
//    false,
//    Unchecked.defaultof<_>,
//    Features.LanguageVersion.Default,
//    false,
//    TcGlobals.CompilationMode.OneOff)


//[<Fact>]
let Test3() =
    let legacyReferenceResolver = SimulatedMSBuildReferenceResolver.getResolver ()
    let defaultFSharpBinariesDir = 
        FSharpEnvironment.BinFolderOfDefaultFSharpCompiler(None).Value
    let directoryBuildingFrom = Directory.GetCurrentDirectory()
    let tryGetMetadataSnapshot = (fun _ -> None)

    let tcConfigB = 
        TcConfigBuilder.CreateNew(
            legacyReferenceResolver,
            defaultFSharpBinariesDir,
            ReduceMemoryFlag.Yes,
            directoryBuildingFrom,
            false,
            false,
            CopyFSharpCoreFlag.No,
            tryGetMetadataSnapshot,
            None,
            range0,
            compilationMode = CompilationMode.OneOff
        )



    let tcConfig = TcConfig.Create(tcConfigB, false)
    
    let sysRes, otherRes, _ =
        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)
    
    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
    let tcGlobals, _ = 
        TcImports.BuildFrameworkTcImports(
            foundationalTcConfigP,
            sysRes,
            otherRes) 
        |> Async.RunImmediate

    let ccuThunk: CcuThunk = Unchecked.defaultof<_>

    let exportRemapping = MakeExportRemapping ccuThunk ccuThunk.Contents
    
    let outfile: string = Unchecked.defaultof<_>
    
    let isIncrementalBuild: bool = false

    let result = CompilerImports.EncodeSignatureData(
        tcConfig,
        tcGlobals,
        exportRemapping,
        ccuThunk,
        outfile,
        isIncrementalBuild)

    Assert.True(true)




//[<Fact>]
//let pickleCcuInfo() =
//    let modul_type = ModuleOrNamespaceType(
//        ModuleOrNamespaceKind.Namespace false,
//        QueueList.Empty,
//        QueueList.Empty)

//    let mspec : Entity =         
//        { entity_typars = LazyWithContext.NotLazy []
//          entity_flags = Unchecked.defaultof<_>
//          entity_stamp = Unchecked.defaultof<_>
//          entity_logical_name = "test" 
//          entity_range = Unchecked.defaultof<_> 
//          entity_attribs = Attribs.Empty
//          entity_tycon_repr = TyconRepresentation.TNoRepr
//          entity_tycon_tcaug = TyconAugmentation.Create()
//          entity_modul_type = MaybeLazy.Strict(modul_type)
//          entity_pubpath = Unchecked.defaultof<_>
//          entity_cpath = Unchecked.defaultof<_>
//          entity_il_repr_cache = Unchecked.defaultof<_>
//          entity_opt_data = Unchecked.defaultof<_>}

//    let minfo: PickledCcuInfo = {
//        mspec = mspec
//        compileTimeWorkingDir = ""
//        usesQuotations = true
//    }

//    ///

//    let resolver = SimulatedMSBuildReferenceResolver.getResolver()
//    let currentDir = Directory.GetCurrentDirectory()

//    let builder = TcConfigBuilder.CreateNew(
//        resolver,
//        currentDir,
//        ReduceMemoryFlag.No,
//        "",
//        false,
//        false,
//        CopyFSharpCoreFlag.No,
//        (fun _ -> None),
//        None,
//        Range.range0,
//        compilationMode = CompilationMode.OneOff
//        )

//    let tcConfig = TcConfig.Create(builder, false)

//    let sysRes, otherRes, _ =
//        TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)
    
//    let foundationalTcConfigP = TcConfigProvider.Constant tcConfig
//    let tcGlobals, _ = 
//        TcImports.BuildFrameworkTcImports(
//            foundationalTcConfigP,
//            sysRes,
//            otherRes) 
//        |> Async.RunImmediate

//    let os = ByteBuffer.Create(42)

//    let oentities = {
//        NodeStamp = fun (tc: TypedTree.Tycon) -> tc.Stamp
//        NodeName = Unchecked.defaultof<_>
//        GetRange = Unchecked.defaultof<_>
//        Deref = id
//        Name = Unchecked.defaultof<_>
//        Table = Table<_>.Create "test"
//    }

//    let table1 = NodeOutTable<_, _>.Create((fun (tp: Typar) -> tp.Stamp), (fun tp -> tp.DisplayName), (fun tp -> tp.Range), id , "otypars")
//    let table2 = NodeOutTable<_, _>.Create((fun (v: Val) -> v.Stamp), (fun v -> v.LogicalName), (fun v -> v.Range), id , "ovals")

//    let st = 
//        {
//            os = os
//            osB = Unchecked.defaultof<_>
//            oscope = Unchecked.defaultof<_>
//            occus = Unchecked.defaultof<_>
//            oentities = oentities
//            otypars = table1
//            ovals = table2
//            oanoninfos = Unchecked.defaultof<_>
//            ostrings = Table<_>.Create ""
//            opubpaths = Unchecked.defaultof<_>
//            onlerefs = Unchecked.defaultof<_>
//            osimpletys = Unchecked.defaultof<_>
//            oglobals = tcGlobals
//            isStructThisArgPos = Unchecked.defaultof<_>
//            ofile = Unchecked.defaultof<_>
//            oInMem = Unchecked.defaultof<_>
//        }

//    TypedTreePickle.pickleCcuInfo minfo st

//    Assert.True(true)


[<Fact>]
let EncodeSignatureData1() =
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

    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.Namespace false,
        QueueList.Empty,
        QueueList.Empty)

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
        Unchecked.defaultof<_>,
        ccuThunk,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>)

    let (_, resources) = result
    let bytes = resources.Head.GetBytes().ReadAllBytes()
    
    let actual = System.Text.Encoding.Default.GetString(bytes)
    let expected = "c`d``f)I-.a/����/�c```p\u0006\u0011� \u0002\r0�\bf�$\u000e\u0005�\u0010U"
    
    Assert.Contains(expected, actual)


[<Fact>]
let EncodeSignatureData2() =
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

    let modul_type = ModuleOrNamespaceType(
        ModuleOrNamespaceKind.ModuleOrType,
        QueueList.Empty,
        QueueList.Empty)

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
        Unchecked.defaultof<_>,
        ccuThunk,
        Unchecked.defaultof<_>,
        Unchecked.defaultof<_>)

    let (_, resources) = result
    let bytes = resources.Head.GetBytes().ReadAllBytes()
    
    let actual = System.Text.Encoding.Default.GetString(bytes)
    let expected = "c`d``f)I-.a/����/�c```p\u0006\u0011� \u0002\r0�\bf�$6\u0005 � �"
    
    Assert.Contains(expected, actual)

