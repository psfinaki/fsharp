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
        compressMetadata = false
        )

    let tcConfig = TcConfig.Create(builder, false)
    
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


    failwith ""

let private encodeSignatureData (tcConfig, tcGlobals, ccuThunk) : (unit -> ReadOnlyByteMemory) =
    let _r = CompilerImports.EncodeSignatureData(
        tcConfig,
        tcGlobals,
        Remap.Empty,
        ccuThunk,
        "",
        false)

    failwith ""

let private decodeSignatureData byteReaderA = 
    CompilerImports.GetSignatureData(
        "",
        Unchecked.defaultof<_>,
        None,
        byteReaderA,
        None)

let private fromSignatureData (data: PickledDataWithReferences<PickledCcuInfo>) : string =
    failwith ""



[<Fact>]
let Signatures() =
    let originalCode = "printfn \"hello world\""

    let signatureData = toSignatureData originalCode

    let encodedSignatureData = encodeSignatureData signatureData

    let decodedSignatureData = decodeSignatureData encodedSignatureData

    let resultingCode = fromSignatureData decodedSignatureData

    Assert.Equal(originalCode, resultingCode)