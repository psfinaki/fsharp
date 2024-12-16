module FSharp.Compiler.Service.Tests.Dump3

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
open FSharp.Compiler.TypedTreeOps

open Internal.Utilities.Collections
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

open Xunit

let private magicFunction (contents: Entity) =
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
let GetSignatureData1() =
    let bytes = [|
        (byte)0;  // ccus
        (byte)0;  // n tycons
        (byte)0;  // n typars
        (byte)0;  // n vals
        (byte)1;  // string table
        (byte)0;
        (byte)0;  // pubpath table
        (byte)0;  // n lerefe table
        (byte)0;  // simple type tables
        (byte)60; // phase1bytes
        (byte)0;
        (byte)0;  // tyar_spec
        (byte)0;  // logical name
        (byte)0;  // some opt data
        (byte)0;  // range
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;  // more opt data
        (byte)0;
        (byte)0;
        (byte)0;  // attributes
        (byte)0;  // tycon
        (byte)0;  // ty
        (byte)0;  // tcaug
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;  // unused
        (byte)0;  // kind
        (byte)0;  // entity flags
        (byte)0;
        (byte)0;  // more opt data
        (byte)0;  // osgn
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;
        (byte)0;  // modul type
        (byte)0;
        (byte)0;
    |]

    let byteReaderA () = 
        ByteMemory.FromArray(bytes).AsReadOnly()

    let byteReaderB = 
        Some (fun () -> ByteMemory.Empty.AsReadOnly())

    let t = 
        GetSignatureData(
            "",
            Unchecked.defaultof<_>,
            Unchecked.defaultof<_>,
            byteReaderA,
            byteReaderB)

    Assert.True(true)
