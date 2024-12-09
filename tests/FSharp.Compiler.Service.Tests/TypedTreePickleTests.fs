module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open FSharp.Compiler
open FSharp.Compiler.IO
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle
open FSharp.Compiler.AbstractIL.IL

open Internal.Utilities.Library

open Xunit

[<Fact>]
let PickleModuleOrNamespace() =
    let mspec : Entity =         
        { entity_typars = LazyWithContext.NotLazy []
          entity_flags = Unchecked.defaultof<_>
          entity_stamp = Unchecked.defaultof<_>
          entity_logical_name = "test" 
          entity_range = Unchecked.defaultof<_> 
          entity_attribs = Unchecked.defaultof<_>
          entity_tycon_repr= Unchecked.defaultof<_>
          entity_tycon_tcaug= Unchecked.defaultof<_>
          entity_modul_type= Unchecked.defaultof<_>
          entity_pubpath = Unchecked.defaultof<_>
          entity_cpath = Unchecked.defaultof<_>
          entity_il_repr_cache = Unchecked.defaultof<_>
          entity_opt_data = Unchecked.defaultof<_>}

    let minfo: PickledCcuInfo = {
        mspec = mspec
        compileTimeWorkingDir = ""
        usesQuotations = true
    }

    let oentities = {
        NodeStamp = fun (tc: TypedTree.Tycon) -> tc.Stamp
        NodeName = Unchecked.defaultof<_>
        GetRange = Unchecked.defaultof<_>
        Deref = id
        Name = Unchecked.defaultof<_>
        Table = Table<_>.Create "test"
    }

    let os = ByteBuffer.Create(42)

    let tryFindSysTypeCcu path typeName publicOnly = None

    let ilg = PrimaryAssemblyILGlobals

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
            Contents = Unchecked.defaultof<_>
            MemberSignatureEquality = Unchecked.defaultof<_>
            TypeForwarders = CcuTypeForwarderTable.Empty
            XmlDocumentationInfo = None
        }

    let fslibCcu = CcuThunk.Create(
        "test",
        ccuData)


    let oglobals = TcGlobals.TcGlobals(
        false,
        ilg,
        fslibCcu,
        "test",
        false,
        false,
        false,
        false,
        tryFindSysTypeCcu,
        false,
        false,
        Unchecked.defaultof<_>,
        Features.LanguageVersion.Default,
        false,
        TcGlobals.CompilationMode.OneOff)

    let st = 
        {
            os = os
            osB = Unchecked.defaultof<_>
            oscope = Unchecked.defaultof<_>
            occus = Unchecked.defaultof<_>
            oentities = oentities
            otypars = Unchecked.defaultof<_>
            ovals = Unchecked.defaultof<_>
            oanoninfos = Unchecked.defaultof<_>
            ostrings = Table<_>.Create ""
            opubpaths = Unchecked.defaultof<_>
            onlerefs = Unchecked.defaultof<_>
            osimpletys = Unchecked.defaultof<_>
            oglobals = oglobals
            isStructThisArgPos = Unchecked.defaultof<_>
            ofile = Unchecked.defaultof<_>
            oInMem = Unchecked.defaultof<_>
        }

    let _result = TypedTreePickle.pickleCcuInfo minfo st

    Assert.True(true)

//[<Fact>]
let PickleTest() =
    let result = TypedTreePickle.pickleFile ""

    Assert.True result

//[<Fact>]
let UnpickleTest() =
    let result = TypedTreePickle.unpickleFile()

    Assert.True result