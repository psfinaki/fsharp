module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open FSharp.Compiler
open FSharp.Compiler.IO
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle

open Xunit

[<Fact>]
let PickleModuleOrNamespace() =
    let mspec = Entity.NewUnlinked()

    let minfo: PickledCcuInfo = {
        mspec = mspec
        compileTimeWorkingDir = ""
        usesQuotations = true
    }

    let oentities = {
        NodeStamp = Unchecked.defaultof<_>
        NodeName = Unchecked.defaultof<_>
        GetRange = Unchecked.defaultof<_>
        Deref = Unchecked.defaultof<_>
        Name = Unchecked.defaultof<_>
        Table = Unchecked.defaultof<_>
    }

    let st = 
        {
            os = Unchecked.defaultof<_>
            osB = Unchecked.defaultof<_>
            oscope = Unchecked.defaultof<_>
            occus = Unchecked.defaultof<_>
            oentities = oentities
            otypars = Unchecked.defaultof<_>
            ovals = Unchecked.defaultof<_>
            oanoninfos = Unchecked.defaultof<_>
            ostrings = Unchecked.defaultof<_>
            opubpaths = Unchecked.defaultof<_>
            onlerefs = Unchecked.defaultof<_>
            osimpletys = Unchecked.defaultof<_>
            oglobals = Unchecked.defaultof<_>
            isStructThisArgPos = Unchecked.defaultof<_>
            ofile = Unchecked.defaultof<_>
            oInMem = Unchecked.defaultof<_>
        }

    let _result = TypedTreePickle.pickleCcuInfo minfo st

    Assert.True(true)

[<Fact>]
let PickleTest() =
    let result = TypedTreePickle.pickleFile ""

    Assert.True result

[<Fact>]
let UnpickleTest() =
    let result = TypedTreePickle.unpickleFile()

    Assert.True result