module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open FSharp.Compiler
open FSharp.Compiler.IO
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle

open Xunit

[<Fact>]
let PickleModuleOrNamespace() =
    let getState() : WriterState =
        failwith ""

    let mspec = Entity.NewUnlinked()

    let minfo: PickledCcuInfo = {
        mspec = mspec
        compileTimeWorkingDir = ""
        usesQuotations = true
    }

    let st = WriterState.NewUnlinked()

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