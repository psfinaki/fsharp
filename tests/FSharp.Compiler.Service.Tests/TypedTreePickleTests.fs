module FSharp.Compiler.Service.Tests.TypedTreePickleTests

open FSharp.Compiler
open FSharp.Compiler.TypedTree

open Xunit

[<Fact>]
let PickleTest() =
    let result = TypedTreePickle.pickleFile ""

    Assert.True result

[<Fact>]
let UnpickleTest() =
    let result = TypedTreePickle.unpickleFile()

    Assert.True result