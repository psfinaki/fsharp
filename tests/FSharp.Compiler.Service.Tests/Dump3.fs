﻿module FSharp.Compiler.Service.Tests.Dump3

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

[<Fact>]
let GetSignatureData1() =
    let bytes = [|
        (byte)0;  // ccus
        (byte)1;  // n tycons
        (byte)0;  // n typars
        (byte)0;  // n vals
        (byte)1;  // string table
        (byte)0;
        (byte)0;  // pubpath table
        (byte)0;  // n lerefe table
        (byte)0;  // simple type tables
        (byte)67; // phase1bytes
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
        (byte)3;  // exception representation
        (byte)0;  // extra space
        (byte)0;  // working dir
        (byte)0;  // use quotations
        (byte)0;  // extra spaces
        (byte)0;
        (byte)0;
    |]

    let byteReaderA () = 
        ByteMemory.FromArray(bytes).AsReadOnly()

    let byteReaderB = None

    let result = 
        GetSignatureData(
            "",
            Unchecked.defaultof<_>,
            Unchecked.defaultof<_>,
            byteReaderA,
            byteReaderB)
    
    Assert.True(true)