module internal FSharp.Compiler.ReuseTcResults.TcResultImport

open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.IO
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.TypedTreePickle
open FSharp.Compiler.ReuseTcResults.TcResultsPickle


let GetTypecheckingDataTcState (file, ilScopeRef, ilModule, byteReaderA, byteReaderB) =
    
    let memA = byteReaderA ()

    let memB =
        match byteReaderB with
        | None -> ByteMemory.Empty.AsReadOnly()
        | Some br -> br ()

    unpickleObjWithDanglingCcus file ilScopeRef ilModule unpickleTcState memA memB

let GetTypecheckingDataTopAttribs (file, ilScopeRef, ilModule, byteReaderA, byteReaderB) =
    
    let memA = byteReaderA ()

    let memB =
        match byteReaderB with
        | None -> ByteMemory.Empty.AsReadOnly()
        | Some br -> br ()

    unpickleObjWithDanglingCcus file ilScopeRef ilModule unpickleTopAttribs memA memB

let GetTypecheckingDataCheckedImplFile (file, ilScopeRef, ilModule, byteReaderA, byteReaderB) =

    let memA = byteReaderA ()

    let memB =
        match byteReaderB with
        | None -> ByteMemory.Empty.AsReadOnly()
        | Some br -> br ()

    unpickleObjWithDanglingCcus file ilScopeRef ilModule unpickleCheckedImplFile memA memB



let WriteTypecheckingDataTcState (tcConfig: TcConfig, tcGlobals, fileName, inMem, ccu, tcState) =

    // need to understand the naming and if we even want two resources here...
    let rName = "FSharpTypecheckingData"
    let rNameB = "FSharpTypecheckingDataB"

    PickleToResource
        inMem
        fileName
        tcGlobals
        tcConfig.compressMetadata
        ccu
        (rName + ccu.AssemblyName)
        (rNameB + ccu.AssemblyName)
        pickleTcState
        tcState


let WriteTypecheckingDataTopAttribs (tcConfig: TcConfig, tcGlobals, fileName, inMem, ccu, topAttribs) =

    // need to understand the naming and if we even want two resources here...
    let rName = "FSharpTypecheckingData"
    let rNameB = "FSharpTypecheckingDataB"

    PickleToResource
        inMem
        fileName
        tcGlobals
        tcConfig.compressMetadata
        ccu
        (rName + ccu.AssemblyName)
        (rNameB + ccu.AssemblyName)
        pickleTopAttribs
        topAttribs


let WriteTypecheckingDataCheckedImplFile (tcConfig: TcConfig, tcGlobals, fileName, inMem, ccu, checkedImplFile) =

    // need to understand the naming and if we even want two resources here...
    let rName = "FSharpTypecheckingData"
    let rNameB = "FSharpTypecheckingDataB"

    PickleToResource
        inMem
        fileName
        tcGlobals
        tcConfig.compressMetadata
        ccu
        (rName + ccu.AssemblyName)
        (rNameB + ccu.AssemblyName)
        pickleCheckedImplFile
        checkedImplFile



let EncodeTypecheckingDataTcState (tcConfig: TcConfig, tcGlobals, generatedCcu, outfile, isIncrementalBuild, tcState) =
    let r1, r2 =
        WriteTypecheckingDataTcState(
            tcConfig, 
            tcGlobals,
            outfile, 
            isIncrementalBuild, 
            generatedCcu,
            tcState)

    let resources =
        [
            r1
            match r2 with
            | None -> ()
            | Some r -> r
        ]

    resources


let EncodeTypecheckingDataTopAttribs (tcConfig: TcConfig, tcGlobals, generatedCcu, outfile, isIncrementalBuild, topAttrs) =
    let r1, r2 =
        WriteTypecheckingDataTopAttribs(
            tcConfig, 
            tcGlobals,
            outfile, 
            isIncrementalBuild, 
            generatedCcu,
            topAttrs)

    let resources =
        [
            r1
            match r2 with
            | None -> ()
            | Some r -> r
        ]

    resources

let EncodeTypecheckingDataCheckedImplFile (tcConfig: TcConfig, tcGlobals, generatedCcu, outfile, isIncrementalBuild, checkedImplFile) =
    let r1, r2 =
        WriteTypecheckingDataCheckedImplFile(
            tcConfig, 
            tcGlobals,
            outfile, 
            isIncrementalBuild, 
            generatedCcu,
            checkedImplFile)

    let resources =
        [
            r1
            match r2 with
            | None -> ()
            | Some r -> r
        ]

    resources

