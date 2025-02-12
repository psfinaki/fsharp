module internal FSharp.Compiler.ReuseTcResults

open System.Collections.Generic
open System.IO

open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.GraphChecking
open FSharp.Compiler.IO
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.Syntax
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.TypedTree
open CompilerImports
open FSharp.Compiler.AbstractIL.IL

type TcData =
    {
        CmdLine: string array
        References: string array
    }

type GraphLine = 
    {
        Index: int
        FileName: string
        Stamp: int64
    }

type Graph = GraphLine array

type GraphComparisonResult =
    {
        CanReuseFileNames: string array
        CannotReuseFileNames: string array
    }

[<Sealed>]
type CachingDriver(tcConfig: TcConfig) =

    let outputDir = tcConfig.outputDir |> Option.defaultValue ""
    let tcDataFilePath = Path.Combine(outputDir, FSharpTcDataResourceName)
    let graphFilePath = Path.Combine(outputDir, "graph")
    let tcAuxResourceFilePath = Path.Combine(outputDir, "tcaux")
    let tcStateFilePath = Path.Combine(outputDir, "tcstate")
    let tcResourceFilePath = Path.Combine(outputDir, "tc")
    
    [<Literal>]
    let CmdLineHeader = "CMDLINE"

    [<Literal>]
    let ReferencesHeader = "REFERENCES"

    let writeThisTcData (tcData: TcData) =
        use tcDataFile = FileSystem.OpenFileForWriteShim tcDataFilePath

        let lines = ResizeArray<string>()
        lines.Add $"BEGIN {CmdLineHeader}"
        lines.AddRange tcData.CmdLine
        lines.Add $"BEGIN {ReferencesHeader}"
        lines.AddRange tcData.References

        tcDataFile.WriteAllLines lines

    let readPrevTcData () =
        if FileSystem.FileExistsShim tcDataFilePath then
            use tcDataFile = FileSystem.OpenFileForReadShim tcDataFilePath

            let cmdLine = ResizeArray<string>()
            let refs = ResizeArray<string>()

            let mutable currentHeader = ""

            tcDataFile.ReadLines()
            |> Seq.iter (fun line ->
                match line with
                | line when line.StartsWith "BEGIN" -> currentHeader <- line.Split ' ' |> Array.last
                | line ->
                    match currentHeader with
                    | CmdLineHeader -> cmdLine.Add line
                    | ReferencesHeader -> refs.Add line
                    | _ -> invalidOp "broken tc cache")

            Some
                {
                    CmdLine = cmdLine.ToArray()
                    References = refs.ToArray()
                }

        else
            None

    let writeThisGraph (graph: Graph) =
        use tcDataFile = FileSystem.OpenFileForWriteShim graphFilePath

        graph
        |> Array.map (fun line -> sprintf "%i,%s,%i" line.Index line.FileName line.Stamp)
        |> tcDataFile.WriteAllLines

    let readPrevGraph () : Graph option =
        if FileSystem.FileExistsShim graphFilePath then
            use graphFile = FileSystem.OpenFileForReadShim graphFilePath
            graphFile.ReadAllLines()
            |> Array.map (fun line ->
                let parts = line.Split(',') |> Array.toList
                {
                    Index = int (parts[0])
                    FileName = parts[1]
                    Stamp = int64 (parts[2])
                }
            )
            |> Some
        else
            None

    let formatAssemblyReference (r: AssemblyReference) =
        let fileName = r.Text
        let lastWriteTime = FileSystem.GetLastWriteTimeShim fileName
        sprintf "%s,%i" fileName lastWriteTime.Ticks

    let getThisCompilationCmdLine args = args

    // maybe split into two things?
    let getThisCompilationGraph inputs =
        let sourceFiles =
            inputs
            |> Seq.toArray
            |> Array.mapi (fun idx (input: ParsedInput) ->
                {
                    Idx = idx
                    FileName = input.FileName
                    ParsedInput = input
                })

        let filePairs = FilePairMap sourceFiles
        let graph, _ = DependencyResolution.mkGraph filePairs sourceFiles

        let list = List<GraphLine>()

        for KeyValue(idx, _) in graph do
            let fileName = sourceFiles[idx].FileName
            let lastWriteTime = FileSystem.GetLastWriteTimeShim fileName
            let graphLine = {
                Index = idx
                FileName = fileName
                Stamp = lastWriteTime.Ticks
            }

            list.Add(graphLine)

        //for KeyValue(idx, deps) in graph do
        //    for depIdx in deps do
        //        list.Add $"%i{idx} --> %i{depIdx}"

        list.ToArray()

    let getThisCompilationReferences = Seq.map formatAssemblyReference >> Seq.toArray

    let compareGraphs (thisGraph: Graph) (baseGraph: Graph) =
        let canReuse = ResizeArray<string>()
        let cannotReuse = ResizeArray<string>()

        for thisGraphLine in thisGraph do
            let baseGraphLineOpt = baseGraph |> Seq.tryFind (fun line -> line.FileName = thisGraphLine.FileName)
            match baseGraphLineOpt with
            | Some baseGraphLine when baseGraphLine.Stamp = thisGraphLine.Stamp ->
                canReuse.Add(thisGraphLine.FileName)
            | _ -> 
                cannotReuse.Add(thisGraphLine.FileName)

        {
            CanReuseFileNames = canReuse.ToArray()
            CannotReuseFileNames = cannotReuse.ToArray()
        }


    member _.CanReuseTcResults (inputs: ParsedInput list) =
        let prevTcDataOpt = readPrevTcData ()

        let thisTcData =
            {
                CmdLine = getThisCompilationCmdLine tcConfig.cmdLineArgs
                References = getThisCompilationReferences tcConfig.referencedDLLs
            }

        match prevTcDataOpt with
        | Some prevTcData ->
            use _ = Activity.start Activity.Events.reuseTcResultsCachePresent []

            if prevTcData = thisTcData then
                use _ = Activity.start Activity.Events.reuseTcResultsCacheHit []

                let prevGraphOpt = readPrevGraph ()
                let thisGraph = getThisCompilationGraph inputs
                match prevGraphOpt with
                | Some prevGraph ->
                    let result = compareGraphs thisGraph prevGraph
                    let canReuse = inputs |> List.where (fun input -> result.CanReuseFileNames |> Seq.contains input.FileName)
                    let cannotReuse = inputs |> List.where (fun input -> result.CannotReuseFileNames |> Seq.contains input.FileName)
                    Some (canReuse, cannotReuse)
                | None -> None
            else
                use _ = Activity.start Activity.Events.reuseTcResultsCacheMissed []
                None

        | None ->
            use _ = Activity.start Activity.Events.reuseTcResultsCacheAbsent []
            None

    member private _.ReuseTcState() : TcState =
        let bytes = File.ReadAllBytes(tcStateFilePath)
        let memory = ByteMemory.FromArray(bytes)
        let byteReaderA () = ReadOnlyByteMemory(memory)

        let data =
            GetTypecheckingDataTcState(
                "", // assembly.FileName,
                ILScopeRef.Local, // assembly.ILScopeRef,
                None, //assembly.RawMetadata.TryGetILModuleDef(),
                byteReaderA,
                None
            )

        let rawData = data.RawData

        {
            tcsCcu = rawData.TcsCcu
            tcsCreatesGeneratedProvidedTypes = rawData.TcsCreatesGeneratedProvidedTypes
            tcsTcSigEnv = Unchecked.defaultof<_>
            tcsTcImplEnv = Unchecked.defaultof<_>
            // not sure about default
            tcsRootSigs = RootSigs.FromList(Comparer.Default, rawData.TcsRootSigs)
            tcsRootImpls = Unchecked.defaultof<_>
            tcsCcuSig = rawData.TcsCcuSig
            tcsImplicitOpenDeclarations = rawData.TcsImplicitOpenDeclarations
        }

    member private _.ReuseTopAttrs() =
        let bytes = File.ReadAllBytes(tcAuxResourceFilePath)
        let memory = ByteMemory.FromArray(bytes)
        let byteReaderA () = ReadOnlyByteMemory(memory)

        let data =
            GetTypecheckingDataTcInfo(
                "", // assembly.FileName,
                ILScopeRef.Local, // assembly.ILScopeRef,
                None, //assembly.RawMetadata.TryGetILModuleDef(),
                byteReaderA,
                None
            )

        let rawData = data.RawData

        {
            mainMethodAttrs = rawData.MainMethodAttrs
            netModuleAttrs = rawData.NetModuleAttrs
            assemblyAttrs = rawData.AssemblyAttrs
        }

    member private _.ReuseDeclaredImpl (implFile: ParsedInput) =
        let fileName = Path.GetFileNameWithoutExtension(implFile.FileName)
        let bytes = File.ReadAllBytes($"{tcResourceFilePath}{fileName}")
        let memory = ByteMemory.FromArray(bytes)
        let byteReaderA () = ReadOnlyByteMemory(memory)

        let data =
            GetTypecheckingDataCheckedImplFile(
                "", // assembly.FileName,
                ILScopeRef.Local, // assembly.ILScopeRef,
                None, //assembly.RawMetadata.TryGetILModuleDef(),
                byteReaderA,
                None
            )

        data.RawData

    member this.ReuseTcResults (inputs: ParsedInput list) =
        let tcState = this.ReuseTcState()
        let topAttrs = this.ReuseTopAttrs()
        let declaredImpls = inputs |> List.map this.ReuseDeclaredImpl

        tcState,
        topAttrs,
        declaredImpls
    
    member private _.CacheTcState(tcState: TcState, tcGlobals, outfile) =
        let x = tcState.tcsRootSigs.ToList()
        let pickledTcState = {
            TcsCcu = tcState.tcsCcu
            TcsCreatesGeneratedProvidedTypes = tcState.tcsCreatesGeneratedProvidedTypes
            TcsRootSigs = tcState.tcsRootSigs.ToList()
            TcsCcuSig = tcState.tcsCcuSig
            TcsImplicitOpenDeclarations = tcState.tcsImplicitOpenDeclarations
        }

        let encodedData =
            EncodeTypecheckingDataTcState(tcConfig, tcGlobals, tcState.Ccu, outfile, false, pickledTcState)

        let resource = encodedData[0].GetBytes().ToArray()
        File.WriteAllBytes(tcStateFilePath, resource)

    member private _.CacheTopAttrs(tcState: TcState, topAttrs: TopAttribs, tcGlobals, outfile) =
        let tcInfo =
            {
                MainMethodAttrs = topAttrs.mainMethodAttrs
                NetModuleAttrs = topAttrs.netModuleAttrs
                AssemblyAttrs = topAttrs.assemblyAttrs
            }

        let encodedData =
            EncodeTypecheckingDataTcInfo(tcConfig, tcGlobals, tcState.Ccu, outfile, false, tcInfo)

        let resource = encodedData[0].GetBytes().ToArray()
        File.WriteAllBytes(tcAuxResourceFilePath, resource)

    member private _.CacheDeclaredImpl(tcState: TcState, impl: CheckedImplFile, tcGlobals, outfile) =
        let encodedData =
            EncodeTypecheckingDataCheckedImplFile(tcConfig, tcGlobals, tcState.Ccu, outfile, false, impl)

        let fileName = Path.GetFileNameWithoutExtension(impl.QualifiedNameOfFile.Range.FileName)
        let resource = encodedData[0].GetBytes().ToArray()
        File.WriteAllBytes($"{tcResourceFilePath}{fileName}", resource)

    member this.CacheTcResults(tcState: TcState, topAttrs: TopAttribs, declaredImpls: CheckedImplFile list, tcEnvAtEndOfLastFile, inputs, tcGlobals, outfile) =
        let thisTcData =
            {
                CmdLine = getThisCompilationCmdLine tcConfig.cmdLineArgs
                References = getThisCompilationReferences tcConfig.referencedDLLs
            }

        writeThisTcData thisTcData

        let thisGraph = getThisCompilationGraph inputs
        writeThisGraph thisGraph


        this.CacheTcState(tcState, tcGlobals, outfile)
        this.CacheTopAttrs(tcState, topAttrs, tcGlobals, outfile)
        declaredImpls |> List.iter (fun impl -> this.CacheDeclaredImpl(tcState, impl, tcGlobals, outfile))
