// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// Open up the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.

namespace FSharp.Compiler.CodeAnalysis

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open FSharp.Compiler.IO
open FSharp.Compiler.NicePrint
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras
open Internal.Utilities.TypeHashing
open FSharp.Core.Printf
open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AccessibilityLogic
open FSharp.Compiler.CheckExpressionsOps
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerDiagnostics
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.Features
open FSharp.Compiler.Infos
open FSharp.Compiler.InfoReader
open FSharp.Compiler.Lexhelp
open FSharp.Compiler.NameResolution
open FSharp.Compiler.OptimizeInputs
open FSharp.Compiler.Parser
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.ScriptClosure
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.SymbolHelpers
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Layout
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Text.Range
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeBasics
open FSharp.Compiler.TypedTreeOps

open Internal.Utilities
open Internal.Utilities.Collections
open FSharp.Compiler.AbstractIL.ILBinaryReader
open System.Threading.Tasks
open System.Runtime.CompilerServices
open Internal.Utilities.Hashing

type FSharpUnresolvedReferencesSet = FSharpUnresolvedReferencesSet of UnresolvedAssemblyReference list

[<RequireQualifiedAccess>]
type DocumentSource =
    | FileSystem
    | Custom of (string -> Async<ISourceText option>)

[<Sealed>]
type DelayedILModuleReader =
    val private name: string
    val private gate: obj
    val mutable private getStream: (CancellationToken -> Stream option)
    val mutable private result: ILModuleReader

    new(name, getStream) =
        {
            name = name
            gate = obj ()
            getStream = getStream
            result = Unchecked.defaultof<_>
        }

    member this.OutputFile = this.name

    member this.TryGetILModuleReader() =
        // fast path
        match box this.result with
        | null ->
            cancellable {
                let! ct = Cancellable.token ()

                return
                    lock this.gate (fun () ->
                        // see if we have a result or not after the lock so we do not evaluate the stream more than once
                        match box this.result with
                        | null ->
                            try
                                let streamOpt = this.getStream ct

                                match streamOpt with
                                | Some stream ->
                                    let ilReaderOptions: ILReaderOptions =
                                        {
                                            pdbDirPath = None
                                            reduceMemoryUsage = ReduceMemoryFlag.Yes
                                            metadataOnly = MetadataOnlyFlag.Yes
                                            tryGetMetadataSnapshot = fun _ -> None
                                        }

                                    let ilReader = OpenILModuleReaderFromStream this.name stream ilReaderOptions
                                    this.result <- ilReader
                                    this.getStream <- Unchecked.defaultof<_> // clear out the function so we do not hold onto anything
                                    Some ilReader
                                | _ -> None
                            with ex ->
                                Trace.TraceInformation("FCS: Unable to get an ILModuleReader: {0}", ex)
                                None
                        | _ -> Some this.result)
            }
        | _ -> cancellable.Return(Some this.result)

[<RequireQualifiedAccess; NoComparison; CustomEquality>]
type FSharpReferencedProject =
    | FSharpReference of projectOutputFile: string * options: FSharpProjectOptions
    | PEReference of getStamp: (unit -> DateTime) * delayedReader: DelayedILModuleReader
    | ILModuleReference of projectOutputFile: string * getStamp: (unit -> DateTime) * getReader: (unit -> ILModuleReader)

    member this.OutputFile =
        match this with
        | FSharpReference(projectOutputFile = projectOutputFile)
        | ILModuleReference(projectOutputFile = projectOutputFile) -> projectOutputFile
        | PEReference(delayedReader = reader) -> reader.OutputFile

    override this.Equals(o) =
        match o with
        | :? FSharpReferencedProject as o ->
            match this, o with
            | FSharpReference(projectOutputFile1, options1), FSharpReference(projectOutputFile2, options2) ->
                projectOutputFile1 = projectOutputFile2
                && FSharpProjectOptions.AreSameForChecking(options1, options2)
            | PEReference(getStamp1, reader1), PEReference(getStamp2, reader2) ->
                reader1.OutputFile = reader2.OutputFile && (getStamp1 ()) = (getStamp2 ())
            | ILModuleReference(projectOutputFile1, getStamp1, _), ILModuleReference(projectOutputFile2, getStamp2, _) ->
                projectOutputFile1 = projectOutputFile2 && (getStamp1 ()) = (getStamp2 ())
            | _ -> false
        | _ -> false

    override this.GetHashCode() = this.OutputFile.GetHashCode()

// NOTE: may be better just to move to optional arguments here
and FSharpProjectOptions =
    {
        ProjectFileName: string
        ProjectId: string option
        SourceFiles: string[]
        OtherOptions: string[]
        ReferencedProjects: FSharpReferencedProject[]
        IsIncompleteTypeCheckEnvironment: bool
        UseScriptResolutionRules: bool
        LoadTime: DateTime
        UnresolvedReferences: FSharpUnresolvedReferencesSet option
        OriginalLoadReferences: (range * string * string) list
        Stamp: int64 option
    }

    static member UseSameProject(options1, options2) =
        match options1.ProjectId, options2.ProjectId with
        | Some(projectId1), Some(projectId2) when
            not (String.IsNullOrWhiteSpace(projectId1))
            && not (String.IsNullOrWhiteSpace(projectId2))
            ->
            projectId1 = projectId2
        | Some _, Some _
        | None, None -> options1.ProjectFileName = options2.ProjectFileName
        | _ -> false

    static member AreSameForChecking(options1, options2) =
        match options1.Stamp, options2.Stamp with
        | Some x, Some y -> (x = y)
        | _ ->
            FSharpProjectOptions.UseSameProject(options1, options2)
            && options1.SourceFiles = options2.SourceFiles
            && options1.OtherOptions = options2.OtherOptions
            && options1.UnresolvedReferences = options2.UnresolvedReferences
            && options1.OriginalLoadReferences = options2.OriginalLoadReferences
            && options1.ReferencedProjects.Length = options2.ReferencedProjects.Length
            && options1.ReferencedProjects = options2.ReferencedProjects
            && options1.LoadTime = options2.LoadTime

    member po.ProjectDirectory = Path.GetDirectoryName(po.ProjectFileName)

    override this.ToString() =
        "FSharpProjectOptions(" + this.ProjectFileName + ")"

[<AutoOpen>]
module internal FSharpCheckerResultsSettings =

    let getToolTipTextSize = GetEnvInteger "FCS_GetToolTipTextCacheSize" 5

    let maxTypeCheckErrorsOutOfProjectContext =
        GetEnvInteger "FCS_MaxErrorsOutOfProjectContext" 3

    // Look for DLLs in the location of the service DLL first.
    let defaultFSharpBinariesDir =
        FSharpEnvironment
            .BinFolderOfDefaultFSharpCompiler(
                Path.GetDirectoryName(typeof<IncrementalBuilder>.Assembly.Location)
                |> Option.ofObj
            )
            .Value

[<Sealed>]
type FSharpSymbolUse(denv: DisplayEnv, symbol: FSharpSymbol, inst: TyparInstantiation, itemOcc, range: range) =

    member _.Symbol = symbol

    member _.GenericArguments =
        let cenv = symbol.SymbolEnv

        inst
        |> List.map (fun (v, ty) -> FSharpGenericParameter(cenv, v), FSharpType(cenv, ty))

    member _.DisplayContext = FSharpDisplayContext(fun _ -> denv)

    member x.IsDefinition = x.IsFromDefinition

    member _.IsFromDefinition = itemOcc = ItemOccurrence.Binding

    member _.IsFromPattern = itemOcc = ItemOccurrence.Pattern

    member _.IsFromType = itemOcc = ItemOccurrence.UseInType

    member _.IsFromAttribute = itemOcc = ItemOccurrence.UseInAttribute

    member _.IsFromDispatchSlotImplementation = itemOcc = ItemOccurrence.Implemented

    member _.IsFromUse = itemOcc = ItemOccurrence.Use

    member _.IsFromComputationExpression =
        match symbol.Item, itemOcc with
        // 'seq' in 'seq { ... }' gets colored as keywords
        | Item.Value vref, ItemOccurrence.Use when valRefEq denv.g denv.g.seq_vref vref -> true
        // custom builders, custom operations get colored as keywords
        | (Item.CustomBuilder _ | Item.CustomOperation _), ItemOccurrence.Use -> true
        | _ -> false

    member _.IsFromOpenStatement = itemOcc = ItemOccurrence.Open

    member _.FileName = range.FileName

    member _.Range = range

/// This type is used to describe what was found during the name resolution.
/// (Depending on the kind of the items, we may stop processing or continue to find better items)
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type NameResResult =
    | Members of (ItemWithInst list * DisplayEnv * range)
    | Cancel of DisplayEnv * range
    | Empty

[<RequireQualifiedAccess>]
type ResolveOverloads =
    | Yes
    | No

[<RequireQualifiedAccess>]
type ExprTypingsResult =
    | NoneBecauseTypecheckIsStaleAndTextChanged
    | NoneBecauseThereWereTypeErrors
    | None
    | Some of (ItemWithInst list * DisplayEnv * range) * TType

type Names = string list

/// A TypeCheckInfo represents everything we get back from the typecheck of a file.
/// It acts like an in-memory database about the file.
/// It is effectively immutable and not updated: when we re-typecheck we just drop the previous
/// scope object on the floor and make a new one.
[<Sealed>]
type internal TypeCheckInfo
    (
        _sTcConfig: TcConfig,
        g: TcGlobals,
        ccuSigForFile: ModuleOrNamespaceType,
        thisCcu: CcuThunk,
        tcImports: TcImports,
        tcAccessRights: AccessorDomain,
        projectFileName: string,
        mainInputFileName: string,
        projectOptions: FSharpProjectOptions,
        sResolutions: TcResolutions,
        sSymbolUses: TcSymbolUses,
        sFallback: NameResolutionEnv,
        loadClosure: LoadClosure option,
        implFileOpt: CheckedImplFile option,
        openDeclarations: OpenDeclaration[]
    ) =

    let amap = tcImports.GetImportMap()
    let infoReader = InfoReader(g, amap)
    let ncenv = NameResolver(g, amap, infoReader, FakeInstantiationGenerator)
    let cenv = SymbolEnv(g, thisCcu, Some ccuSigForFile, tcImports, amap, infoReader)

    /// Find the most precise naming environment for the given line and column
    let GetBestEnvForPos cursorPos =

        let mutable bestSoFar = None

        // Find the most deeply nested enclosing scope that contains given position
        sResolutions.CapturedEnvs
        |> ResizeArray.iter (fun (mPossible, env, ad) ->
            if rangeContainsPos mPossible cursorPos then
                match bestSoFar with
                | Some(bestm, _, _) ->
                    if rangeContainsRange bestm mPossible then
                        bestSoFar <- Some(mPossible, env, ad)
                | None -> bestSoFar <- Some(mPossible, env, ad))

        let mostDeeplyNestedEnclosingScope = bestSoFar

        // Look for better subtrees on the r.h.s. of the subtree to the left of where we are
        // Should really go all the way down the r.h.s. of the subtree to the left of where we are
        // This is all needed when the index is floating free in the area just after the environment we really want to capture
        // We guarantee to only refine to a more nested environment.  It may not be strictly
        // the right environment, but will always be at least as rich

        let mutable bestAlmostIncludedSoFar = None

        sResolutions.CapturedEnvs
        |> ResizeArray.iter (fun (mPossible, env, ad) ->
            // take only ranges that strictly do not include cursorPos (all ranges that touch cursorPos were processed during 'Strict Inclusion' part)
            if rangeBeforePos mPossible cursorPos && not (posEq mPossible.End cursorPos) then
                let contained =
                    match mostDeeplyNestedEnclosingScope with
                    | Some(bestm, _, _) -> rangeContainsRange bestm mPossible
                    | None -> true

                if contained then
                    match bestAlmostIncludedSoFar with
                    | Some(mRight: range, _, _) ->
                        if
                            posGt mPossible.End mRight.End
                            || (posEq mPossible.End mRight.End && posGt mPossible.Start mRight.Start)
                        then
                            bestAlmostIncludedSoFar <- Some(mPossible, env, ad)
                    | _ -> bestAlmostIncludedSoFar <- Some(mPossible, env, ad))

        let resEnv =
            match bestAlmostIncludedSoFar, mostDeeplyNestedEnclosingScope with
            | Some(_, env, ad), None -> env, ad
            | Some(_, almostIncludedEnv, ad), Some(_, mostDeeplyNestedEnv, _) when
                almostIncludedEnv.eFieldLabels.Count >= mostDeeplyNestedEnv.eFieldLabels.Count
                ->
                almostIncludedEnv, ad
            | _ ->
                match mostDeeplyNestedEnclosingScope with
                | Some(_, env, ad) -> env, ad
                | None -> sFallback, AccessibleFromSomeFSharpCode

        let pm = mkRange mainInputFileName cursorPos cursorPos

        resEnv, pm

    /// The items that come back from ResolveCompletionsInType are a bit
    /// noisy. Filter a few things out.
    ///
    /// e.g. prefer types to constructors for ToolTipText
    let FilterItemsForCtors filterCtors (items: ItemWithInst list) =
        let items =
            items
            |> List.filter (fun item ->
                match item.Item with
                | Item.CtorGroup _ when filterCtors = ResolveTypeNamesToTypeRefs -> false
                | _ -> true)

        items

    // Filter items to show only valid & return Some if there are any
    let ReturnItemsOfType (items: ItemWithInst list) g denv (m: range) filterCtors =
        let items =
            items
            |> RemoveDuplicateItems g
            |> RemoveExplicitlySuppressed g
            |> FilterItemsForCtors filterCtors

        if not (isNil items) then
            NameResResult.Members(items, denv, m)
        else
            NameResResult.Empty

    let GetCapturedNameResolutions (endOfNamesPos: pos) resolveOverloads =
        let filter (endPos: pos) items =
            items
            |> ResizeArray.filter (fun (cnr: CapturedNameResolution) ->
                let range = cnr.Range
                range.EndLine = endPos.Line && range.EndColumn = endPos.Column)

        match resolveOverloads with
        | ResolveOverloads.Yes -> filter endOfNamesPos sResolutions.CapturedNameResolutions

        | ResolveOverloads.No ->
            let items = filter endOfNamesPos sResolutions.CapturedMethodGroupResolutions

            if items.Count <> 0 then
                items
            else
                filter endOfNamesPos sResolutions.CapturedNameResolutions

    /// Looks at the exact name resolutions that occurred during type checking
    /// If 'membersByResidue' is specified, we look for members of the item obtained
    /// from the name resolution and filter them by the specified residue (?)
    let GetPreciseItemsFromNameResolution (line, colAtEndOfNames, membersByResidue, filterCtors, resolveOverloads) =
        let endOfNamesPos = mkPos line colAtEndOfNames

        // Logic below expects the list to be in reverse order of resolution
        let cnrs =
            GetCapturedNameResolutions endOfNamesPos resolveOverloads
            |> ResizeArray.toList
            |> List.rev

        match cnrs, membersByResidue with

        // Exact resolution via SomeType.$ or SomeType<int>.$
        //
        // If we're looking for members using a residue, we'd expect only
        // a single item (pick the first one) and we need the residue (which may be "")
        | _, _ -> NameResResult.Empty

    let TryGetTypeFromNameResolution (line, colAtEndOfNames, membersByResidue, resolveOverloads) =
        let endOfNamesPos = mkPos line colAtEndOfNames

        let items =
            GetCapturedNameResolutions endOfNamesPos resolveOverloads
            |> ResizeArray.toList
            |> List.rev

        match items, membersByResidue with
        | _, _ -> None

    let CollectParameters (methods: MethInfo list) amap m : Item list =
        methods
        |> List.collect (fun meth ->
            match meth.GetParamDatas(amap, m, meth.FormalMethodInst) with
            | x :: _ ->
                x
                |> List.choose (fun (ParamData(_isParamArray, _isInArg, _isOutArg, _optArgInfo, _callerInfo, name, _, ty)) ->
                    match name with
                    | Some id -> Some(Item.OtherName(Some id, ty, None, Some(ArgumentContainer.Method meth), id.idRange))
                    | None -> None)
            | _ -> [])

    let GetNamedParametersAndSettableFields endOfExprPos =
        let cnrs =
            GetCapturedNameResolutions endOfExprPos ResolveOverloads.No
            |> ResizeArray.toList
            |> List.rev

        let result =
            match cnrs with
            | _ -> None

        match result with
        | None -> NameResResult.Empty
        | Some(denv, m, items) ->
            let items = List.map ItemWithNoInst items
            ReturnItemsOfType items g denv m TypeNameResolutionFlag.ResolveTypeNamesToTypeRefs

    /// finds captured typing for the given position
    let GetExprTypingForPosition endOfExprPos =
        let quals =
            sResolutions.CapturedExpressionTypings
            |> Seq.filter (fun (ty, nenv, _, m) ->
                // We only want expression types that end at the particular position in the file we are looking at.
                posEq m.End endOfExprPos
                &&

                // Get rid of function types.  True, given a 2-arg curried function "f x y", it is legal to do "(f x).GetType()",
                // but you almost never want to do this in practice, and we choose not to offer up any intellisense for
                // F# function types.
                not (isFunTy nenv.DisplayEnv.g ty))
            |> Seq.toArray

        // filter out errors

        let quals =
            quals
            |> Array.filter (fun (ty, nenv, _, _) ->
                let denv = nenv.DisplayEnv
                not (isTyparTy denv.g ty && (destTyparTy denv.g ty).IsFromError))

        let thereWereSomeQuals = not (Array.isEmpty quals)
        thereWereSomeQuals, quals

    /// Returns the list of available record fields, taking into account potential nesting
    let GetRecdFieldsForCopyAndUpdateExpr (identRange: range, plid: string list) =
        let rec dive ty (denv: DisplayEnv) ad m plid isPastTypePrefix wasPathEmpty =
            if isRecdTy denv.g ty then
                let fields =
                    ncenv.InfoReader.GetRecordOrClassFieldsOfType(None, ad, m, ty)
                    |> List.filter (fun rfref -> not rfref.IsStatic && IsFieldInfoAccessible ad rfref)

                match plid with
                | [] ->
                    if wasPathEmpty || isPastTypePrefix then
                        Some(fields |> List.map Item.RecdField, denv, m)
                    else
                        None
                | id :: rest ->
                    match fields |> List.tryFind (fun f -> f.LogicalName = id) with
                    | Some f -> dive f.FieldType denv ad m rest true wasPathEmpty
                    | _ ->
                        // Field name can be optionally qualified.
                        // If we haven't matched a field name yet, keep peeling off the prefix.
                        if isPastTypePrefix then
                            Some([], denv, m)
                        else
                            dive ty denv ad m rest false wasPathEmpty
            else
                match tryDestAnonRecdTy denv.g ty with
                | ValueSome(anonInfo, tys) ->
                    match plid with
                    | [] ->
                        let items =
                            [
                                for i in 0 .. anonInfo.SortedIds.Length - 1 do
                                    Item.AnonRecdField(anonInfo, tys, i, anonInfo.SortedIds[i].idRange)
                            ]

                        Some(items, denv, m)
                    | id :: rest ->
                        match anonInfo.SortedNames |> Array.tryFindIndex (fun x -> x = id) with
                        | Some i -> dive tys[i] denv ad m rest true wasPathEmpty
                        | _ -> Some([], denv, m)
                | ValueNone -> Some([], denv, m)

        match
            GetExprTypingForPosition identRange.End
            |> snd
            |> Array.tryFind (fun (_, _, _, rq) -> posEq identRange.Start rq.Start)
        with
        | Some(ty, nenv, ad, m) -> dive ty nenv.DisplayEnv ad m plid false plid.IsEmpty
        | _ -> None

    /// Find items in the best naming environment.
    let GetEnvironmentLookupResolutions (nenv, ad, m, plid, filterCtors, showObsolete) =
        let items =
            ResolvePartialLongIdent ncenv nenv (ConstraintSolver.IsApplicableMethApprox g amap m) m ad plid showObsolete

        let items = items |> List.map ItemWithNoInst
        let items = items |> RemoveDuplicateItems g
        let items = items |> RemoveExplicitlySuppressed g
        let items = items |> FilterItemsForCtors filterCtors
        (items, nenv.DisplayEnv, m)

    /// Find items in the best naming environment.
    let GetEnvironmentLookupResolutionsAtPosition (cursorPos, plid, filterCtors, showObsolete) =
        let (nenv, ad), m = GetBestEnvForPos cursorPos
        GetEnvironmentLookupResolutions(nenv, ad, m, plid, filterCtors, showObsolete)

    /// Find record fields in the best naming environment.
    let GetClassOrRecordFieldsEnvironmentLookupResolutions (cursorPos, plid, fieldsOnly) =
        let (nenv, ad), m = GetBestEnvForPos cursorPos

        let items =
            ResolvePartialLongIdentToClassOrRecdFields ncenv nenv m ad plid false fieldsOnly

        let items = items |> List.map ItemWithNoInst
        let items = items |> RemoveDuplicateItems g
        let items = items |> RemoveExplicitlySuppressed g
        items, nenv.DisplayEnv, m

    /// Is the item suitable for completion at "inherits $"
    let IsInheritsCompletionCandidate item =
        match item with
        | Item.ModuleOrNamespaces _ -> true
        | Item.Types(_, ty :: _) when isClassTy g ty && not (isSealedTy g ty) -> true
        | _ -> false

    /// Is the item suitable for completion at "interface $"
    let IsInterfaceCompletionCandidate item =
        match item with
        | Item.ModuleOrNamespaces _ -> true
        | Item.Types(_, ty :: _) when isInterfaceTy g ty -> true
        | _ -> false

    /// Return only items with the specified name, modulo "Attribute" for type completions
    let FilterDeclItemsByResidue (getItem: 'a -> Item) residue (items: 'a list) =
        let attributedResidue = residue + "Attribute"

        let nameMatchesResidue name =
            (residue = name) || (attributedResidue = name)

        items
        |> List.filter (fun x ->
            let item = getItem x
            let n1 = item.DisplayName

            match item with
            | Item.Types _ -> nameMatchesResidue n1
            | Item.CtorGroup(_, meths) ->
                nameMatchesResidue n1
                || meths
                   |> List.exists (fun meth ->
                       let tcref = meth.ApparentEnclosingTyconRef
#if !NO_TYPEPROVIDERS
                       tcref.IsProvided
                       ||
#endif
                       nameMatchesResidue tcref.DisplayName)
            | Item.Value v when (v.IsPropertyGetterMethod || v.IsPropertySetterMethod) -> residue = v.Id.idText || residue = n1
            | _ -> residue = n1)

    /// Post-filter items to make sure they have precisely the right name
    /// This also checks that there are some remaining results
    /// exactMatchResidueOpt = Some _ -- means that we are looking for exact matches
    let FilterRelevantItemsBy (getItem: 'a -> Item) (exactMatchResidueOpt: string option) check (items: 'a list, denv, m) =
        // can throw if type is in located in non-resolved CCU: i.e. bigint if reference to System.Numerics is absent
        let inline safeCheck item =
            try
                check item
            with _ ->
                false

        // Are we looking for items with precisely the given name?
        if isNil items then
            // When (items = []) we must returns Some([],..) and not None
            // because this value is used if we want to stop further processing (e.g. let x.$ = ...)
            Some(items, denv, m)
        else
            match exactMatchResidueOpt with
            | Some exactMatchResidue ->
                let items =
                    items
                    |> FilterDeclItemsByResidue getItem exactMatchResidue
                    |> List.filter safeCheck

                if not (isNil items) then Some(items, denv, m) else None
            | _ ->
                let items = items |> List.filter safeCheck
                Some(items, denv, m)

    /// Post-filter items to make sure they have precisely the right name
    /// This also checks that there are some remaining results
    let (|FilterRelevantItems|_|) getItem exactMatchResidueOpt orig =
        FilterRelevantItemsBy getItem exactMatchResidueOpt (fun _ -> true) orig

    /// Find the first non-whitespace position in a line prior to the given character
    let FindFirstNonWhitespacePosition (lineStr: string) i =
        if i >= lineStr.Length then
            None
        else
            let mutable p = i

            while p >= 0 && Char.IsWhiteSpace(lineStr[p]) do
                p <- p - 1

            if p >= 0 then Some p else None

    let DefaultCompletionItem item = failwith "whatever"

    let getItem (x: ItemWithInst) = x.Item

    /// Gets all field identifiers of a union case that can be referred to in a pattern.
    let GetUnionCaseFields caseIdRange alreadyReferencedFields =
        sResolutions.CapturedNameResolutions
        |> ResizeArray.tryPick (fun r ->
            match r.Item with
            | Item.UnionCase(uci, _) when equals r.Range caseIdRange ->
                uci.UnionCase.RecdFields
                |> List.indexed
                |> List.choose (fun (index, field) ->
                    if List.contains field.LogicalName alreadyReferencedFields then
                        None
                    else
                        Item.UnionCaseField(uci, index)
                        |> ItemWithNoInst
                        |> DefaultCompletionItem
                        |> Some)
                |> Some
            | _ -> None)

    let toCompletionItems (items: ItemWithInst list, denv: DisplayEnv, m: range) =
        items |> List.map DefaultCompletionItem, denv, m

    /// Find record fields in the best naming environment.
    let GetEnvironmentLookupResolutionsIncludingRecordFieldsAtPosition cursorPos plid envItems =
        // An empty record expression may be completed into something like these:
        // { XXX = ... }
        // { xxx with XXX ... }
        // Provide both expression items in scope and available record fields.
        let (nenv, _), m = GetBestEnvForPos cursorPos

        let fieldItems, _, _ =
            GetClassOrRecordFieldsEnvironmentLookupResolutions(cursorPos, plid, true)

        let fieldCompletionItems, _, _ as fieldsResult =
            (fieldItems, nenv.DisplayEnv, m) |> toCompletionItems

        match envItems with
        | Some(items, denv, m) -> Some(fieldCompletionItems @ items, denv, m)
        | _ -> Some(fieldsResult)

    /// Return 'false' if this is not a completion item valid in an interface file.
    let IsValidSignatureFileItem item =
        match item with
        | Item.TypeVar _
        | Item.Types _
        | Item.ModuleOrNamespaces _ -> true
        | _ -> false

    /// Find the most precise display context for the given line and column.
    member _.GetBestDisplayEnvForPos cursorPos = GetBestEnvForPos cursorPos

    member _.GetVisibleNamespacesAndModulesAtPosition(cursorPos: pos) : ModuleOrNamespaceRef list =
        let (nenv, ad), m = GetBestEnvForPos cursorPos
        GetVisibleNamespacesAndModulesAtPoint ncenv nenv OpenQualified m ad

    /// Determines if a long ident is resolvable at a specific point.
    member _.IsRelativeNameResolvable(cursorPos: pos, plid: string list, item: Item) : bool =
        DiagnosticsScope.Protect
            range0
            (fun () ->
                /// Find items in the best naming environment.
                let (nenv, ad), m = GetBestEnvForPos cursorPos
                IsItemResolvable ncenv nenv m ad plid item)
            (fun msg ->
                Trace.TraceInformation(sprintf "FCS: recovering from error in IsRelativeNameResolvable: '%s'" msg)
                false)

    /// Determines if a long ident is resolvable at a specific point.
    member scope.IsRelativeNameResolvableFromSymbol(cursorPos: pos, plid: string list, symbol: FSharpSymbol) : bool =
        scope.IsRelativeNameResolvable(cursorPos, plid, symbol.Item)

    member _.PartialAssemblySignatureForFile =
        FSharpAssemblySignature(g, thisCcu, ccuSigForFile, tcImports, None, ccuSigForFile)

    member _.AccessRights = tcAccessRights

    member _.ProjectOptions = projectOptions

    member _.GetReferencedAssemblies() =
        [
            for x in tcImports.GetImportedAssemblies() do
                FSharpAssembly(g, tcImports, x.FSharpViewOfMetadata)
        ]

    member _.GetFormatSpecifierLocationsAndArity() =
        sSymbolUses.GetFormatSpecifierLocationsAndArity()

    /// The resolutions in the file
    member _.ScopeResolutions = sResolutions

    /// The uses of symbols in the analyzed file
    member _.ScopeSymbolUses = sSymbolUses

    member _.TcGlobals = g

    member _.TcImports = tcImports

    /// The inferred signature of the file
    member _.CcuSigForFile = ccuSigForFile

    /// The assembly being analyzed
    member _.ThisCcu = thisCcu

    member _.ImplementationFile = implFileOpt

    /// All open declarations in the file, including auto open modules
    member _.OpenDeclarations = openDeclarations

    member _.SymbolEnv = cenv

    override _.ToString() =
        "TypeCheckInfo(" + mainInputFileName + ")"

type FSharpParsingOptions =
    {
        SourceFiles: string[]
        ApplyLineDirectives: bool
        ConditionalDefines: string list
        DiagnosticOptions: FSharpDiagnosticOptions
        LangVersionText: string
        IsInteractive: bool
        IndentationAwareSyntax: bool option
        StrictIndentation: bool option
        CompilingFSharpCore: bool
        IsExe: bool
    }

    member x.LastFileName =
        Debug.Assert(not (Array.isEmpty x.SourceFiles), "Parsing options don't contain any file")
        Array.last x.SourceFiles

    static member Default =
        {
            SourceFiles = Array.empty
            ApplyLineDirectives = false
            ConditionalDefines = []
            DiagnosticOptions = FSharpDiagnosticOptions.Default
            LangVersionText = LanguageVersion.Default.VersionText
            IsInteractive = false
            IndentationAwareSyntax = None
            StrictIndentation = None
            CompilingFSharpCore = false
            IsExe = false
        }

    static member FromTcConfig(tcConfig: TcConfig, sourceFiles, isInteractive: bool) =
        {
            SourceFiles = sourceFiles
            ApplyLineDirectives = tcConfig.applyLineDirectives
            ConditionalDefines = tcConfig.conditionalDefines
            DiagnosticOptions = tcConfig.diagnosticsOptions
            LangVersionText = tcConfig.langVersion.VersionText
            IsInteractive = isInteractive
            IndentationAwareSyntax = tcConfig.indentationAwareSyntax
            StrictIndentation = tcConfig.strictIndentation
            CompilingFSharpCore = tcConfig.compilingFSharpCore
            IsExe = tcConfig.target.IsExe
        }

    static member FromTcConfigBuilder(tcConfigB: TcConfigBuilder, sourceFiles, isInteractive: bool) =
        {
            SourceFiles = sourceFiles
            ApplyLineDirectives = tcConfigB.applyLineDirectives
            ConditionalDefines = tcConfigB.conditionalDefines
            DiagnosticOptions = tcConfigB.diagnosticsOptions
            LangVersionText = tcConfigB.langVersion.VersionText
            IsInteractive = isInteractive
            IndentationAwareSyntax = tcConfigB.indentationAwareSyntax
            StrictIndentation = tcConfigB.strictIndentation
            CompilingFSharpCore = tcConfigB.compilingFSharpCore
            IsExe = tcConfigB.target.IsExe
        }

module internal ParseAndCheckFile =

    /// Diagnostics handler for parsing & type checking while processing a single file
    type DiagnosticsHandler
        (
            reportErrors,
            mainInputFileName,
            diagnosticsOptions: FSharpDiagnosticOptions,
            sourceText: ISourceText,
            suggestNamesForErrors: bool,
            flatErrors: bool
        ) =
        let mutable options = diagnosticsOptions
        let diagnosticsCollector = ResizeArray<_>()
        let mutable errorCount = 0

        // We'll need number of lines for adjusting error messages at EOF
        let fileInfo = sourceText.GetLastCharacterPosition()

        let collectOne severity diagnostic =
            // 1. Extended diagnostic data should be created after typechecking because it requires a valid SymbolEnv
            // 2. Diagnostic message should be created during the diagnostic sink, because after typechecking
            //    the formatting of types in it may change (for example, 'a to obj)
            //
            // So we'll create a diagnostic later, but cache the FormatCore message now
            diagnostic.Exception.Data["CachedFormatCore"] <- diagnostic.FormatCore(flatErrors, suggestNamesForErrors)
            diagnosticsCollector.Add(struct (diagnostic, severity))

            if severity = FSharpDiagnosticSeverity.Error then
                errorCount <- errorCount + 1

        // This function gets called whenever an error happens during parsing or checking
        let diagnosticSink severity (diagnostic: PhasedDiagnostic) =
            // Sanity check here. The phase of an error should be in a phase known to the language service.
            let diagnostic =
                if not (diagnostic.IsPhaseInCompile()) then
                    // Reaching this point means that the error would be sticky if we let it prop up to the language service.
                    // Assert and recover by replacing phase with one known to the language service.
                    Trace.TraceInformation(
                        sprintf
                            "The subcategory '%s' seen in an error should not be seen by the language service"
                            (diagnostic.Subcategory())
                    )

                    { diagnostic with
                        Phase = BuildPhase.TypeCheck
                    }
                else
                    diagnostic

            if reportErrors then
                match diagnostic with
#if !NO_TYPEPROVIDERS
                | {
                      Exception = :? TypeProviderError as tpe
                  } -> tpe.Iter(fun exn -> collectOne severity { diagnostic with Exception = exn })
#endif
                | _ -> collectOne severity diagnostic

        let diagnosticsLogger =
            { new DiagnosticsLogger("DiagnosticsHandler") with
                member _.DiagnosticSink(exn, severity) = diagnosticSink severity exn
                member _.ErrorCount = errorCount
            }

        // Public members
        member _.DiagnosticsLogger = diagnosticsLogger

        member _.ErrorCount = errorCount

        member _.DiagnosticOptions
            with set opts = options <- opts

        member _.AnyErrors = errorCount > 0

        member _.CollectedPhasedDiagnostics =
            [|
                for struct (diagnostic, severity) in diagnosticsCollector -> diagnostic, severity
            |]

        member _.CollectedDiagnostics(symbolEnv: SymbolEnv option) =
            [|
                for struct (diagnostic, severity) in diagnosticsCollector do
                    yield!
                        DiagnosticHelpers.ReportDiagnostic(
                            options,
                            false,
                            mainInputFileName,
                            fileInfo,
                            diagnostic,
                            severity,
                            suggestNamesForErrors,
                            flatErrors,
                            symbolEnv
                        )
            |]

    let getLightSyntaxStatus fileName options =
        let indentationAwareSyntaxOnByDefault =
            List.exists (FileSystemUtils.checkSuffix fileName) FSharpIndentationAwareSyntaxFileSuffixes

        let indentationSyntaxStatus =
            if indentationAwareSyntaxOnByDefault then
                (options.IndentationAwareSyntax <> Some false)
            else
                (options.IndentationAwareSyntax = Some true)

        IndentationAwareSyntaxStatus(indentationSyntaxStatus, true)

    let createLexerFunction fileName options lexbuf (errHandler: DiagnosticsHandler) (ct: CancellationToken) =
        let indentationSyntaxStatus = getLightSyntaxStatus fileName options

        // If we're editing a script then we define INTERACTIVE otherwise COMPILED.
        // Since this parsing for intellisense we always define EDITING.
        let conditionalDefines =
            [ "blah" ]

        // Note: we don't really attempt to intern strings across a large scope.
        let lexResourceManager = LexResourceManager()

        // When analyzing files using ParseOneFile, i.e. for the use of editing clients, we do not apply line directives.
        // TODO(pathmap): expose PathMap on the service API, and thread it through here
        let lexargs =
            mkLexargs (
                conditionalDefines,
                indentationSyntaxStatus,
                lexResourceManager,
                [],
                errHandler.DiagnosticsLogger,
                PathMap.empty,
                options.ApplyLineDirectives
            )

        let tokenizer =
            LexFilter.LexFilter(indentationSyntaxStatus, options.CompilingFSharpCore, Lexer.token lexargs true, lexbuf, false)

        if ct.CanBeCanceled then
            (fun _ ->
                ct.ThrowIfCancellationRequested()
                tokenizer.GetToken())
        else
            (fun _ -> tokenizer.GetToken())

    let createLexbuf langVersion strictIndentation sourceText =
        UnicodeLexing.SourceTextAsLexbuf(true, LanguageVersion(langVersion), strictIndentation, sourceText)

    let matchBraces
        (
            sourceText: ISourceText,
            fileName,
            options: FSharpParsingOptions,
            userOpName: string,
            suggestNamesForErrors: bool,
            ct: CancellationToken
        ) =
        // Make sure there is an DiagnosticsLogger installed whenever we do stuff that might record errors, even if we ultimately ignore the errors
        let delayedLogger = CapturingDiagnosticsLogger("matchBraces")
        use _ = UseDiagnosticsLogger delayedLogger
        use _ = UseBuildPhase BuildPhase.Parse

        Trace.TraceInformation("FCS: {0}.{1} ({2})", userOpName, "matchBraces", fileName)

        let matchingBraces = ResizeArray<_>()

        usingLexbufForParsing (createLexbuf options.LangVersionText options.StrictIndentation sourceText, fileName) (fun lexbuf ->
            let errHandler =
                DiagnosticsHandler(false, fileName, options.DiagnosticOptions, sourceText, suggestNamesForErrors, false)

            let lexfun = createLexerFunction fileName options lexbuf errHandler ct

            let parenTokensBalance t1 t2 =
                match t1, t2 with
                | LPAREN, RPAREN
                | LPAREN, RPAREN_IS_HERE
                | LBRACE _, RBRACE _
                | LBRACE_BAR, BAR_RBRACE
                | LBRACE _, RBRACE_IS_HERE
                | INTERP_STRING_BEGIN_PART _, INTERP_STRING_END _
                | INTERP_STRING_BEGIN_PART _, INTERP_STRING_PART _
                | INTERP_STRING_PART _, INTERP_STRING_PART _
                | INTERP_STRING_PART _, INTERP_STRING_END _
                | SIG, END
                | STRUCT, END
                | LBRACK_BAR, BAR_RBRACK
                | LBRACK, RBRACK
                | LBRACK_LESS, GREATER_RBRACK
                | BEGIN, END -> true
                | LQUOTE q1, RQUOTE q2 -> q1 = q2
                | _ -> false

            let rec matchBraces stack =
                match lexfun lexbuf, stack with
                | tok2, (tok1, braceOffset, m1) :: stackAfterMatch when parenTokensBalance tok1 tok2 ->
                    let m2 = lexbuf.LexemeRange

                    // For INTERP_STRING_PART and INTERP_STRING_END grab the one character
                    // range that corresponds to the "}" at the start of the token
                    let m2Start =
                        match tok2 with
                        | INTERP_STRING_PART _
                        | INTERP_STRING_END _ ->
                            mkFileIndexRange
                                m2.FileIndex
                                (mkPos m2.Start.Line (m2.Start.Column - braceOffset))
                                (mkPos m2.Start.Line (m2.Start.Column + 1))
                        | _ -> m2

                    matchingBraces.Add(m1, m2Start)

                    // INTERP_STRING_PART corresponds to both "} ... {" i.e. both the completion
                    // of a match and the start of a potential new one.
                    let stackAfterMatch =
                        match tok2 with
                        | INTERP_STRING_PART _ ->
                            let m2End =
                                mkFileIndexRange m2.FileIndex (mkPos m2.End.Line (max (m2.End.Column - 1 - braceOffset) 0)) m2.End

                            (tok2, braceOffset, m2End) :: stackAfterMatch
                        | _ -> stackAfterMatch

                    matchBraces stackAfterMatch

                | LPAREN | LBRACE _ | LBRACK | LBRACE_BAR | LBRACK_BAR | LQUOTE _ | LBRACK_LESS as tok, _ ->
                    matchBraces ((tok, 0, lexbuf.LexemeRange) :: stack)

                // INTERP_STRING_BEGIN_PART corresponds to $"... {" at the start of an interpolated string
                //
                // INTERP_STRING_PART corresponds to "} ... {" in the middle of an interpolated string (in
                //   this case it msut not have matched something on the stack, e.g. an incomplete '[' in the
                //   interpolation expression)
                //
                // Either way we start a new potential match at the last character
                | INTERP_STRING_BEGIN_PART _ | INTERP_STRING_PART _ as tok, _ ->
                    let braceOffset =
                        match tok with
                        | INTERP_STRING_BEGIN_PART(_, SynStringKind.TripleQuote, (LexerContinuation.Token(_, (_, _, dl, _, _) :: _))) ->
                            dl - 1
                        | _ -> 0

                    let m = lexbuf.LexemeRange

                    let m2 =
                        mkFileIndexRange m.FileIndex (mkPos m.End.Line (max (m.End.Column - 1 - braceOffset) 0)) m.End

                    matchBraces ((tok, braceOffset, m2) :: stack)

                | (EOF _ | LEX_FAILURE _), _ -> ()
                | _ -> matchBraces stack

            matchBraces [])

        matchingBraces.ToArray()

    let parseFile
        (
            sourceText: ISourceText,
            fileName: string,
            options: FSharpParsingOptions,
            userOpName: string,
            suggestNamesForErrors: bool,
            flatErrors: bool,
            identCapture: bool,
            ct: CancellationToken
        ) =
        Trace.TraceInformation("FCS: {0}.{1} ({2})", userOpName, "parseFile", fileName)

        use act =
            Activity.start "ParseAndCheckFile.parseFile" [| Activity.Tags.fileName, fileName |]

        let errHandler =
            DiagnosticsHandler(true, fileName, options.DiagnosticOptions, sourceText, suggestNamesForErrors, flatErrors)

        use _ = UseDiagnosticsLogger errHandler.DiagnosticsLogger

        use _ = UseBuildPhase BuildPhase.Parse

        let parseResult =
            usingLexbufForParsing (createLexbuf options.LangVersionText options.StrictIndentation sourceText, fileName) (fun lexbuf ->

                let lexfun = createLexerFunction fileName options lexbuf errHandler ct

                let isLastCompiland =
                    fileName.Equals(options.LastFileName, StringComparison.CurrentCultureIgnoreCase)
                    || IsScript(fileName)

                let isExe = options.IsExe

                try
                    ParseInput(
                        lexfun,
                        options.DiagnosticOptions,
                        errHandler.DiagnosticsLogger,
                        lexbuf,
                        None,
                        fileName,
                        (isLastCompiland, isExe),
                        identCapture,
                        Some userOpName
                    )
                with
                | :? OperationCanceledException -> reraise ()
                | e ->
                    errHandler.DiagnosticsLogger.StopProcessingRecovery e range0 // don't re-raise any exceptions, we must return None.
                    EmptyParsedInput(fileName, (isLastCompiland, isExe)))

        errHandler.CollectedDiagnostics(None), parseResult, errHandler.AnyErrors

    let ApplyLoadClosure
        (
            tcConfig,
            parsedMainInput,
            mainInputFileName: string,
            loadClosure: LoadClosure option,
            tcImports: TcImports,
            backgroundDiagnostics
        ) =

        // If additional references were brought in by the preprocessor then we need to process them
        match loadClosure with
        | Some loadClosure ->
            // Play unresolved references for this file.
            tcImports.ReportUnresolvedAssemblyReferences(loadClosure.UnresolvedReferences)

            // If there was a loadClosure, replay the errors and warnings from resolution, excluding parsing
            loadClosure.LoadClosureRootFileDiagnostics |> List.iter diagnosticSink

            let fileOfBackgroundError (diagnostic: PhasedDiagnostic, _) =
                match diagnostic.Range with
                | Some m -> Some m.FileName
                | None -> None

            let sameFile file hashLoadInFile =
                match file with
                | None -> false
                | Some file -> (0 = String.Compare(hashLoadInFile, file, StringComparison.OrdinalIgnoreCase))

            //  walk the list of #loads and keep the ones for this file.
            let hashLoadsInFile =
                loadClosure.SourceFiles |> List.filter (fun (_, ms) -> ms <> []) // #loaded file, ranges of #load

            let hashLoadBackgroundDiagnostics, otherBackgroundDiagnostics =
                backgroundDiagnostics
                |> Array.partition (fun backgroundError ->
                    hashLoadsInFile
                    |> List.exists (fst >> sameFile (fileOfBackgroundError backgroundError)))

            // Create single errors for the #load-ed files.
            // Group errors and warnings by file name.
            let hashLoadBackgroundDiagnosticsGroupedByFileName =
                hashLoadBackgroundDiagnostics
                |> Array.map (fun err -> fileOfBackgroundError err, err)
                |> Array.groupBy fst // fileWithErrors, error list

            //  Join the sets and report errors.
            //  It is by-design that these messages are only present in the language service. A true build would report the errors at their
            //  spots in the individual source files.
            for fileOfHashLoad, rangesOfHashLoad in hashLoadsInFile do
                for file, errorGroupedByFileName in hashLoadBackgroundDiagnosticsGroupedByFileName do
                    if sameFile file fileOfHashLoad then
                        for rangeOfHashLoad in rangesOfHashLoad do // Handle the case of two #loads of the same file
                            let diagnostics =
                                errorGroupedByFileName |> Array.map (fun (_, (pe, f)) -> pe.Exception, f) // Strip the build phase here. It will be replaced, in total, with TypeCheck

                            let errors =
                                [
                                    for err, severity in diagnostics do
                                        if severity = FSharpDiagnosticSeverity.Error then
                                            err
                                ]

                            let warnings =
                                [
                                    for err, severity in diagnostics do
                                        if severity = FSharpDiagnosticSeverity.Warning then
                                            err
                                ]

                            let infos =
                                [
                                    for err, severity in diagnostics do
                                        if severity = FSharpDiagnosticSeverity.Info then
                                            err
                                ]

                            let message = HashLoadedSourceHasIssues(infos, warnings, errors, rangeOfHashLoad)

                            if isNil errors && isNil warnings then warning message
                            elif isNil errors then warning message
                            else errorR message

            // Replay other background errors.
            for diagnostic, severity in otherBackgroundDiagnostics do
                match severity with
                | FSharpDiagnosticSeverity.Info -> informationalWarning diagnostic.Exception
                | FSharpDiagnosticSeverity.Warning -> warning diagnostic.Exception
                | FSharpDiagnosticSeverity.Error -> errorR diagnostic.Exception
                | FSharpDiagnosticSeverity.Hidden -> ()

        | None ->
            // For non-scripts, check for disallow #r and #load.
            ApplyMetaCommandsFromInputToTcConfig(
                tcConfig,
                parsedMainInput,
                !! Path.GetDirectoryName(mainInputFileName),
                tcImports.DependencyProvider
            )
            |> ignore

    // Type check a single file against an initial context, gleaning both errors and intellisense information.
    let CheckOneFile
        (
            parseResults: FSharpParseFileResults,
            sourceText: ISourceText,
            mainInputFileName: string,
            projectOptions: FSharpProjectOptions,
            projectFileName: string,
            tcConfig: TcConfig,
            tcGlobals: TcGlobals,
            tcImports: TcImports,
            tcState: TcState,
            moduleNamesDict: ModuleNamesDict,
            loadClosure: LoadClosure option,
            backgroundDiagnostics: (PhasedDiagnostic * FSharpDiagnosticSeverity)[],
            suggestNamesForErrors: bool
        ) =

        cancellable {
            use _ =
                Activity.start
                    "ParseAndCheckFile.CheckOneFile"
                    [|
                        Activity.Tags.fileName, mainInputFileName
                        Activity.Tags.length, sourceText.Length.ToString()
                    |]

            let parsedMainInput = parseResults.ParseTree

            // Initialize the error handler
            let errHandler =
                DiagnosticsHandler(
                    true,
                    mainInputFileName,
                    tcConfig.diagnosticsOptions,
                    sourceText,
                    suggestNamesForErrors,
                    tcConfig.flatErrors
                )

            use _ = UseDiagnosticsLogger errHandler.DiagnosticsLogger

            use _unwindBP = UseBuildPhase BuildPhase.TypeCheck

            // Apply nowarns to tcConfig (may generate errors, so ensure diagnosticsLogger is installed)
            let tcConfig =
                ApplyNoWarnsToTcConfig(tcConfig, parsedMainInput, !! Path.GetDirectoryName(mainInputFileName))

            // update the error handler with the modified tcConfig
            errHandler.DiagnosticOptions <- tcConfig.diagnosticsOptions

            // If additional references were brought in by the preprocessor then we need to process them
            ApplyLoadClosure(tcConfig, parsedMainInput, mainInputFileName, loadClosure, tcImports, backgroundDiagnostics)

            // Typecheck the real input.
            let sink = TcResultsSinkImpl(tcGlobals, sourceText = sourceText)

            let! resOpt =
                cancellable {
                    try
                        let checkForErrors () =
                            (parseResults.ParseHadErrors || errHandler.ErrorCount > 0)

                        let parsedMainInput, _moduleNamesDict =
                            DeduplicateParsedInputModuleName moduleNamesDict parsedMainInput

                        // Typecheck is potentially a long running operation. We chop it up here with an Eventually continuation and, at each slice, give a chance
                        // for the client to claim the result as obsolete and have the typecheck abort.

                        use _unwind =
                            new CompilationGlobalsScope(errHandler.DiagnosticsLogger, BuildPhase.TypeCheck)

                        let! result =
                            CheckOneInputAndFinish(
                                checkForErrors,
                                tcConfig,
                                tcImports,
                                tcGlobals,
                                None,
                                TcResultsSink.WithSink sink,
                                tcState,
                                parsedMainInput
                            )

                        return result
                    with e ->
                        errorR e

                        let mty =
                            Construct.NewEmptyModuleOrNamespaceType(ModuleOrNamespaceKind.Namespace true)

                        return ((tcState.TcEnvFromSignatures, EmptyTopAttrs, [], [ mty ]), tcState)
                }

            // Play background errors and warnings for this file.
            do
                for err, severity in backgroundDiagnostics do
                    diagnosticSink (err, severity)

            let (tcEnvAtEnd, _, implFiles, ccuSigsForFiles), tcState = resOpt

            let symbolEnv = SymbolEnv(tcGlobals, tcState.Ccu, Some tcState.CcuSig, tcImports)
            let errors = errHandler.CollectedDiagnostics(Some symbolEnv)

            let res =
                TypeCheckInfo(
                    tcConfig,
                    tcGlobals,
                    List.head ccuSigsForFiles,
                    tcState.Ccu,
                    tcImports,
                    tcEnvAtEnd.AccessRights,
                    projectFileName,
                    mainInputFileName,
                    projectOptions,
                    sink.GetResolutions(),
                    sink.GetSymbolUses(),
                    tcEnvAtEnd.NameEnv,
                    loadClosure,
                    List.tryHead implFiles,
                    sink.GetOpenDeclarations()
                )

            return errors, res
        }

[<Sealed>]
type FSharpProjectContext(thisCcu: CcuThunk, assemblies: FSharpAssembly list, ad: AccessorDomain, projectOptions: FSharpProjectOptions) =

    member _.ProjectOptions = projectOptions

    member _.GetReferencedAssemblies() = assemblies

    member _.AccessibilityRights = FSharpAccessibilityRights(thisCcu, ad)

/// A live object of this type keeps the background corresponding background builder (and type providers) alive (through reference-counting).
//
// Note: objects returned by the methods of this type do not require the corresponding background builder to be alive.
[<Sealed>]
type FSharpCheckFileResults
    (
        fileName: string,
        errors: FSharpDiagnostic[],
        scopeOptX: TypeCheckInfo option,
        dependencyFiles: string[],
        builderX: IncrementalBuilder option,
        keepAssemblyContents: bool
    ) =

    // Here 'details' keeps 'builder' alive
    let details =
        match scopeOptX with
        | None -> None
        | Some scopeX -> Some(scopeX, builderX)

    member _.Diagnostics = errors

    member _.HasFullTypeCheckInfo = details.IsSome

    member _.TryGetCurrentTcImports() =
        match details with
        | None -> None
        | Some(scope, _builderOpt) -> Some scope.TcImports

    member info.GetFormatSpecifierLocations() =
        info.GetFormatSpecifierLocationsAndArity() |> Array.map fst

    member _.GetFormatSpecifierLocationsAndArity() =
        match details with
        | None -> [||]
        | Some(scope, _builderOpt) -> scope.GetFormatSpecifierLocationsAndArity()

    member _.PartialAssemblySignature =
        match details with
        | None -> failwith "not available"
        | Some(scope, _builderOpt) -> scope.PartialAssemblySignatureForFile

    member _.ProjectContext =
        match details with
        | None -> failwith "not available"
        | Some(scope, _builderOpt) ->
            FSharpProjectContext(scope.ThisCcu, scope.GetReferencedAssemblies(), scope.AccessRights, scope.ProjectOptions)

    member _.DependencyFiles = dependencyFiles

    member _.GetAllUsesOfAllSymbolsInFile(?cancellationToken: CancellationToken) =
        match details with
        | None -> Seq.empty
        | Some(scope, _builderOpt) ->
            let cenv = scope.SymbolEnv

            seq {
                for symbolUseChunk in scope.ScopeSymbolUses.AllUsesOfSymbols do
                    for symbolUse in symbolUseChunk do
                        cancellationToken |> Option.iter (fun ct -> ct.ThrowIfCancellationRequested())

                        if symbolUse.ItemOccurrence <> ItemOccurrence.RelatedText then
                            let symbol = FSharpSymbol.Create(cenv, symbolUse.ItemWithInst.Item)
                            let inst = symbolUse.ItemWithInst.TyparInstantiation
                            FSharpSymbolUse(symbolUse.DisplayEnv, symbol, inst, symbolUse.ItemOccurrence, symbolUse.Range)
            }

    member _.GetUsesOfSymbolInFile(symbol: FSharpSymbol, ?cancellationToken: CancellationToken) =
        match details with
        | None -> [||]
        | Some(scope, _builderOpt) ->
            [|
                for symbolUse in
                    scope.ScopeSymbolUses.GetUsesOfSymbol(symbol.Item)
                    |> Seq.distinctBy (fun symbolUse -> symbolUse.ItemOccurrence, symbolUse.Range) do
                    cancellationToken |> Option.iter (fun ct -> ct.ThrowIfCancellationRequested())

                    if symbolUse.ItemOccurrence <> ItemOccurrence.RelatedText then
                        let inst = symbolUse.ItemWithInst.TyparInstantiation
                        FSharpSymbolUse(symbolUse.DisplayEnv, symbol, inst, symbolUse.ItemOccurrence, symbolUse.Range)
            |]

    member _.GetVisibleNamespacesAndModulesAtPoint(pos: pos) =
        match details with
        | None -> [||]
        | Some(scope, _builderOpt) -> scope.GetVisibleNamespacesAndModulesAtPosition(pos) |> List.toArray

    member _.IsRelativeNameResolvable(cursorPos: pos, plid: string list, item: Item) =
        match details with
        | None -> true
        | Some(scope, _builderOpt) -> scope.IsRelativeNameResolvable(cursorPos, plid, item)

    member _.IsRelativeNameResolvableFromSymbol(cursorPos: pos, plid: string list, symbol: FSharpSymbol) =
        match details with
        | None -> true
        | Some(scope, _builderOpt) -> scope.IsRelativeNameResolvableFromSymbol(cursorPos, plid, symbol)

    member _.GetDisplayContextForPos(cursorPos: pos) =
        match details with
        | None -> None
        | Some(scope, _builderOpt) ->
            let (nenv, _), _ = scope.GetBestDisplayEnvForPos cursorPos
            Some(FSharpDisplayContext(fun _ -> nenv.DisplayEnv))

    member _.GenerateSignature(?pageWidth: int) =
        match details with
        | None -> None
        | Some(scope, _builderOpt) ->
            scope.ImplementationFile
            |> Option.map (fun implFile ->
                let denv = DisplayEnv.InitialForSigFileGeneration scope.TcGlobals
                let infoReader = InfoReader(scope.TcGlobals, scope.TcImports.GetImportMap())
                let (CheckedImplFile(contents = mexpr)) = implFile

                let ad =
                    match scopeOptX with
                    | Some scope -> scope.AccessRights
                    | _ -> AccessibleFromSomewhere

                let layout =
                    NicePrint.layoutImpliedSignatureOfModuleOrNamespace true denv infoReader ad range0 mexpr

                match pageWidth with
                | None -> layout
                | Some pageWidth -> Display.squashTo pageWidth layout
                |> LayoutRender.showL
                |> SourceText.ofString)

    member internal _.CalculateSignatureHash() =
        let visibility = PublicAndInternal

        match details with
        | None -> failwith "Typechecked details not available for CalculateSignatureHash() operation."
        | Some(scope, _builderOpt) ->
            scope.ImplementationFile
            |> Option.map (fun implFile ->
                Fsharp.Compiler.SignatureHash.calculateSignatureHashOfFiles [ implFile ] scope.TcGlobals visibility)

    member _.ImplementationFile =
        if not keepAssemblyContents then
            invalidOp
                "The 'keepAssemblyContents' flag must be set to true on the FSharpChecker in order to access the checked contents of assemblies"

        scopeOptX
        |> Option.map (fun scope ->
            let cenv =
                SymbolEnv(scope.TcGlobals, scope.ThisCcu, Some scope.CcuSigForFile, scope.TcImports)

            scope.ImplementationFile
            |> Option.map (fun implFile -> FSharpImplementationFileContents(cenv, implFile)))
        |> Option.defaultValue None

    member _.OpenDeclarations =
        scopeOptX
        |> Option.map (fun scope ->
            let cenv = scope.SymbolEnv

            scope.OpenDeclarations
            |> Array.map (fun x ->
                let modules = x.Modules |> List.map (fun x -> FSharpEntity(cenv, x))
                let types = x.Types |> List.map (fun x -> FSharpType(cenv, x))
                FSharpOpenDeclaration(x.Target, x.Range, modules, types, x.AppliedScope, x.IsOwnNamespace)))
        |> Option.defaultValue [||]

    override _.ToString() =
        "FSharpCheckFileResults(" + fileName + ")"

    static member MakeEmpty(fileName: string, creationErrors: FSharpDiagnostic[], keepAssemblyContents) =
        FSharpCheckFileResults(fileName, creationErrors, None, [||], None, keepAssemblyContents)

    static member JoinErrors
        (
            isIncompleteTypeCheckEnvironment,
            creationErrors: FSharpDiagnostic[],
            parseErrors: FSharpDiagnostic[],
            tcErrors: FSharpDiagnostic[]
        ) =
        [|
            yield! creationErrors
            yield! parseErrors
            if isIncompleteTypeCheckEnvironment then
                yield! Seq.truncate maxTypeCheckErrorsOutOfProjectContext tcErrors
            else
                yield! tcErrors
        |]

    static member Make
        (
            mainInputFileName: string,
            projectFileName,
            tcConfig,
            tcGlobals,
            isIncompleteTypeCheckEnvironment: bool,
            builder: IncrementalBuilder option,
            projectOptions,
            dependencyFiles,
            creationErrors: FSharpDiagnostic[],
            parseErrors: FSharpDiagnostic[],
            tcErrors: FSharpDiagnostic[],
            keepAssemblyContents,
            ccuSigForFile,
            thisCcu,
            tcImports,
            tcAccessRights,
            sResolutions,
            sSymbolUses,
            sFallback,
            loadClosure,
            implFileOpt,
            openDeclarations
        ) =

        let tcFileInfo =
            TypeCheckInfo(
                tcConfig,
                tcGlobals,
                ccuSigForFile,
                thisCcu,
                tcImports,
                tcAccessRights,
                projectFileName,
                mainInputFileName,
                projectOptions,
                sResolutions,
                sSymbolUses,
                sFallback,
                loadClosure,
                implFileOpt,
                openDeclarations
            )

        let errors =
            FSharpCheckFileResults.JoinErrors(isIncompleteTypeCheckEnvironment, creationErrors, parseErrors, tcErrors)

        FSharpCheckFileResults(mainInputFileName, errors, Some tcFileInfo, dependencyFiles, builder, keepAssemblyContents)

    static member CheckOneFile
        (
            parseResults: FSharpParseFileResults,
            sourceText: ISourceText,
            mainInputFileName: string,
            projectFileName: string,
            tcConfig: TcConfig,
            tcGlobals: TcGlobals,
            tcImports: TcImports,
            tcState: TcState,
            moduleNamesDict: ModuleNamesDict,
            loadClosure: LoadClosure option,
            backgroundDiagnostics: (PhasedDiagnostic * FSharpDiagnosticSeverity)[],
            isIncompleteTypeCheckEnvironment: bool,
            projectOptions: FSharpProjectOptions,
            builder: IncrementalBuilder option,
            dependencyFiles: string[],
            creationErrors: FSharpDiagnostic[],
            parseErrors: FSharpDiagnostic[],
            keepAssemblyContents: bool,
            suggestNamesForErrors: bool
        ) =
        cancellable {
            let! tcErrors, tcFileInfo =
                ParseAndCheckFile.CheckOneFile(
                    parseResults,
                    sourceText,
                    mainInputFileName,
                    projectOptions,
                    projectFileName,
                    tcConfig,
                    tcGlobals,
                    tcImports,
                    tcState,
                    moduleNamesDict,
                    loadClosure,
                    backgroundDiagnostics,
                    suggestNamesForErrors
                )

            let errors =
                FSharpCheckFileResults.JoinErrors(isIncompleteTypeCheckEnvironment, creationErrors, parseErrors, tcErrors)

            let results =
                FSharpCheckFileResults(mainInputFileName, errors, Some tcFileInfo, dependencyFiles, builder, keepAssemblyContents)

            return results
        }

[<Sealed>]
// 'details' is an option because the creation of the tcGlobals etc. for the project may have failed.
type FSharpCheckProjectResults
    (
        projectFileName: string,
        tcConfigOption: TcConfig option,
        keepAssemblyContents: bool,
        diagnostics: FSharpDiagnostic[],
        details:
            (TcGlobals *
            TcImports *
            CcuThunk *
            ModuleOrNamespaceType *
            Choice<IncrementalBuilder, Async<TcSymbolUses seq>> *
            TopAttribs option *
            (unit -> IRawFSharpAssemblyData option) *
            ILAssemblyRef *
            AccessorDomain *
            CheckedImplFile list option *
            string[] *
            FSharpProjectOptions) option
    ) =

    let getDetails () =
        match details with
        | None ->
            invalidOp (
                "The project has no results due to critical errors in the project options. Check the HasCriticalErrors before accessing the detailed results. Errors: "
                + String.concat "\n" [ for e in diagnostics -> e.Message ]
            )
        | Some d -> d

    let getTcConfig () =
        match tcConfigOption with
        | None ->
            invalidOp (
                "The project has no results due to critical errors in the project options. Check the HasCriticalErrors before accessing the detailed results. Errors: "
                + String.concat "\n" [ for e in diagnostics -> e.Message ]
            )
        | Some d -> d

    member _.Diagnostics = diagnostics

    member _.HasCriticalErrors = details.IsNone

    member _.AssemblySignature =
        let tcGlobals, tcImports, thisCcu, ccuSig, _, topAttribs, _, _, _, _, _, _ =
            getDetails ()

        FSharpAssemblySignature(tcGlobals, thisCcu, ccuSig, tcImports, topAttribs, ccuSig)

    // TODO: Looks like we don't need this
    member _.TypedImplementationFiles =
        if not keepAssemblyContents then
            invalidOp
                "The 'keepAssemblyContents' flag must be set to true on the FSharpChecker in order to access the checked contents of assemblies"

        let tcGlobals, tcImports, thisCcu, _, _, _, _, _, _, tcAssemblyExpr, _, _ =
            getDetails ()

        let mimpls =
            match tcAssemblyExpr with
            | None -> []
            | Some mimpls -> mimpls

        tcGlobals, thisCcu, tcImports, mimpls

    member info.AssemblyContents =
        if not keepAssemblyContents then
            invalidOp
                "The 'keepAssemblyContents' flag must be set to true on the FSharpChecker in order to access the checked contents of assemblies"

        let tcGlobals, tcImports, thisCcu, ccuSig, _, _, _, _, _, tcAssemblyExpr, _, _ =
            getDetails ()

        let mimpls =
            match tcAssemblyExpr with
            | None -> []
            | Some mimpls -> mimpls

        FSharpAssemblyContents(tcGlobals, thisCcu, Some ccuSig, tcImports, mimpls)

    member _.GetOptimizedAssemblyContents() =
        if not keepAssemblyContents then
            invalidOp
                "The 'keepAssemblyContents' flag must be set to true on the FSharpChecker in order to access the checked contents of assemblies"

        let tcGlobals, tcImports, thisCcu, ccuSig, _, _, _, _, _, tcAssemblyExpr, _, _ =
            getDetails ()

        let mimpls =
            match tcAssemblyExpr with
            | None -> []
            | Some mimpls -> mimpls

        let outfile = "" // only used if tcConfig.writeTermsToFiles is true
        let importMap = tcImports.GetImportMap()
        let optEnv0 = GetInitialOptimizationEnv(tcImports, tcGlobals)
        let tcConfig = getTcConfig ()
        let isIncrementalFragment = false
        let tcVal = LightweightTcValForUsingInBuildMethodCall tcGlobals

        let optimizedImpls, _optimizationData, _ =
            ApplyAllOptimizations(tcConfig, tcGlobals, tcVal, outfile, importMap, isIncrementalFragment, optEnv0, thisCcu, mimpls)

        let mimpls =
            match optimizedImpls with
            | CheckedAssemblyAfterOptimization files -> files |> List.map (fun implFile -> implFile.ImplFile)

        FSharpAssemblyContents(tcGlobals, thisCcu, Some ccuSig, tcImports, mimpls)

    // Not, this does not have to be a SyncOp, it can be called from any thread
    // TODO: this should be async
    member _.GetUsesOfSymbol(symbol: FSharpSymbol, ?cancellationToken: CancellationToken) =
        let _, _, _, _, builderOrSymbolUses, _, _, _, _, _, _, _ = getDetails ()

        let results =
            match builderOrSymbolUses with
            | Choice1Of2 builder ->
                builder.SourceFiles
                |> Array.ofList
                |> Array.collect (fun x ->
                    match builder.GetCheckResultsForFileInProjectEvenIfStale x with
                    | Some partialCheckResults ->
                        match partialCheckResults.TryPeekTcInfoWithExtras() with
                        | Some(_, tcInfoExtras) -> tcInfoExtras.TcSymbolUses.GetUsesOfSymbol symbol.Item
                        | _ -> [||]
                    | _ -> [||])
                |> Array.toSeq
            | Choice2Of2 task ->
                Async.RunSynchronously(
                    async {
                        let! tcSymbolUses = task

                        return
                            seq {
                                for symbolUses in tcSymbolUses do
                                    yield! symbolUses.GetUsesOfSymbol symbol.Item
                            }
                    },
                    ?cancellationToken = cancellationToken
                )

        results
        |> Seq.filter (fun symbolUse -> symbolUse.ItemOccurrence <> ItemOccurrence.RelatedText)
        |> Seq.distinctBy (fun symbolUse -> symbolUse.ItemOccurrence, symbolUse.Range)
        |> Seq.map (fun symbolUse ->
            cancellationToken |> Option.iter (fun ct -> ct.ThrowIfCancellationRequested())
            let inst = symbolUse.ItemWithInst.TyparInstantiation
            FSharpSymbolUse(symbolUse.DisplayEnv, symbol, inst, symbolUse.ItemOccurrence, symbolUse.Range))
        |> Seq.toArray

    // Not, this does not have to be a SyncOp, it can be called from any thread
    // TODO: this should be async
    member _.GetAllUsesOfAllSymbols(?cancellationToken: CancellationToken) =
        let tcGlobals, tcImports, thisCcu, ccuSig, builderOrSymbolUses, _, _, _, _, _, _, _ =
            getDetails ()

        let cenv = SymbolEnv(tcGlobals, thisCcu, Some ccuSig, tcImports)

        let tcSymbolUses =
            match builderOrSymbolUses with
            | Choice1Of2 builder ->
                builder.SourceFiles
                |> Array.ofList
                |> Array.map (fun x ->
                    match builder.GetCheckResultsForFileInProjectEvenIfStale x with
                    | Some partialCheckResults ->
                        match partialCheckResults.TryPeekTcInfoWithExtras() with
                        | Some(_, tcInfoExtras) -> tcInfoExtras.TcSymbolUses
                        | _ -> TcSymbolUses.Empty
                    | _ -> TcSymbolUses.Empty)
                |> Array.toSeq
            | Choice2Of2 tcSymbolUses -> Async.RunSynchronously(tcSymbolUses, ?cancellationToken = cancellationToken)

        [|
            for r in tcSymbolUses do
                for symbolUseChunk in r.AllUsesOfSymbols do
                    for symbolUse in symbolUseChunk do
                        cancellationToken |> Option.iter (fun ct -> ct.ThrowIfCancellationRequested())

                        if symbolUse.ItemOccurrence <> ItemOccurrence.RelatedText then
                            let symbol = FSharpSymbol.Create(cenv, symbolUse.ItemWithInst.Item)
                            let inst = symbolUse.ItemWithInst.TyparInstantiation
                            FSharpSymbolUse(symbolUse.DisplayEnv, symbol, inst, symbolUse.ItemOccurrence, symbolUse.Range)
        |]

    member _.ProjectContext =
        let tcGlobals, tcImports, thisCcu, _, _, _, _, _, ad, _, _, projectOptions =
            getDetails ()

        let assemblies =
            tcImports.GetImportedAssemblies()
            |> List.map (fun x -> FSharpAssembly(tcGlobals, tcImports, x.FSharpViewOfMetadata))

        FSharpProjectContext(thisCcu, assemblies, ad, projectOptions)

    member _.DependencyFiles =
        let _tcGlobals, _, _, _, _, _, _, _, _, _, dependencyFiles, _ = getDetails ()
        dependencyFiles

    member _.AssemblyFullName =
        let _tcGlobals, _, _, _, _, _, _, ilAssemRef, _, _, _, _ = getDetails ()
        ilAssemRef.QualifiedName

    override _.ToString() =
        "FSharpCheckProjectResults(" + projectFileName + ")"

type FsiInteractiveChecker(legacyReferenceResolver, tcConfig: TcConfig, tcGlobals: TcGlobals, tcImports: TcImports, tcState) =

    let keepAssemblyContents = false

    member _.ParseAndCheckInteraction(sourceText: ISourceText, ?userOpName: string) =
        cancellable {
            let userOpName = defaultArg userOpName "Unknown"
            let fileName = Path.Combine(tcConfig.implicitIncludeDir, "stdin.fsx")
            let suggestNamesForErrors = true // Will always be true, this is just for readability
            // Note: projectSourceFiles is only used to compute isLastCompiland, and is ignored if Build.IsScript(mainInputFileName) is true (which it is in this case).
            let parsingOptions =
                FSharpParsingOptions.FromTcConfig(tcConfig, [| fileName |], true)

            let! ct = Cancellable.token ()

            let parseErrors, parsedInput, anyErrors =
                ParseAndCheckFile.parseFile (
                    sourceText,
                    fileName,
                    parsingOptions,
                    userOpName,
                    suggestNamesForErrors,
                    tcConfig.flatErrors,
                    tcConfig.captureIdentifiersWhenParsing,
                    ct
                )

            let dependencyFiles = [||] // interactions have no dependencies

            let parseResults =
                FSharpParseFileResults(parseErrors, parsedInput, parseHadErrors = anyErrors, dependencyFiles = dependencyFiles)

            let backgroundDiagnostics = [||]
            let reduceMemoryUsage = ReduceMemoryFlag.Yes
            let assumeDotNetFramework = (tcConfig.primaryAssembly = PrimaryAssembly.Mscorlib)

            let applyCompilerOptions tcConfigB =
                let fsiCompilerOptions = CompilerOptions.GetCoreFsiCompilerOptions tcConfigB
                CompilerOptions.ParseCompilerOptions(ignore, fsiCompilerOptions, [])

            let loadClosure =
                LoadClosure.ComputeClosureOfScriptText(
                    legacyReferenceResolver,
                    defaultFSharpBinariesDir,
                    fileName,
                    sourceText,
                    CodeContext.Editing,
                    tcConfig.useSimpleResolution,
                    tcConfig.useFsiAuxLib,
                    tcConfig.useSdkRefs,
                    tcConfig.sdkDirOverride,
                    LexResourceManager(),
                    applyCompilerOptions,
                    assumeDotNetFramework,
                    tryGetMetadataSnapshot = (fun _ -> None),
                    reduceMemoryUsage = reduceMemoryUsage,
                    dependencyProvider = tcImports.DependencyProvider
                )

            let projectOptions =
                {
                    ProjectFileName = "script.fsproj"
                    ProjectId = None
                    SourceFiles = [||]
                    OtherOptions = [||]
                    ReferencedProjects = [||]
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime.Now
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = None
                }

            let! tcErrors, tcFileInfo =
                ParseAndCheckFile.CheckOneFile(
                    parseResults,
                    sourceText,
                    fileName,
                    projectOptions,
                    projectOptions.ProjectFileName,
                    tcConfig,
                    tcGlobals,
                    tcImports,
                    tcState,
                    Map.empty,
                    Some loadClosure,
                    backgroundDiagnostics,
                    suggestNamesForErrors
                )

            let errors = Array.append parseErrors tcErrors

            let typeCheckResults =
                FSharpCheckFileResults(fileName, errors, Some tcFileInfo, dependencyFiles, None, false)

            let details =
                (tcGlobals,
                 tcImports,
                 tcFileInfo.ThisCcu,
                 tcFileInfo.CcuSigForFile,
                 Choice2Of2(tcFileInfo.ScopeSymbolUses |> Seq.singleton |> async.Return),
                 None,
                 (fun () -> None),
                 mkSimpleAssemblyRef "stdin",
                 tcState.TcEnvFromImpls.AccessRights,
                 None,
                 dependencyFiles,
                 projectOptions)

            let projectResults =
                FSharpCheckProjectResults(fileName, Some tcConfig, keepAssemblyContents, errors, Some details)

            return parseResults, typeCheckResults, projectResults
        }

/// The result of calling TypeCheckResult including the possibility of abort and background compiler not caught up.
[<RequireQualifiedAccess>]
type public FSharpCheckFileAnswer =
    /// Aborted because cancellation caused an abandonment of the operation
    | Aborted

    /// Success
    | Succeeded of FSharpCheckFileResults
