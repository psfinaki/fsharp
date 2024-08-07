// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Solves constraints using a mutable constraint-solver state
module internal FSharp.Compiler.ConstraintSolver

open FSharp.Compiler.AccessibilityLogic
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.Import
open FSharp.Compiler.Infos
open FSharp.Compiler.InfoReader
open FSharp.Compiler.MethodCalls
open FSharp.Compiler.Syntax
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeOps

/// Information about the context of a type equation.
[<RequireQualifiedAccess>]
type ContextInfo =

    /// No context was given.
    | NoContext

    /// The type equation comes from an IF expression.
    | IfExpression of range

    /// The type equation comes from an omitted else branch.
    | OmittedElseBranch of range

    /// The type equation comes from a type check of the result of an else branch.
    | ElseBranchResult of range

    /// The type equation comes from the verification of record fields.
    | RecordFields

    /// The type equation comes from the verification of a tuple in record fields.
    | TupleInRecordFields

    /// The type equation comes from a list or array constructor
    | CollectionElement of bool * range

    /// The type equation comes from a return in a computation expression.
    | ReturnInComputationExpression

    /// The type equation comes from a yield in a computation expression.
    | YieldInComputationExpression

    /// The type equation comes from a runtime type test.
    | RuntimeTypeTest of bool

    /// The type equation comes from an downcast where a upcast could be used.
    | DowncastUsedInsteadOfUpcast of bool

    /// The type equation comes from a return type of a pattern match clause (not the first clause).
    | FollowingPatternMatchClause of range

    /// The type equation comes from a pattern match guard.
    | PatternMatchGuard of range

    /// The type equation comes from a sequence expression.
    | SequenceExpression of TType

/// Captures relevant information for a particular failed overload resolution.
type OverloadInformation =
    { methodSlot: CalledMeth<Expr>
      infoReader: InfoReader
      error: exn }

/// Cases for overload resolution failure that exists in the implementation of the compiler.
type OverloadResolutionFailure =
    | NoOverloadsFound of methodName: string * candidates: OverloadInformation list * cx: TraitConstraintInfo option
    | PossibleCandidates of
        methodName: string *
        candidates: OverloadInformation list *  // methodNames may be different (with operators?), this is refactored from original logic to assemble overload failure message
        cx: TraitConstraintInfo option

/// Represents known information prior to checking an expression or pattern, e.g. it's expected type
type OverallTy =
    /// Each branch of the expression must have the type indicated
    | MustEqual of TType

    /// Each branch of the expression must convert to the type indicated
    | MustConvertTo of isMethodArg: bool * ty: TType

    /// Represents a point where no subsumption/widening is possible
    member Commit: TType

exception ConstraintSolverTupleDiffLengths of
    displayEnv: DisplayEnv *
    contextInfo: ContextInfo *
    TType list *
    TType list *
    range *
    range

exception ConstraintSolverInfiniteTypes of
    displayEnv: DisplayEnv *
    contextInfo: ContextInfo *
    TType *
    TType *
    range *
    range

exception ConstraintSolverTypesNotInEqualityRelation of
    displayEnv: DisplayEnv *
    TType *
    TType *
    range *
    range *
    ContextInfo

exception ConstraintSolverTypesNotInSubsumptionRelation of
    displayEnv: DisplayEnv *
    argTy: TType *
    paramTy: TType *
    callRange: range *
    parameterRange: range

exception ConstraintSolverMissingConstraint of displayEnv: DisplayEnv * Typar * TyparConstraint * range * range

exception ConstraintSolverNullnessWarningEquivWithTypes of
    DisplayEnv *
    TType *
    TType *
    NullnessInfo *
    NullnessInfo *
    range *
    range

exception ConstraintSolverNullnessWarningWithTypes of
    DisplayEnv *
    TType *
    TType *
    NullnessInfo *
    NullnessInfo *
    range *
    range

exception ConstraintSolverNullnessWarningWithType of DisplayEnv * TType * NullnessInfo * range * range

exception ConstraintSolverNullnessWarning of string * range * range

exception ConstraintSolverError of string * range * range

exception ErrorFromApplyingDefault of
    tcGlobals: TcGlobals *
    displayEnv: DisplayEnv *
    Typar *
    TType *
    error: exn *
    range: range

exception ErrorFromAddingTypeEquation of
    tcGlobals: TcGlobals *
    displayEnv: DisplayEnv *
    expectedTy: TType *
    actualTy: TType *
    error: exn *
    range: range

exception ErrorsFromAddingSubsumptionConstraint of
    tcGlobals: TcGlobals *
    displayEnv: DisplayEnv *
    expectedTy: TType *
    actualTy: TType *
    error: exn *
    ctxtInfo: ContextInfo *
    parameterRange: range

exception ErrorFromAddingConstraint of displayEnv: DisplayEnv * error: exn * range: range
exception UnresolvedConversionOperator of displayEnv: DisplayEnv * TType * TType * range

exception UnresolvedOverloading of
    displayEnv: DisplayEnv *
    callerArgs: CallerArgs<Expr> *
    failure: OverloadResolutionFailure *
    range: range

exception NonRigidTypar of displayEnv: DisplayEnv * string option * range * TType * TType * range

exception ArgDoesNotMatchError of
    error: ErrorsFromAddingSubsumptionConstraint *
    calledMeth: CalledMeth<Expr> *
    calledArg: CalledArg *
    callerArg: CallerArg<Expr>

/// A function that denotes captured tcVal, Used in constraint solver and elsewhere to get appropriate expressions for a ValRef.
type TcValF = ValRef -> ValUseFlag -> TType list -> range -> Expr * TType

type ConstraintSolverState =
    {
        g: TcGlobals

        amap: ImportMap

        InfoReader: InfoReader

        /// The function used to freshen values we encounter during trait constraint solving
        TcVal: TcValF

        /// This table stores all unsolved, ungeneralized trait constraints, indexed by free type variable.
        /// That is, there will be one entry in this table for each free type variable in
        /// each outstanding, unsolved, ungeneralized trait constraint. Constraints are removed from the table and resolved
        /// each time a solution to an index variable is found.
        mutable ExtraCxs: Internal.Utilities.Collections.HashMultiMap<Stamp, TraitConstraintInfo * range>

        /// Checks to run after all inference is complete, but before defaults are applied and internal unknowns solved
        PostInferenceChecksPreDefaults: ResizeArray<unit -> unit>

        /// Checks to run after all inference is complete.
        PostInferenceChecksFinal: ResizeArray<unit -> unit>

        WarnWhenUsingWithoutNullOnAWithNullTarget: string option
    }

    static member New: TcGlobals * ImportMap * InfoReader * TcValF -> ConstraintSolverState

    /// Add a post-inference check to run at the end of inference
    member PushPostInferenceCheck: preDefaults: bool * check: (unit -> unit) -> unit

    /// Get the post-inference checks to run near the end of inference, but before defaults are applied
    member GetPostInferenceChecksPreDefaults: unit -> seq<unit -> unit>

    /// Get the post-inference checks to run at the end of inference
    member GetPostInferenceChecksFinal: unit -> seq<unit -> unit>

val BakedInTraitConstraintNames: Set<string>

[<Sealed; NoEquality; NoComparison>]
type Trace

type OptionalTrace =
    | NoTrace
    | WithTrace of Trace

val SimplifyMeasuresInTypeScheme: TcGlobals -> bool -> Typars -> TType -> TyparConstraint list -> Typars

/// The entry point to resolve the overloading for an entire call
val ResolveOverloadingForCall:
    DisplayEnv ->
    ConstraintSolverState ->
    range ->
    methodName: string ->
    callerArgs: CallerArgs<Expr> ->
    AccessorDomain ->
    calledMethGroup: CalledMeth<Expr> list ->
    permitOptArgs: bool ->
    reqdRetTy: OverallTy ->
        CalledMeth<Expr> option * OperationResult<unit>

val UnifyUniqueOverloading:
    DisplayEnv ->
    ConstraintSolverState ->
    range ->
    int * int ->
        string ->
        AccessorDomain ->
        CalledMeth<SynExpr> list ->
        OverallTy ->
            OperationResult<bool>

/// Re-assess the staticness of the type parameters
val UpdateStaticReqOfTypar: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> Typar -> unit

/// Remove the global constraints related to generalized type variables
val EliminateConstraintsForGeneralizedTypars:
    DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> Typars -> unit

val CheckDeclaredTypars: DisplayEnv -> ConstraintSolverState -> range -> Typars -> Typars -> unit

val AddCxTypeEqualsType: ContextInfo -> DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> unit

val AddCxTypeEqualsTypeUndoIfFailed: DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> bool

val AddCxTypeEqualsTypeUndoIfFailedOrWarnings: DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> bool

val AddCxTypeEqualsTypeMatchingOnlyUndoIfFailed: DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> bool

val AddCxTypeMustSubsumeType:
    ContextInfo -> DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> TType -> unit

val AddCxTypeMustSubsumeTypeUndoIfFailed: DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> bool

val AddCxTypeMustSubsumeTypeMatchingOnlyUndoIfFailed:
    DisplayEnv -> ConstraintSolverState -> range -> extraRigidTypars: FreeTypars -> TType -> TType -> bool

val AddCxMethodConstraint: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TraitConstraintInfo -> unit

val AddCxTypeDefnNotSupportsNull: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeUseSupportsNull: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeCanCarryNullnessInfo: DisplayEnv -> ConstraintSolverState -> range -> TType -> Nullness -> unit

val AddCxTypeMustSupportComparison: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeMustSupportEquality: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeMustSupportDefaultCtor: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeIsReferenceType: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeIsValueType: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeIsUnmanaged: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit

val AddCxTypeIsEnum: DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> TType -> unit

val AddCxTypeIsDelegate:
    DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> TType -> TType -> unit

val AddCxTyparDefaultsTo: DisplayEnv -> ConstraintSolverState -> range -> ContextInfo -> Typar -> int -> TType -> unit

val SolveTypeAsError: DisplayEnv -> ConstraintSolverState -> range -> TType -> unit

val ApplyTyparDefaultAtPriority: DisplayEnv -> ConstraintSolverState -> priority: int -> Typar -> unit

/// Generate a witness expression if none is otherwise available, e.g. in legacy non-witness-passing code
val CodegenWitnessExprForTraitConstraint:
    TcValF -> TcGlobals -> ImportMap -> range -> TraitConstraintInfo -> Expr list -> OperationResult<Expr option>

/// Determine if a codegen witness for a trait will require witness args to be available, e.g. in generic code
val CodegenWitnessExprForTraitConstraintWillRequireWitnessArgs:
    TcValF -> TcGlobals -> ImportMap -> range -> TraitConstraintInfo -> OperationResult<bool>

/// Generate the arguments passed when using a generic construct that accepts traits witnesses
val CodegenWitnessesForTyparInst:
    TcValF ->
    TcGlobals ->
    ImportMap ->
    range ->
    Typars ->
    TType list ->
        OperationResult<Choice<TraitConstraintInfo, Expr> list>

/// Generate the lambda argument passed for a use of a generic construct that accepts trait witnesses
val CodegenWitnessArgForTraitConstraint:
    TcValF ->
    TcGlobals ->
    ImportMap ->
    range ->
    TraitConstraintInfo ->
        OperationResult<Choice<TraitConstraintInfo, Expr>>

/// For some code like "let f() = ([] = [])", a free choice is made for a type parameter
/// for an interior type variable.  This chooses a solution for a type parameter subject
/// to its constraints and applies that solution by using a constraint.
val ChooseTyparSolutionAndSolve: ConstraintSolverState -> DisplayEnv -> Typar -> unit

val IsApplicableMethApprox: TcGlobals -> ImportMap -> range -> MethInfo -> TType -> bool

val CanonicalizePartialInferenceProblem: ConstraintSolverState -> DisplayEnv -> range -> Typars -> unit
