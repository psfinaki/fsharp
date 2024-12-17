// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.CodeAnalysis

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// Represents the results of parsing an F# file and a set of analysis operations based on the parse tree alone.
[<Sealed>]
type public FSharpParseFileResults =

    /// The syntax tree resulting from the parse
    member ParseTree: ParsedInput

    /// Name of the file for which this information were created
    member FileName: string

    /// Return the inner-most range associated with a possible breakpoint location
    member ValidateBreakpointLocation: pos: pos -> range option

    /// When these files change then the build is invalid
    member DependencyFiles: string[]

    /// Get the errors and warnings for the parse
    member Diagnostics: FSharpDiagnostic[]

    /// Indicates if any errors occurred during the parse
    member ParseHadErrors: bool

    internal new:
        diagnostics: FSharpDiagnostic[] * input: ParsedInput * parseHadErrors: bool * dependencyFiles: string[] ->
            FSharpParseFileResults
