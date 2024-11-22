// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.UnicodeLexing

module internal WarnScopes =

    /// To be called during lexing to register the line directives for warn scope processing.
    val RegisterLineDirective: lexbuf: Lexbuf * fileIndex: int * line: int -> unit

    /// To be called during lexing to save #nowarn / #warnon directives.
    val ParseAndRegisterWarnDirective: lexbuf: Lexbuf -> unit

    /// To be called after lexing a file to create warn scopes from the stored line and
    /// warn directives and to add them to the warn scopes from other files in the diagnostics options.
    val MergeInto: FSharpDiagnosticOptions -> range list -> Lexbuf -> unit

    /// Check if the range is inside a "warnon" scope for the given warning number.
    val IsWarnon: FSharpDiagnosticOptions -> warningNumber: int -> mo: range option -> bool

    /// Check if the range is inside a "nowarn" scope for the given warning number.
    val IsNowarn: FSharpDiagnosticOptions -> warningNumber: int -> mo: range option -> bool
