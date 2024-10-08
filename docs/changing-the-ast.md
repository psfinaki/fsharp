---
title: Changing the AST
category: Compiler Internals
categoryindex: 200
index: 800
---
# Changing the AST

Making changes to the AST is a common task when working on new F# compiler features or when working on developer tooling.  
This document describes the process of making changes to the AST.

The easiest way to modify the AST is to start with the type definitions in `SyntaxTree.fsi` and `SyntaxTree.fs` and then let the compiler guide you to the places where you need to make changes.
Let's look at an example: We want to extend the AST to include the range of the `/` symbol in a `SynRationalConst.Rational`.  

There are two solutions to choose from:
- Add a new field to the `Rational` union case
- Add a dedicated trivia type to the union case which contains the new range and maybe move the existing ranges to the trivia type as well  

The pros of introducing a dedicated trivia type are:
- Having the additional information in a separate structure allows it to grow more easily over time. Adding new information to an existing trivia type won't disrupt most FCS consumers.  
- It is clear that it is information that is not relevant for the compilation.  

The cons are: 
- It can be a bit excessive to introduce for a single field.
- The existing AST node might already contain fields that are historically more suited for trivia, but predate the SyntaxTrivia module.

In this example, we'll go with the first solution and add a new field named `divRange` to the `Rational` union case as it felt a bit excessive to introduce a new trivia type for a single field.  
But these are the type of decisions that need to be made when changing the AST.

```fsharp
type SynRationalConst =

    // ...

    | Rational of
        numerator: int32 *
        numeratorRange: range *
        divRange: range *   // our new field
        denominator: int32 *
        denominatorRange: range *
        range: range
    
    // ...
```	

After modifying `SyntaxTree.fsi` and `SyntaxTree.fs`, the compiler will report errors in `pars.fsy`. If not, the `fsy` file wasn't processed by the compilation. In this case, a rebuild of `FSharp.Compiler.Service.fsproj` should help.  
`pars.fsy` is the parser specification of F#, a list of rules that describe how to parse F# code. Don't be scared by the size of the file or the unfamiliar content.
It's easier than it looks.
The F# compiler uses a parser generator called [fsyacc](https://github.com/fsprojects/FsLexYacc) to generate the parser from the specification in `pars.fsy`.
Let's look at the most relevant syntax parts of a `.fsy` file:

```fsharp
rationalConstant:
  | INT32 INFIX_STAR_DIV_MOD_OP INT32
    { if $2 <> "/" then reportParseErrorAt (rhs parseState 2) (FSComp.SR.parsUnexpectedOperatorForUnitOfMeasure())
      if fst $3 = 0 then reportParseErrorAt (rhs parseState 3) (FSComp.SR.parsIllegalDenominatorForMeasureExponent())
      if (snd $1) || (snd $3) then errorR(Error(FSComp.SR.lexOutsideThirtyTwoBitSigned(), lhs parseState))
      SynRationalConst.Rational(fst $1, rhs parseState 1, fst $3, rhs parseState 3, lhs parseState) }
  | // ...
```

The first line is the name of the rule, `rationalConstant` in this case. It's a so called non-terminal symbol in contrast to a terminal symbol like `INT32` or `INFIX_STAR_DIV_MOD_OP`. The individual cases of the rule are separated by `|`, they are called productions.

By now, you should be able to see the similarities between an fsyacc rule and the pattern matching you know from F#.  
The code between the curly braces is the code that gets executed when the rule is matched and is _real_ F# code. After compilation, it ends up in 
`.\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\pars.fs`, generated by fsyacc.

The first three lines do error checking and report errors if the input is invalid.
Then the code calls the `Rational` constructor of `SynRationalConst` and passes some values to it. Here we need to make changes to adjust the parser to our modified type definition.  
The values or symbols that matched the rule are available as `$1`, `$2`, `$3` etc. in the code. As you can see, `$1` is a tuple, consisting of the parsed number and a boolean indicating whether the number is a valid 32 bit signed integer or not.
The code is executed in the context of the parser, so you can use the `parseState` variable, an instance of `IParseState`, to access the current state of the parser. There are helper functions defined in `ParseHelpers.fs` that make it easier to work with it.  
`rhs parseState 1` returns the range of the first symbol that matched the rule, here `INT32`. So, it returns the range of `23` in `23/42`.  
`rhs` is short for _right hand side_.  
Another helper is `rhs2`. Using it like `rhs2 parseState 2 3` for example, returns the range covering the symbols from the second to the third symbol that matched the rule. Given `23/42`, it would return the range of `/42`.  
 `lhs parseState` returns the range of the whole rule, `23/42` in our example.
 When parser recovery is of concern for a rule, it's preferred to use `rhs2` over `lhs`.
 
 Circling back to our original example of adding a new field to `SynRationalConst`, we need to add a new parameter to the call of the `Rational` constructor. We want to pass the range of the `/` symbol, so we need to add `rhs parseState 2` as the third parameter to the constructor call:  
 
 ```fsharp
SynRationalConst.Rational(fst $1, rhs parseState 1, rhs parseState 2, fst $3, rhs parseState 3, lhs parseState)
```	

That's it. Adjusting the other constructor calls of `Rational` in `pars.fsy` should be enough to have a working parser again which returns the modified AST.  
While fixing the remaining compiler errors outside of `pars.fsy`, it's a good idea to use named access to the fields of the `SynRationalConst.Rational` union case instead of positional access. This way, the compilation won't fail if additional fields are added to the union case in the future.  
After a successful compilation, you can run the parser tests in `SyntaxTreeTests.fs` to verify that everything works as expected.  
It's likely that you'll need to update the baseline files as described in `SyntaxTreeTests.fs`.
