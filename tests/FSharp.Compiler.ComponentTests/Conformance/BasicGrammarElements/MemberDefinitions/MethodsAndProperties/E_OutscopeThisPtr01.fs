// #Regression #Conformance #DeclarationElements #MemberDefinitions #MethodsAndProperties 
#light
// Regression test for FSHARP1.0:579
// Verify the ability to outscope the this pointer: it's an error!

type Foo() =
    member this.Square this = this * this


let t = new Foo()
let this  = 2
if t.Square this <> 4 then failwith "Failed: 1"
