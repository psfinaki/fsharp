// #Regression #Conformance #DeclarationElements #MemberDefinitions #Overloading 
// Verify error message when returning a 'a which is of type unit


[<AbstractClass>]
type 'a ``base`` =
  new() = { }
  abstract M : unit -> 'a

type intBase =
  inherit ``base``<int>
  new() = { inherit ``base``<int>() }
  override this.M () = 99

let aIntBase = new intBase()
let r  = aIntBase.M()

type unitBase =
  inherit ``base``<unit>
  new() = { inherit ``base``<unit>() }
  override this.M () = ()

let aUnitBase = new unitBase()
let r2  = aUnitBase.M()
