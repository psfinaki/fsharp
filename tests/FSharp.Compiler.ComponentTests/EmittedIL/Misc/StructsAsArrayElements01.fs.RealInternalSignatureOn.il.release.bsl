




.assembly extern runtime { }
.assembly extern FSharp.Core { }
.assembly assembly
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute::.ctor(int32,
                                                                                                      int32,
                                                                                                      int32) = ( 01 00 02 00 00 00 00 00 00 00 00 00 00 00 00 00 ) 

  
  

  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.mresource public FSharpSignatureData.assembly
{
  
  
}
.mresource public FSharpOptimizationData.assembly
{
  
  
}
.module assembly.exe

.imagebase {value}
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0003       
.corflags 0x00000001    





.class public abstract auto ansi sealed assembly
       extends [runtime]System.Object
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
  .class sequential ansi serializable sealed nested public T
         extends [runtime]System.ValueType
         implements class [runtime]System.IEquatable`1<valuetype assembly/T>,
                    [runtime]System.Collections.IStructuralEquatable,
                    class [runtime]System.IComparable`1<valuetype assembly/T>,
                    [runtime]System.IComparable,
                    [runtime]System.Collections.IStructuralComparable
  {
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.StructAttribute::.ctor() = ( 01 00 00 00 ) 
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 03 00 00 00 00 00 ) 
    .field public int32 i
    .method public hidebysig virtual final instance int32  CompareTo(valuetype assembly/T obj) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  5
      .locals init (valuetype assembly/T& V_0,
               class [runtime]System.Collections.IComparer V_1,
               int32 V_2,
               int32 V_3)
      IL_0000:  ldarga.s   obj
      IL_0002:  stloc.0
      IL_0003:  call       class [runtime]System.Collections.IComparer [FSharp.Core]Microsoft.FSharp.Core.LanguagePrimitives::get_GenericComparer()
      IL_0008:  stloc.1
      IL_0009:  ldarg.0
      IL_000a:  ldfld      int32 assembly/T::i
      IL_000f:  stloc.2
      IL_0010:  ldloc.0
      IL_0011:  ldfld      int32 assembly/T::i
      IL_0016:  stloc.3
      IL_0017:  ldloc.2
      IL_0018:  ldloc.3
      IL_0019:  cgt
      IL_001b:  ldloc.2
      IL_001c:  ldloc.3
      IL_001d:  clt
      IL_001f:  sub
      IL_0020:  ret
    } 

    .method public hidebysig virtual final instance int32  CompareTo(object obj) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  8
      IL_0000:  ldarg.0
      IL_0001:  ldarg.1
      IL_0002:  unbox.any  assembly/T
      IL_0007:  call       instance int32 assembly/T::CompareTo(valuetype assembly/T)
      IL_000c:  ret
    } 

    .method public hidebysig virtual final 
            instance int32  CompareTo(object obj,
                                      class [runtime]System.Collections.IComparer comp) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  5
      .locals init (valuetype assembly/T V_0,
               valuetype assembly/T& V_1,
               class [runtime]System.Collections.IComparer V_2,
               int32 V_3,
               int32 V_4)
      IL_0000:  ldarg.1
      IL_0001:  unbox.any  assembly/T
      IL_0006:  stloc.0
      IL_0007:  ldloca.s   V_0
      IL_0009:  stloc.1
      IL_000a:  ldarg.2
      IL_000b:  stloc.2
      IL_000c:  ldarg.0
      IL_000d:  ldfld      int32 assembly/T::i
      IL_0012:  stloc.3
      IL_0013:  ldloc.1
      IL_0014:  ldfld      int32 assembly/T::i
      IL_0019:  stloc.s    V_4
      IL_001b:  ldloc.3
      IL_001c:  ldloc.s    V_4
      IL_001e:  cgt
      IL_0020:  ldloc.3
      IL_0021:  ldloc.s    V_4
      IL_0023:  clt
      IL_0025:  sub
      IL_0026:  ret
    } 

    .method public hidebysig virtual final instance int32  GetHashCode(class [runtime]System.Collections.IEqualityComparer comp) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  7
      .locals init (int32 V_0,
               class [runtime]System.Collections.IEqualityComparer V_1)
      IL_0000:  ldc.i4.0
      IL_0001:  stloc.0
      IL_0002:  ldc.i4     0x9e3779b9
      IL_0007:  ldarg.1
      IL_0008:  stloc.1
      IL_0009:  ldarg.0
      IL_000a:  ldfld      int32 assembly/T::i
      IL_000f:  ldloc.0
      IL_0010:  ldc.i4.6
      IL_0011:  shl
      IL_0012:  ldloc.0
      IL_0013:  ldc.i4.2
      IL_0014:  shr
      IL_0015:  add
      IL_0016:  add
      IL_0017:  add
      IL_0018:  stloc.0
      IL_0019:  ldloc.0
      IL_001a:  ret
    } 

    .method public hidebysig virtual final instance int32  GetHashCode() cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  8
      IL_0000:  ldarg.0
      IL_0001:  call       class [runtime]System.Collections.IEqualityComparer [FSharp.Core]Microsoft.FSharp.Core.LanguagePrimitives::get_GenericEqualityComparer()
      IL_0006:  call       instance int32 assembly/T::GetHashCode(class [runtime]System.Collections.IEqualityComparer)
      IL_000b:  ret
    } 

    .method public hidebysig virtual final 
            instance bool  Equals(valuetype assembly/T obj,
                                  class [runtime]System.Collections.IEqualityComparer comp) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  4
      .locals init (valuetype assembly/T& V_0,
               class [runtime]System.Collections.IEqualityComparer V_1)
      IL_0000:  ldarga.s   obj
      IL_0002:  stloc.0
      IL_0003:  ldarg.2
      IL_0004:  stloc.1
      IL_0005:  ldarg.0
      IL_0006:  ldfld      int32 assembly/T::i
      IL_000b:  ldloc.0
      IL_000c:  ldfld      int32 assembly/T::i
      IL_0011:  ceq
      IL_0013:  ret
    } 

    .method public hidebysig virtual final 
            instance bool  Equals(object obj,
                                  class [runtime]System.Collections.IEqualityComparer comp) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  5
      .locals init (object V_0,
               valuetype assembly/T V_1)
      IL_0000:  ldarg.1
      IL_0001:  stloc.0
      IL_0002:  ldloc.0
      IL_0003:  isinst     assembly/T
      IL_0008:  ldnull
      IL_0009:  cgt.un
      IL_000b:  brfalse.s  IL_001d

      IL_000d:  ldarg.1
      IL_000e:  unbox.any  assembly/T
      IL_0013:  stloc.1
      IL_0014:  ldarg.0
      IL_0015:  ldloc.1
      IL_0016:  ldarg.2
      IL_0017:  call       instance bool assembly/T::Equals(valuetype assembly/T,
                                                                            class [runtime]System.Collections.IEqualityComparer)
      IL_001c:  ret

      IL_001d:  ldc.i4.0
      IL_001e:  ret
    } 

    .method public hidebysig instance void Set(int32 i) cil managed
    {
      
      .maxstack  8
      IL_0000:  ldarg.0
      IL_0001:  ldarg.1
      IL_0002:  stfld      int32 assembly/T::i
      IL_0007:  ret
    } 

    .method public hidebysig virtual final instance bool  Equals(valuetype assembly/T obj) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  4
      .locals init (valuetype assembly/T& V_0)
      IL_0000:  ldarga.s   obj
      IL_0002:  stloc.0
      IL_0003:  ldarg.0
      IL_0004:  ldfld      int32 assembly/T::i
      IL_0009:  ldloc.0
      IL_000a:  ldfld      int32 assembly/T::i
      IL_000f:  ceq
      IL_0011:  ret
    } 

    .method public hidebysig virtual final instance bool  Equals(object obj) cil managed
    {
      .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      
      .maxstack  4
      .locals init (object V_0,
               valuetype assembly/T V_1)
      IL_0000:  ldarg.1
      IL_0001:  stloc.0
      IL_0002:  ldloc.0
      IL_0003:  isinst     assembly/T
      IL_0008:  ldnull
      IL_0009:  cgt.un
      IL_000b:  brfalse.s  IL_001c

      IL_000d:  ldarg.1
      IL_000e:  unbox.any  assembly/T
      IL_0013:  stloc.1
      IL_0014:  ldarg.0
      IL_0015:  ldloc.1
      IL_0016:  call       instance bool assembly/T::Equals(valuetype assembly/T)
      IL_001b:  ret

      IL_001c:  ldc.i4.0
      IL_001d:  ret
    } 

  } 

  .field static assembly valuetype assembly/T[] a@11
  .custom instance void [runtime]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [runtime]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .method public specialname static valuetype assembly/T[] get_a() cil managed
  {
    
    .maxstack  8
    IL_0000:  ldsfld     valuetype assembly/T[] assembly::a@11
    IL_0005:  ret
  } 

  .method private specialname rtspecialname static void  .cctor() cil managed
  {
    
    .maxstack  8
    IL_0000:  ldc.i4.0
    IL_0001:  stsfld     int32 '<StartupCode$assembly>'.$assembly::init@
    IL_0006:  ldsfld     int32 '<StartupCode$assembly>'.$assembly::init@
    IL_000b:  pop
    IL_000c:  ret
  } 

  .method assembly specialname static void staticInitialization@() cil managed
  {
    
    .maxstack  4
    .locals init (valuetype assembly/T V_0)
    IL_0000:  ldc.i4.s   10
    IL_0002:  ldloc.0
    IL_0003:  call       !!0[] [FSharp.Core]Microsoft.FSharp.Collections.ArrayModule::Create<valuetype assembly/T>(int32,
                                                                                                                                   !!0)
    IL_0008:  stsfld     valuetype assembly/T[] assembly::a@11
    IL_000d:  call       valuetype assembly/T[] assembly::get_a()
    IL_0012:  ldc.i4.0
    IL_0013:  ldelema    assembly/T
    IL_0018:  ldc.i4.s   27
    IL_001a:  call       instance void assembly/T::Set(int32)
    IL_001f:  nop
    IL_0020:  ret
  } 

  .property valuetype assembly/T[]
          a()
  {
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 09 00 00 00 00 00 ) 
    .get valuetype assembly/T[] assembly::get_a()
  } 
} 

.class private abstract auto ansi sealed '<StartupCode$assembly>'.$assembly
       extends [runtime]System.Object
{
  .field static assembly int32 init@
  .custom instance void [runtime]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [runtime]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .custom instance void [runtime]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
  .custom instance void [runtime]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
  .method public static void  main@() cil managed
  {
    .entrypoint
    
    .maxstack  8
    IL_0000:  call       void assembly::staticInitialization@()
    IL_0005:  ret
  } 

} 






